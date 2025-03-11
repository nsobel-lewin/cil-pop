from warnings import warn
import numpy as np
import pandas as pd
import os
import provider
from IPython.core.debugger import set_trace

def read_hierarchicalpopprovider(ssp, projection_path="/project/cil/gcp/population/processed/", downscaling_product="landscan", downscaling_year=2022, frequency=5, startyear=2025, stopyear=2100):
    """
    Read files on disk to create a PopulationProvider instance

    Parameters
    ----------
    ssp : str
    projection_path : str, optional
        CSV file containing downscaled population projections at the IR level. 
        Defaults to "/project/cil/gcp/population/processed/"
    downscaling_product : str, optional
        Product used for downscaling SSP projections. 
        Must be a subfolder of `projection_path`/
        Defaults to landscan. 
    downscaling_year : int, optional
        Year of product used for downscaling SSP projections. 
        Must be a subfolder of `projection_path`/`downscaling_product`/
        Defaults to 2022.
    frequency : int, optional
        Frequency of population data. 
        Frequencies below 5 years use linearly interpolated population. 
        Defaults to 5 years. 
    startyear : int, optional
        First year in time series.
        Defaults to 2010.
    stopyear : int, optional
        Last year in time series. 
        Defaults to 2100.

    Returns
    -------
    HierarchicalPopulationProvider

    Examples
    --------
    >>> provider = read_hierarchicalpopprovider(
    ...     'SSP3',
    ...     projection_path="/project/cil/gcp/population/processed/", 
    ...     downscaling_product="landscan", 
    ...     downscaling_year=2022, 
    ...     frequency=5, 
    ...     startyear=2010, 
    ...     stopyear=2100
    ... )
    >>> population = provider.get_timeseries('ZWE.2.2')  # Get the series for any hierid or ISO

    See Also
    --------
    HierarchicalPopulationProvider : Provider of population timeseries
    """
    
    ir_pop_path = os.path.join(projection_path, downscaling_product, str(downscaling_year), "scaled/ir_pop.csv")
    ssp_path = os.path.join(projection_path, "SSP/cleaned_SSP_data.csv")

    df_ir_pops = pd.read_csv(ir_pop_path)
    df_ssp = pd.read_csv(ssp_path)
    
    return HierarchicalPopulationProvider(
        ssp=ssp,
        df_ir_pops=df_ir_pops,
        df_ssp=df_ssp,
        startyear=startyear,
        stopyear=stopyear
    )

class HierarchicalPopulationProvider(provider.BySpaceProvider):
    """
    Provider of population timeseries, selecting "best" available source

    This is most commonly instantiated through ''read_hierarchicalpopprovider()''.

    Parameters
    ----------
    ssp : str
        Target SSP scenario, e.g. "SSP3".
    df_ir_pops : pd.Dataframe
        IR-level population. Must have columns ("gadmid", "hierid", "ISO", "year", "pop")
    df_ssp : pd.Dataframe
        ISO-level SSP projections. Must have columns ("ISO", "year", "ssp", "population")
    startyear : int, optional
        Year to draw baseline value from. Must be greater than the minima of the "year" columns in 
        df_ir_pops and df_ssp
    stopyear : int, optional
        Must be less than the maximal value in df_ssp's "year" column

    See Also
    --------
    read_hierarchicalpopprovider : Read files on disk to create a HierarchicalPopulationProvider instance.
    """
    
    def __init__(self, ssp, df_ir_pops, df_ssp, startyear=2020, stopyear=2100, iam = np.nan):
        """ssp should be as described in the files (e.g., ssp = 'SSP3')"""
        super().__init__(iam, ssp, startyear)
        
        self.stopyear = stopyear
        self.ir_pop_year = df_ir_pops.loc[1, "year"]
        self.df_ir_pops_this = df_ir_pops[['hierid', 'pop']].rename(columns = {'hierid' : 'geo_key'})
        
        # Compute growth factor
        df_ssp = df_ssp.loc[df_ssp['ssp'] == ssp]
        base_pop = df_ssp.loc[df_ssp['year'] == startyear].set_index('ISO')['population'].to_dict()
        df_ssp = df_ssp.copy() 
        df_ssp.loc[:, 'population_growth_factor'] = df_ssp.apply(lambda row: row['population'] / base_pop.get(row['ISO'], row['population']), axis=1)
        df_growth = df_ssp.loc[:, ['year', 'ISO', 'population_growth_factor']].rename(columns = {'ISO' : 'geo_key'})

        # Store growth rates, if missing default to constant
        self.df_growth_this = df_growth
        self.df_growth_default = pd.DataFrame({'year': range(startyear, stopyear), 'population_growth_factor': 1})

    def _get_best_key_available(self, geo_key, df_this, df_default):
        """Attempt to get value, if missing use default"""
        df = df_this.loc[df_this.geo_key == geo_key]
        if df.shape[0] > 0:
            return df
                               
        return df_default

    def get_timeseries(self, hierid):
        """Return an np.array of population for the given region."""
        
        iso = hierid[:3]
        
        # Select baseline pop
        df_pop_baseline = self._get_best_key_available(
            hierid,
            self.df_ir_pops_this,
            pd.DataFrame({'geo_key': [hierid], 'pop': [np.nan]})
        ).reset_index()

        pop_baseline = df_pop_baseline.loc[0, 'pop']
         
        # Select growth series from ISO
        df_growth = self._get_best_key_available(
            iso,
            self.df_growth_this,
            self.df_growth_default
        )
         
        df_ts = df_growth.loc[:, ['year']].copy()
        df_ts.loc[:,'pop'] = pop_baseline * df_growth.loc[:, 'population_growth_factor']
        return df_ts

if __name__ == '__main__':
    # Test the provider
    import time

    time0 = time.time()
    provider = read_hierarchicalpopprovider('SSP3')
    df_baseline = pd.read_csv("/project/cil/gcp/population/processed/landscan/2022/scaled/ir_pop.csv", usecols = ['hierid'])
    time1 = time.time()
    print("Load time: %s seconds" % (time1 - time0))
    for ii in df_baseline.hierid:
        xx = provider.get_timeseries(ii)
    time2 = time.time()
    print("First pass: %s seconds" % (time2 - time1))
