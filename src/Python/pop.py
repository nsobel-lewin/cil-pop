from warnings import warn
import numpy as np
import pandas as pd
import os
from impactlab_tools.utils import files
from . import provider


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

    df_ir_pop = pd.read_csv(ir_pops_path)
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
    
    def __init__(self, ssp, df_ir_pops, df_ssp, startyear=2020, stopyear=2100):
        """ssp should be as described in the files (e.g., ssp = 'SSP3')"""
        super().__init__(ssp, startyear)
        self.stopyear = stopyear
        self.ir_pop_year = df_ir_pops.loc[1, "year"]

        self.df_ir_pops = df_ir_pops 
        self.baseline_global = df_baseline.loc[(df_baseline.scenario == ssp) & (df_baseline.year == startyear)].select_dtypes("number").median()

        # Load the growth rates, and split by priority of data
        df_growth['yearindex'] = np.int_((df_growth.year - startyear) / 5)
        self.df_growth_this = df_growth.loc[(df_growth.model == iam) & (df_growth.scenario == ssp)]
        self.df_growth_anyiam = df_growth.loc[(df_growth.scenario == ssp)].groupby(['iso', 'year']).median()
        self.growth_global = df_growth.loc[(df_growth.scenario == ssp) & (df_growth.model == iam)].groupby(['year']).median()

        self.df_nightlights = df_nightlights

    def _get_best_iso_available(self, iso, df_this, df_anyiam, df_global):
        """Get the highest priority data available: first data from the IAM, then from any IAM, then global."""
        df = df_this.loc[df_this.iso == iso]
        if df.shape[0] > 0:
            return df

        if iso in df_anyiam.index:
            df = df_anyiam.loc[iso]
            if df.shape[0] > 0:
                return df

        return df_global

    def get_timeseries(self, hierid):
        """Return an np.array of GDPpc for the given region."""
        
        iso_gdppcs = self.get_iso_timeseries(hierid[:3])
        ratio = self.df_nightlights.loc[self.df_nightlights.hierid == hierid].gdppc_ratio
        if len(ratio) == 0:
            return iso_gdppcs # Assume all combined
        if np.isnan(ratio.values[0]) or ratio.values[0] == 0:
            return 0.8 * iso_gdppcs
        
        return iso_gdppcs * ratio.values[0]

    @lru_cache(maxsize=None)
    def get_iso_timeseries(self, iso):
        """Return an np.array of GDPpc for the given ISO country."""
        # Select baseline GDPpc
        df_baseline = self._get_best_iso_available(
            iso,
            self.df_baseline_this,
            self.df_baseline_anyiam,
            self.baseline_global
        )
        baseline = df_baseline.value
        if isinstance(baseline, pd.Series):
            baseline = baseline.values[0]

        # Select growth series
        df_growth = self._get_best_iso_available(
            iso,
            self.df_growth_this,
            self.df_growth_anyiam,
            self.growth_global
        )

        # Calculate GDPpc as they grow in time
        gdppcs = [baseline]
        for year in range(self.startyear + 1, self.stopyear + 1):
            yearindex = int((year - 1 - self.startyear) / 5) # Last year's growth
            growthrate = df_growth.loc[df_growth.yearindex == yearindex].growth.values
            new_gdppc = gdppcs[-1] * growthrate
            gdppcs.append(new_gdppc.item())

        return np.array(gdppcs)


if __name__ == '__main__':
    # Test the provider
    import time

    time0 = time.time()
    provider = GDPpcProvider('low', 'SSP3')
    df_baseline = metacsv.read_csv(files.sharedpath("regions/hierarchy_metacsv.csv"))
    time1 = time.time()s
    print("Load time: %s seconds" % (time1 - time0))

    for ii in np.where(df_baseline.is_terminal)[0]:
        xx = provider.get_timeseries(df_baseline.iloc[ii, 0])
    time2 = time.time()
    print("First pass: %s seconds" % (time2 - time1))

    for ii in np.where(df_baseline.is_terminal)[0]:
        xx = provider.get_timeseries(df_baseline.iloc[ii, 0])
    time3 = time.time()
    print("Second pass: %s seconds" % (time3 - time2))
