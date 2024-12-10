from functools import lru_cache
from warnings import warn
import numpy as np
import pandas as pd
import metacsv
from impactlab_tools.utils import files
from . import provider


def read_hierarchicalgdppcprovider(iam, ssp, growth_path_or_buffer, baseline_path_or_buffer, nightlights_path_or_buffer,
                                   startyear=2010, stopyear=2100, use_sharedpath=False):
    """
    Read files on disk to create a HierarchicalGDPpcProvider instance

    Parameters
    ----------
    iam : str
    ssp : str
    growth_path_or_buffer
        CSV file containing GDPpc growth projection at 5-year intervals. Must have columns giving "year",
        IAM model ("model"), SSP scenario ("scenario"), the ISO entity ("iso"),
        and the actual GDPpc growth values value ("growth").
    baseline_path_or_buffer
        CSV file containing GDPpc baseline values. Must have columns giving "year",
        IAM model ("model"), SSP scenario ("scenario"), the ISO entity ("iso"),
        and corresponding the actual GDPpc values ("value").
    nightlights_path_or_buffer
        CSV file of nightlight-based ratios for regions.
    startyear : int, optional
        Year to draw baseline value from. Must be in 'baseline_path_or_buffer' file's "year" column.
    stopyear : int, optional
        Must be within 5 years of the largest "year"  in 'growth_path_or_buffer'.
    use_sharedpath : bool, optional
        Interpret paths without leading "/" as "shareddir" paths?

    Returns
    -------
    HierarchicalGDPpcProvider

    Examples
    --------
    >>> provider = read_hierarchicalgdppcprovider(
    ...     'low', 'SSP3',
    ...     growth_path_or_buffer='inputdata/gdppc/growth.csv',
    ...     baseline_path_or_buffer='inputdata/gdppc/baseline.csv',
    ...     nightlights_path_or_buffer='inputdata/nightlights_normalized.csv'
    ... )
    >>> gdppcs = provider.get_timeseries('ZWE.2.2')  # Get the series for any hierid or ISO

    See Also
    --------
    HierarchicalGDPpcProvider : Provider of GDP per capita (GDPpc) timeseries, selecting "best" data source
    """
    if use_sharedpath:
        baseline_path_or_buffer = files.sharedpath(baseline_path_or_buffer)
        growth_path_or_buffer = files.sharedpath(growth_path_or_buffer)
        nightlights_path_or_buffer = files.sharedpath(nightlights_path_or_buffer)

    df = metacsv.read_csv(baseline_path_or_buffer)
    df_growth = metacsv.read_csv(growth_path_or_buffer)
    df_nightlights = metacsv.read_csv(nightlights_path_or_buffer)

    return HierarchicalGDPpcProvider(
        iam=iam,
        ssp=ssp,
        df_baseline=df,
        df_growth=df_growth,
        df_nightlights=df_nightlights,
        startyear=startyear,
        stopyear=stopyear
    )


def GDPpcProvider(iam, ssp, baseline_year=2010, growth_filepath='social/baselines/gdppc-growth.csv',
                  baseline_filepath='social/baselines/gdppc-merged-nohier.csv',
                  nightlights_filepath='social/baselines/nightlight_weight_normalized.csv', stopyear=2100):
    """Get HierarchicalGDPpcProvider through the legacy GDPpcProvider interface

    This interface is deprecated, please use read_hierarchicalgdppcprovider() or
    instantiate HierarchicalGDPpcProvider, directly.

    Parameters
    ----------
    iam : str
    ssp : str
    baseline_year : int, optional
    growth_filepath : str, optional
    baseline_filepath : str, optional
    nightlights_filepath : str, optional
    stopyear : int, optional

    Returns
    -------
    out : impactcommmon.exogenous_economy.HierarchicalGDPpcProvider

    Examples
    --------
    >>> provider = GDPpcProvider('low', 'SSP3')  # Requires setting IMPERICS_SHAREDDIR.
    >>> gdppcs = provider.get_timeseries('ZWE.2.2')  # Get the series for any hierid or ISO

    See Also
    --------
    read_hierarchicalgdppcprovider : Read files on disk to create a HierarchicalGDPpcProvider instance
    HierarchicalGDPpcProvider : Provider of GDP per capita (GDPpc) timeseries, selecting "best" data source
    """
    warn(
        "GDPpcProvider is deprecated, please use read_hierarchicalgdppcprovider or HierarchicalGDPpcProvider, directly",
        DeprecationWarning
    )
    out = read_hierarchicalgdppcprovider(
        iam=iam,
        ssp=ssp,
        growth_path_or_buffer=growth_filepath,
        baseline_path_or_buffer=baseline_filepath,
        nightlights_path_or_buffer=nightlights_filepath,
        use_sharedpath=True,
        startyear=baseline_year,
        stopyear=stopyear
    )
    return out


class HierarchicalGDPpcProvider(provider.BySpaceProvider):
    """
    Provider of GDP per capita (GDPpc) timeseries, selecting "best" available source

    The provider selects the "best" data by using the highest priority data
    available: first data from the IAM, then from any IAM, then global.

    This is most commonly instantiated through ''read_hierarchicalgdppcprovider()''.

    Parameters
    ----------
    iam : str
        Target IAM model, e.g. "high" or "low".
    ssp : str
        Target SSP scenario, e.g. "SSP3".
    df_baseline : pd.Dataframe
        Annual GDPpc baseline observations. Must have columns giving "year",
        IAM model ("model"), SSP scenario ("scenario"), the ISO entity ("iso"),
        and corresponding the actual GDPpc value ("value").
    df_growth : pd.Dataframe
        Projected GDPpc differences at 5-year intervals. Must have the same
        columns as `df_baseline` ("year", "model", "scenario", "iso") but
        with a "growth" column of projected changes in GDPpc.
    df_nightlights : pd.Dataframe
    startyear : int, optional
        Year to draw baseline value from. Must be in 'df_baseline's "year" column.
    stopyear : int, optional
        Must be within 5 years of the largest "year"  in 'df_growth'.

    See Also
    --------
    read_hierarchicalgdppcprovider : Read files on disk to create a HierarchicalGDPpcProvider instance.
    """
    
    def __init__(self, iam, ssp, df_baseline, df_growth, df_nightlights, startyear=2010, stopyear=2100):
        """iam and ssp should be as described in the files (e.g., iam = 'low', ssp = 'SSP3')"""
        super().__init__(iam, ssp, startyear)
        self.stopyear = stopyear

        self.df_baseline_this = df_baseline.loc[(df_baseline.model == iam) & (df_baseline.scenario == ssp) & (df_baseline.year == startyear)]
        self.df_baseline_anyiam = df_baseline.loc[(df_baseline.scenario == ssp) & (df_baseline.year == startyear)].groupby('iso').median()
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
    time1 = time.time()
    print("Load time: %s seconds" % (time1 - time0))

    for ii in np.where(df_baseline.is_terminal)[0]:
        xx = provider.get_timeseries(df_baseline.iloc[ii, 0])
    time2 = time.time()
    print("First pass: %s seconds" % (time2 - time1))

    for ii in np.where(df_baseline.is_terminal)[0]:
        xx = provider.get_timeseries(df_baseline.iloc[ii, 0])
    time3 = time.time()
    print("Second pass: %s seconds" % (time3 - time2))
