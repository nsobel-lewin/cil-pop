class BySpaceProvider(object):
    def __init__(self, iam, ssp, startyear):
        self.iam = iam
        self.ssp = ssp
        self.startyear = startyear
        
    def get_startyear(self):
        return self.startyear

    def get_timeseries(self, region):
        raise NotImplementedError()

class BySpaceTimeProvider(object):
    def __init__(self, iam, ssp):
        self.iam = iam
        self.ssp = ssp
        self.reset()

    def reset(self):
        raise NotImplementedError()
        
    def get_value(self, region, time):
        raise NotImplementedError()

class BySpaceTimeFromSpaceProvider(BySpaceTimeProvider):
    def __init__(self, provider):
        self.provider = provider
        super(BySpaceTimeFromSpaceProvider, self).__init__(provider.iam, provider.ssp)

    def reset(self):
        self.cache = {}
        
    def get_value(self, region, time):
        if region not in self.cache:
            self.cache[region] = self.provider.get_timeseries(region)

        startyear = self.provider.get_startyear()
        if time < startyear:
            return self.cache[region][0]
            
        return self.cache[region][time - startyear]

    def get_startyear(self):
        return self.provider.get_startyear()

    def get_timeseries(self, region):
        return self.provider.get_timeseries(region)
