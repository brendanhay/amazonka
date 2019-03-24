{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Product where

import Network.AWS.CostExplorer.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The amount of instance usage that a reservation covered.
--
--
--
-- /See:/ 'coverage' smart constructor.
data Coverage = Coverage'
  { _cCoverageNormalizedUnits :: !(Maybe CoverageNormalizedUnits)
  , _cCoverageHours           :: !(Maybe CoverageHours)
  , _cCoverageCost            :: !(Maybe CoverageCost)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Coverage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCoverageNormalizedUnits' - The amount of instance usage that the reservation covered, in normalized units.
--
-- * 'cCoverageHours' - The amount of instance usage that the reservation covered, in hours.
--
-- * 'cCoverageCost' - The amount of cost that the reservation covered.
coverage
    :: Coverage
coverage =
  Coverage'
    { _cCoverageNormalizedUnits = Nothing
    , _cCoverageHours = Nothing
    , _cCoverageCost = Nothing
    }


-- | The amount of instance usage that the reservation covered, in normalized units.
cCoverageNormalizedUnits :: Lens' Coverage (Maybe CoverageNormalizedUnits)
cCoverageNormalizedUnits = lens _cCoverageNormalizedUnits (\ s a -> s{_cCoverageNormalizedUnits = a})

-- | The amount of instance usage that the reservation covered, in hours.
cCoverageHours :: Lens' Coverage (Maybe CoverageHours)
cCoverageHours = lens _cCoverageHours (\ s a -> s{_cCoverageHours = a})

-- | The amount of cost that the reservation covered.
cCoverageCost :: Lens' Coverage (Maybe CoverageCost)
cCoverageCost = lens _cCoverageCost (\ s a -> s{_cCoverageCost = a})

instance FromJSON Coverage where
        parseJSON
          = withObject "Coverage"
              (\ x ->
                 Coverage' <$>
                   (x .:? "CoverageNormalizedUnits") <*>
                     (x .:? "CoverageHours")
                     <*> (x .:? "CoverageCost"))

instance Hashable Coverage where

instance NFData Coverage where

-- | Reservation coverage for a specified period, in hours.
--
--
--
-- /See:/ 'coverageByTime' smart constructor.
data CoverageByTime = CoverageByTime'
  { _cbtGroups     :: !(Maybe [ReservationCoverageGroup])
  , _cbtTimePeriod :: !(Maybe DateInterval)
  , _cbtTotal      :: !(Maybe Coverage)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CoverageByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbtGroups' - The groups of instances that the reservation covered.
--
-- * 'cbtTimePeriod' - The period that this coverage was used over.
--
-- * 'cbtTotal' - The total reservation coverage, in hours.
coverageByTime
    :: CoverageByTime
coverageByTime =
  CoverageByTime'
    {_cbtGroups = Nothing, _cbtTimePeriod = Nothing, _cbtTotal = Nothing}


-- | The groups of instances that the reservation covered.
cbtGroups :: Lens' CoverageByTime [ReservationCoverageGroup]
cbtGroups = lens _cbtGroups (\ s a -> s{_cbtGroups = a}) . _Default . _Coerce

-- | The period that this coverage was used over.
cbtTimePeriod :: Lens' CoverageByTime (Maybe DateInterval)
cbtTimePeriod = lens _cbtTimePeriod (\ s a -> s{_cbtTimePeriod = a})

-- | The total reservation coverage, in hours.
cbtTotal :: Lens' CoverageByTime (Maybe Coverage)
cbtTotal = lens _cbtTotal (\ s a -> s{_cbtTotal = a})

instance FromJSON CoverageByTime where
        parseJSON
          = withObject "CoverageByTime"
              (\ x ->
                 CoverageByTime' <$>
                   (x .:? "Groups" .!= mempty) <*> (x .:? "TimePeriod")
                     <*> (x .:? "Total"))

instance Hashable CoverageByTime where

instance NFData CoverageByTime where

-- | How much it cost to run an instance.
--
--
--
-- /See:/ 'coverageCost' smart constructor.
newtype CoverageCost = CoverageCost'
  { _ccOnDemandCost :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CoverageCost' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccOnDemandCost' - How much an On-Demand instance cost.
coverageCost
    :: CoverageCost
coverageCost = CoverageCost' {_ccOnDemandCost = Nothing}


-- | How much an On-Demand instance cost.
ccOnDemandCost :: Lens' CoverageCost (Maybe Text)
ccOnDemandCost = lens _ccOnDemandCost (\ s a -> s{_ccOnDemandCost = a})

instance FromJSON CoverageCost where
        parseJSON
          = withObject "CoverageCost"
              (\ x -> CoverageCost' <$> (x .:? "OnDemandCost"))

instance Hashable CoverageCost where

instance NFData CoverageCost where

-- | How long a running instance either used a reservation or was On-Demand.
--
--
--
-- /See:/ 'coverageHours' smart constructor.
data CoverageHours = CoverageHours'
  { _chCoverageHoursPercentage :: !(Maybe Text)
  , _chOnDemandHours           :: !(Maybe Text)
  , _chTotalRunningHours       :: !(Maybe Text)
  , _chReservedHours           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CoverageHours' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chCoverageHoursPercentage' - The percentage of instance hours that a reservation covered.
--
-- * 'chOnDemandHours' - The number of instance running hours that On-Demand Instances covered.
--
-- * 'chTotalRunningHours' - The total instance usage, in hours.
--
-- * 'chReservedHours' - The number of instance running hours that reservations covered.
coverageHours
    :: CoverageHours
coverageHours =
  CoverageHours'
    { _chCoverageHoursPercentage = Nothing
    , _chOnDemandHours = Nothing
    , _chTotalRunningHours = Nothing
    , _chReservedHours = Nothing
    }


-- | The percentage of instance hours that a reservation covered.
chCoverageHoursPercentage :: Lens' CoverageHours (Maybe Text)
chCoverageHoursPercentage = lens _chCoverageHoursPercentage (\ s a -> s{_chCoverageHoursPercentage = a})

-- | The number of instance running hours that On-Demand Instances covered.
chOnDemandHours :: Lens' CoverageHours (Maybe Text)
chOnDemandHours = lens _chOnDemandHours (\ s a -> s{_chOnDemandHours = a})

-- | The total instance usage, in hours.
chTotalRunningHours :: Lens' CoverageHours (Maybe Text)
chTotalRunningHours = lens _chTotalRunningHours (\ s a -> s{_chTotalRunningHours = a})

-- | The number of instance running hours that reservations covered.
chReservedHours :: Lens' CoverageHours (Maybe Text)
chReservedHours = lens _chReservedHours (\ s a -> s{_chReservedHours = a})

instance FromJSON CoverageHours where
        parseJSON
          = withObject "CoverageHours"
              (\ x ->
                 CoverageHours' <$>
                   (x .:? "CoverageHoursPercentage") <*>
                     (x .:? "OnDemandHours")
                     <*> (x .:? "TotalRunningHours")
                     <*> (x .:? "ReservedHours"))

instance Hashable CoverageHours where

instance NFData CoverageHours where

-- | The amount of instance usage, in normalized units. Normalized units enable you to see your EC2 usage for multiple sizes of instances in a uniform way. For example, suppose you run an xlarge instance and a 2xlarge instance. If you run both instances for the same amount of time, the 2xlarge instance uses twice as much of your reservation as the xlarge instance, even though both instances show only one instance-hour. Using normalized units instead of instance-hours, the xlarge instance used 8 normalized units, and the 2xlarge instance used 16 normalized units.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/ .
--
--
-- /See:/ 'coverageNormalizedUnits' smart constructor.
data CoverageNormalizedUnits = CoverageNormalizedUnits'
  { _cnuReservedNormalizedUnits           :: !(Maybe Text)
  , _cnuTotalRunningNormalizedUnits       :: !(Maybe Text)
  , _cnuCoverageNormalizedUnitsPercentage :: !(Maybe Text)
  , _cnuOnDemandNormalizedUnits           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CoverageNormalizedUnits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnuReservedNormalizedUnits' - The number of normalized units that a reservation covers.
--
-- * 'cnuTotalRunningNormalizedUnits' - The total number of normalized units that you used.
--
-- * 'cnuCoverageNormalizedUnitsPercentage' - The percentage of your used instance normalized units that a reservation covers.
--
-- * 'cnuOnDemandNormalizedUnits' - The number of normalized units that are covered by On-Demand Instances instead of a reservation.
coverageNormalizedUnits
    :: CoverageNormalizedUnits
coverageNormalizedUnits =
  CoverageNormalizedUnits'
    { _cnuReservedNormalizedUnits = Nothing
    , _cnuTotalRunningNormalizedUnits = Nothing
    , _cnuCoverageNormalizedUnitsPercentage = Nothing
    , _cnuOnDemandNormalizedUnits = Nothing
    }


-- | The number of normalized units that a reservation covers.
cnuReservedNormalizedUnits :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuReservedNormalizedUnits = lens _cnuReservedNormalizedUnits (\ s a -> s{_cnuReservedNormalizedUnits = a})

-- | The total number of normalized units that you used.
cnuTotalRunningNormalizedUnits :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuTotalRunningNormalizedUnits = lens _cnuTotalRunningNormalizedUnits (\ s a -> s{_cnuTotalRunningNormalizedUnits = a})

-- | The percentage of your used instance normalized units that a reservation covers.
cnuCoverageNormalizedUnitsPercentage :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuCoverageNormalizedUnitsPercentage = lens _cnuCoverageNormalizedUnitsPercentage (\ s a -> s{_cnuCoverageNormalizedUnitsPercentage = a})

-- | The number of normalized units that are covered by On-Demand Instances instead of a reservation.
cnuOnDemandNormalizedUnits :: Lens' CoverageNormalizedUnits (Maybe Text)
cnuOnDemandNormalizedUnits = lens _cnuOnDemandNormalizedUnits (\ s a -> s{_cnuOnDemandNormalizedUnits = a})

instance FromJSON CoverageNormalizedUnits where
        parseJSON
          = withObject "CoverageNormalizedUnits"
              (\ x ->
                 CoverageNormalizedUnits' <$>
                   (x .:? "ReservedNormalizedUnits") <*>
                     (x .:? "TotalRunningNormalizedUnits")
                     <*> (x .:? "CoverageNormalizedUnitsPercentage")
                     <*> (x .:? "OnDemandNormalizedUnits"))

instance Hashable CoverageNormalizedUnits where

instance NFData CoverageNormalizedUnits where

-- | The time period that you want the usage and costs for.
--
--
--
-- /See:/ 'dateInterval' smart constructor.
data DateInterval = DateInterval'
  { _diStart :: !Text
  , _diEnd   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DateInterval' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diStart' - The beginning of the time period that you want the usage and costs for. The start date is inclusive. For example, if @start@ is @2017-01-01@ , AWS retrieves cost and usage data starting at @2017-01-01@ up to the end date.
--
-- * 'diEnd' - The end of the time period that you want the usage and costs for. The end date is exclusive. For example, if @end@ is @2017-05-01@ , AWS retrieves cost and usage data from the start date up to, but not including, @2017-05-01@ .
dateInterval
    :: Text -- ^ 'diStart'
    -> Text -- ^ 'diEnd'
    -> DateInterval
dateInterval pStart_ pEnd_ = DateInterval' {_diStart = pStart_, _diEnd = pEnd_}


-- | The beginning of the time period that you want the usage and costs for. The start date is inclusive. For example, if @start@ is @2017-01-01@ , AWS retrieves cost and usage data starting at @2017-01-01@ up to the end date.
diStart :: Lens' DateInterval Text
diStart = lens _diStart (\ s a -> s{_diStart = a})

-- | The end of the time period that you want the usage and costs for. The end date is exclusive. For example, if @end@ is @2017-05-01@ , AWS retrieves cost and usage data from the start date up to, but not including, @2017-05-01@ .
diEnd :: Lens' DateInterval Text
diEnd = lens _diEnd (\ s a -> s{_diEnd = a})

instance FromJSON DateInterval where
        parseJSON
          = withObject "DateInterval"
              (\ x ->
                 DateInterval' <$> (x .: "Start") <*> (x .: "End"))

instance Hashable DateInterval where

instance NFData DateInterval where

instance ToJSON DateInterval where
        toJSON DateInterval'{..}
          = object
              (catMaybes
                 [Just ("Start" .= _diStart), Just ("End" .= _diEnd)])

-- | The metadata that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
--
--
-- /See:/ 'dimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { _dvValues :: !(Maybe [Text])
  , _dvKey    :: !(Maybe Dimension)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DimensionValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvValues' - The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values. Valid values for the @SERVICE@ dimension are @Amazon Elastic Compute Cloud - Compute@ , @Amazon Elasticsearch Service@ , @Amazon ElastiCache@ , @Amazon Redshift@ , and @Amazon Relational Database Service@ .
--
-- * 'dvKey' - The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
dimensionValues
    :: DimensionValues
dimensionValues = DimensionValues' {_dvValues = Nothing, _dvKey = Nothing}


-- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values. Valid values for the @SERVICE@ dimension are @Amazon Elastic Compute Cloud - Compute@ , @Amazon Elasticsearch Service@ , @Amazon ElastiCache@ , @Amazon Redshift@ , and @Amazon Relational Database Service@ .
dvValues :: Lens' DimensionValues [Text]
dvValues = lens _dvValues (\ s a -> s{_dvValues = a}) . _Default . _Coerce

-- | The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
dvKey :: Lens' DimensionValues (Maybe Dimension)
dvKey = lens _dvKey (\ s a -> s{_dvKey = a})

instance Hashable DimensionValues where

instance NFData DimensionValues where

instance ToJSON DimensionValues where
        toJSON DimensionValues'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _dvValues, ("Key" .=) <$> _dvKey])

-- | The metadata of a specific type that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
--
--
-- /See:/ 'dimensionValuesWithAttributes' smart constructor.
data DimensionValuesWithAttributes = DimensionValuesWithAttributes'
  { _dvwaValue      :: !(Maybe Text)
  , _dvwaAttributes :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DimensionValuesWithAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvwaValue' - The value of a dimension with a specific attribute.
--
-- * 'dvwaAttributes' - The attribute that applies to a specific @Dimension@ .
dimensionValuesWithAttributes
    :: DimensionValuesWithAttributes
dimensionValuesWithAttributes =
  DimensionValuesWithAttributes'
    {_dvwaValue = Nothing, _dvwaAttributes = Nothing}


-- | The value of a dimension with a specific attribute.
dvwaValue :: Lens' DimensionValuesWithAttributes (Maybe Text)
dvwaValue = lens _dvwaValue (\ s a -> s{_dvwaValue = a})

-- | The attribute that applies to a specific @Dimension@ .
dvwaAttributes :: Lens' DimensionValuesWithAttributes (HashMap Text Text)
dvwaAttributes = lens _dvwaAttributes (\ s a -> s{_dvwaAttributes = a}) . _Default . _Map

instance FromJSON DimensionValuesWithAttributes where
        parseJSON
          = withObject "DimensionValuesWithAttributes"
              (\ x ->
                 DimensionValuesWithAttributes' <$>
                   (x .:? "Value") <*> (x .:? "Attributes" .!= mempty))

instance Hashable DimensionValuesWithAttributes where

instance NFData DimensionValuesWithAttributes where

-- | Details about the Amazon EC2 instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'ec2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { _eidCurrentGeneration :: !(Maybe Bool)
  , _eidPlatform          :: !(Maybe Text)
  , _eidFamily            :: !(Maybe Text)
  , _eidInstanceType      :: !(Maybe Text)
  , _eidAvailabilityZone  :: !(Maybe Text)
  , _eidSizeFlexEligible  :: !(Maybe Bool)
  , _eidTenancy           :: !(Maybe Text)
  , _eidRegion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2InstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eidCurrentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- * 'eidPlatform' - The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- * 'eidFamily' - The instance family of the recommended reservation.
--
-- * 'eidInstanceType' - The type of instance that AWS recommends.
--
-- * 'eidAvailabilityZone' - The Availability Zone of the recommended reservation.
--
-- * 'eidSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'eidTenancy' - Whether the recommended reservation is dedicated or shared.
--
-- * 'eidRegion' - The AWS Region of the recommended reservation.
ec2InstanceDetails
    :: EC2InstanceDetails
ec2InstanceDetails =
  EC2InstanceDetails'
    { _eidCurrentGeneration = Nothing
    , _eidPlatform = Nothing
    , _eidFamily = Nothing
    , _eidInstanceType = Nothing
    , _eidAvailabilityZone = Nothing
    , _eidSizeFlexEligible = Nothing
    , _eidTenancy = Nothing
    , _eidRegion = Nothing
    }


-- | Whether the recommendation is for a current-generation instance.
eidCurrentGeneration :: Lens' EC2InstanceDetails (Maybe Bool)
eidCurrentGeneration = lens _eidCurrentGeneration (\ s a -> s{_eidCurrentGeneration = a})

-- | The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
eidPlatform :: Lens' EC2InstanceDetails (Maybe Text)
eidPlatform = lens _eidPlatform (\ s a -> s{_eidPlatform = a})

-- | The instance family of the recommended reservation.
eidFamily :: Lens' EC2InstanceDetails (Maybe Text)
eidFamily = lens _eidFamily (\ s a -> s{_eidFamily = a})

-- | The type of instance that AWS recommends.
eidInstanceType :: Lens' EC2InstanceDetails (Maybe Text)
eidInstanceType = lens _eidInstanceType (\ s a -> s{_eidInstanceType = a})

-- | The Availability Zone of the recommended reservation.
eidAvailabilityZone :: Lens' EC2InstanceDetails (Maybe Text)
eidAvailabilityZone = lens _eidAvailabilityZone (\ s a -> s{_eidAvailabilityZone = a})

-- | Whether the recommended reservation is size flexible.
eidSizeFlexEligible :: Lens' EC2InstanceDetails (Maybe Bool)
eidSizeFlexEligible = lens _eidSizeFlexEligible (\ s a -> s{_eidSizeFlexEligible = a})

-- | Whether the recommended reservation is dedicated or shared.
eidTenancy :: Lens' EC2InstanceDetails (Maybe Text)
eidTenancy = lens _eidTenancy (\ s a -> s{_eidTenancy = a})

-- | The AWS Region of the recommended reservation.
eidRegion :: Lens' EC2InstanceDetails (Maybe Text)
eidRegion = lens _eidRegion (\ s a -> s{_eidRegion = a})

instance FromJSON EC2InstanceDetails where
        parseJSON
          = withObject "EC2InstanceDetails"
              (\ x ->
                 EC2InstanceDetails' <$>
                   (x .:? "CurrentGeneration") <*> (x .:? "Platform")
                     <*> (x .:? "Family")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "SizeFlexEligible")
                     <*> (x .:? "Tenancy")
                     <*> (x .:? "Region"))

instance Hashable EC2InstanceDetails where

instance NFData EC2InstanceDetails where

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
--
--
--
-- /See:/ 'ec2Specification' smart constructor.
newtype EC2Specification = EC2Specification'
  { _esOfferingClass :: Maybe OfferingClass
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2Specification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esOfferingClass' - Whether you want a recommendation for standard or convertible reservations.
ec2Specification
    :: EC2Specification
ec2Specification = EC2Specification' {_esOfferingClass = Nothing}


-- | Whether you want a recommendation for standard or convertible reservations.
esOfferingClass :: Lens' EC2Specification (Maybe OfferingClass)
esOfferingClass = lens _esOfferingClass (\ s a -> s{_esOfferingClass = a})

instance FromJSON EC2Specification where
        parseJSON
          = withObject "EC2Specification"
              (\ x ->
                 EC2Specification' <$> (x .:? "OfferingClass"))

instance Hashable EC2Specification where

instance NFData EC2Specification where

instance ToJSON EC2Specification where
        toJSON EC2Specification'{..}
          = object
              (catMaybes
                 [("OfferingClass" .=) <$> _esOfferingClass])

-- | Details about the Amazon ES instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'eSInstanceDetails' smart constructor.
data ESInstanceDetails = ESInstanceDetails'
  { _esidCurrentGeneration :: !(Maybe Bool)
  , _esidInstanceClass     :: !(Maybe Text)
  , _esidInstanceSize      :: !(Maybe Text)
  , _esidSizeFlexEligible  :: !(Maybe Bool)
  , _esidRegion            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ESInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esidCurrentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- * 'esidInstanceClass' - The class of instance that AWS recommends.
--
-- * 'esidInstanceSize' - The size of instance that AWS recommends.
--
-- * 'esidSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'esidRegion' - The AWS Region of the recommended reservation.
eSInstanceDetails
    :: ESInstanceDetails
eSInstanceDetails =
  ESInstanceDetails'
    { _esidCurrentGeneration = Nothing
    , _esidInstanceClass = Nothing
    , _esidInstanceSize = Nothing
    , _esidSizeFlexEligible = Nothing
    , _esidRegion = Nothing
    }


-- | Whether the recommendation is for a current-generation instance.
esidCurrentGeneration :: Lens' ESInstanceDetails (Maybe Bool)
esidCurrentGeneration = lens _esidCurrentGeneration (\ s a -> s{_esidCurrentGeneration = a})

-- | The class of instance that AWS recommends.
esidInstanceClass :: Lens' ESInstanceDetails (Maybe Text)
esidInstanceClass = lens _esidInstanceClass (\ s a -> s{_esidInstanceClass = a})

-- | The size of instance that AWS recommends.
esidInstanceSize :: Lens' ESInstanceDetails (Maybe Text)
esidInstanceSize = lens _esidInstanceSize (\ s a -> s{_esidInstanceSize = a})

-- | Whether the recommended reservation is size flexible.
esidSizeFlexEligible :: Lens' ESInstanceDetails (Maybe Bool)
esidSizeFlexEligible = lens _esidSizeFlexEligible (\ s a -> s{_esidSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
esidRegion :: Lens' ESInstanceDetails (Maybe Text)
esidRegion = lens _esidRegion (\ s a -> s{_esidRegion = a})

instance FromJSON ESInstanceDetails where
        parseJSON
          = withObject "ESInstanceDetails"
              (\ x ->
                 ESInstanceDetails' <$>
                   (x .:? "CurrentGeneration") <*>
                     (x .:? "InstanceClass")
                     <*> (x .:? "InstanceSize")
                     <*> (x .:? "SizeFlexEligible")
                     <*> (x .:? "Region"))

instance Hashable ESInstanceDetails where

instance NFData ESInstanceDetails where

-- | Details about the Amazon ElastiCache instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'elastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { _ecidCurrentGeneration  :: !(Maybe Bool)
  , _ecidProductDescription :: !(Maybe Text)
  , _ecidFamily             :: !(Maybe Text)
  , _ecidSizeFlexEligible   :: !(Maybe Bool)
  , _ecidRegion             :: !(Maybe Text)
  , _ecidNodeType           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElastiCacheInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecidCurrentGeneration' - Whether the recommendation is for a current generation instance.
--
-- * 'ecidProductDescription' - The description of the recommended reservation.
--
-- * 'ecidFamily' - The instance family of the recommended reservation.
--
-- * 'ecidSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'ecidRegion' - The AWS Region of the recommended reservation.
--
-- * 'ecidNodeType' - The type of node that AWS recommends.
elastiCacheInstanceDetails
    :: ElastiCacheInstanceDetails
elastiCacheInstanceDetails =
  ElastiCacheInstanceDetails'
    { _ecidCurrentGeneration = Nothing
    , _ecidProductDescription = Nothing
    , _ecidFamily = Nothing
    , _ecidSizeFlexEligible = Nothing
    , _ecidRegion = Nothing
    , _ecidNodeType = Nothing
    }


-- | Whether the recommendation is for a current generation instance.
ecidCurrentGeneration :: Lens' ElastiCacheInstanceDetails (Maybe Bool)
ecidCurrentGeneration = lens _ecidCurrentGeneration (\ s a -> s{_ecidCurrentGeneration = a})

-- | The description of the recommended reservation.
ecidProductDescription :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidProductDescription = lens _ecidProductDescription (\ s a -> s{_ecidProductDescription = a})

-- | The instance family of the recommended reservation.
ecidFamily :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidFamily = lens _ecidFamily (\ s a -> s{_ecidFamily = a})

-- | Whether the recommended reservation is size flexible.
ecidSizeFlexEligible :: Lens' ElastiCacheInstanceDetails (Maybe Bool)
ecidSizeFlexEligible = lens _ecidSizeFlexEligible (\ s a -> s{_ecidSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
ecidRegion :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidRegion = lens _ecidRegion (\ s a -> s{_ecidRegion = a})

-- | The type of node that AWS recommends.
ecidNodeType :: Lens' ElastiCacheInstanceDetails (Maybe Text)
ecidNodeType = lens _ecidNodeType (\ s a -> s{_ecidNodeType = a})

instance FromJSON ElastiCacheInstanceDetails where
        parseJSON
          = withObject "ElastiCacheInstanceDetails"
              (\ x ->
                 ElastiCacheInstanceDetails' <$>
                   (x .:? "CurrentGeneration") <*>
                     (x .:? "ProductDescription")
                     <*> (x .:? "Family")
                     <*> (x .:? "SizeFlexEligible")
                     <*> (x .:? "Region")
                     <*> (x .:? "NodeType"))

instance Hashable ElastiCacheInstanceDetails where

instance NFData ElastiCacheInstanceDetails where

-- | Use @Expression@ to filter by cost or by usage. There are two patterns:
--
--
--     * Simple dimension values - You can set the dimension name and values for the filters that you plan to use. For example, you can filter for @INSTANCE_TYPE==m4.xlarge OR INSTANCE_TYPE==c4.large@ . The @Expression@ for that looks like this:
--
-- @{ "Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.xlarge",
