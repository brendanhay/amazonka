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
newtype Coverage = Coverage'
  { _cCoverageHours :: Maybe CoverageHours
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Coverage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCoverageHours' - The amount of instance usage that a reservation covered, in hours.
coverage
    :: Coverage
coverage = Coverage' {_cCoverageHours = Nothing}


-- | The amount of instance usage that a reservation covered, in hours.
cCoverageHours :: Lens' Coverage (Maybe CoverageHours)
cCoverageHours = lens _cCoverageHours (\ s a -> s{_cCoverageHours = a})

instance FromJSON Coverage where
        parseJSON
          = withObject "Coverage"
              (\ x -> Coverage' <$> (x .:? "CoverageHours"))

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
-- * 'cbtGroups' - The groups of instances that are covered by a reservation.
--
-- * 'cbtTimePeriod' - The period over which this coverage was used.
--
-- * 'cbtTotal' - The total reservation coverage, in hours.
coverageByTime
    :: CoverageByTime
coverageByTime =
  CoverageByTime'
    {_cbtGroups = Nothing, _cbtTimePeriod = Nothing, _cbtTotal = Nothing}


-- | The groups of instances that are covered by a reservation.
cbtGroups :: Lens' CoverageByTime [ReservationCoverageGroup]
cbtGroups = lens _cbtGroups (\ s a -> s{_cbtGroups = a}) . _Default . _Coerce

-- | The period over which this coverage was used.
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
-- * 'chCoverageHoursPercentage' - The percentage of instance hours that are covered by a reservation.
--
-- * 'chOnDemandHours' - The number of instance running hours that are covered by On-Demand Instances.
--
-- * 'chTotalRunningHours' - The total instance usage, in hours.
--
-- * 'chReservedHours' - The number of instance running hours that are covered by reservations.
coverageHours
    :: CoverageHours
coverageHours =
  CoverageHours'
    { _chCoverageHoursPercentage = Nothing
    , _chOnDemandHours = Nothing
    , _chTotalRunningHours = Nothing
    , _chReservedHours = Nothing
    }


-- | The percentage of instance hours that are covered by a reservation.
chCoverageHoursPercentage :: Lens' CoverageHours (Maybe Text)
chCoverageHoursPercentage = lens _chCoverageHoursPercentage (\ s a -> s{_chCoverageHoursPercentage = a})

-- | The number of instance running hours that are covered by On-Demand Instances.
chOnDemandHours :: Lens' CoverageHours (Maybe Text)
chOnDemandHours = lens _chOnDemandHours (\ s a -> s{_chOnDemandHours = a})

-- | The total instance usage, in hours.
chTotalRunningHours :: Lens' CoverageHours (Maybe Text)
chTotalRunningHours = lens _chTotalRunningHours (\ s a -> s{_chTotalRunningHours = a})

-- | The number of instance running hours that are covered by reservations.
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
-- * 'dvValues' - The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- * 'dvKey' - The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
dimensionValues
    :: DimensionValues
dimensionValues = DimensionValues' {_dvValues = Nothing, _dvKey = Nothing}


-- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
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

-- | Details about the EC2 instances that AWS recommends that you purchase.
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
-- * 'eidCurrentGeneration' - Whether the recommendation is for a current generation instance.
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


-- | Whether the recommendation is for a current generation instance.
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

-- | The EC2 hardware specifications that you want AWS to provide recommendations for.
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

-- | Use @Expression@ to filter by cost or by usage. There are two patterns:
--
--
--     * Simple dimension values - You can set the dimension name and values for the filters that you plan to use. For example, you can filter for @INSTANCE_TYPE==m4.xlarge OR INSTANCE_TYPE==c4.large@ . The @Expression@ for that looks like this:
--
-- @{ "Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.xlarge", “c4.large” ] } }@
--
-- The list of dimension values are OR'd together to retrieve cost or usage data. You can create @Expression@ and @DimensionValues@ objects using either @with*@ methods or @set*@ methods in multiple lines.
--
--     * Compound dimension values with logical operations - You can use multiple @Expression@ types and the logical operators @AND/OR/NOT@ to create a list of one or more @Expression@ objects. This allows you to filter on more advanced options. For example, you can filter on @((INSTANCE_TYPE == m4.large OR INSTANCE_TYPE == m3.large) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)@ . The @Expression@ for that looks like this:
--
-- @{ "And": [ {"Or": [ {"Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.x.large", "c4.large" ] }}, {"Tag": { "Key": "TagName", "Values": ["Value1"] } } ]}, {"Not": {"dimensions": { "Key": "USAGE_TYPE", "Values": ["DataTransfer"] }}} ] } @
--
-- @{ "And": [ ... ], "DimensionValues": { "Dimension": "USAGE_TYPE", "Values": [ "DataTransfer" ] } } @
--
--
--
--
-- /See:/ 'expression' smart constructor.
data Expression = Expression'
  { _eNot        :: !(Maybe Expression)
  , _eAnd        :: !(Maybe [Expression])
  , _eOr         :: !(Maybe [Expression])
  , _eDimensions :: !(Maybe DimensionValues)
  , _eTags       :: !(Maybe TagValues)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Expression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eNot' - Return results that don't match a @Dimension@ object.
--
-- * 'eAnd' - Return results that match both @Dimension@ objects.
--
-- * 'eOr' - Return results that match either @Dimension@ object.
--
-- * 'eDimensions' - The specific @Dimension@ to use for @Expression@ .
--
-- * 'eTags' - The specific @Tag@ to use for @Expression@ .
expression
    :: Expression
expression =
  Expression'
    { _eNot = Nothing
    , _eAnd = Nothing
    , _eOr = Nothing
    , _eDimensions = Nothing
    , _eTags = Nothing
    }


-- | Return results that don't match a @Dimension@ object.
eNot :: Lens' Expression (Maybe Expression)
eNot = lens _eNot (\ s a -> s{_eNot = a})

-- | Return results that match both @Dimension@ objects.
eAnd :: Lens' Expression [Expression]
eAnd = lens _eAnd (\ s a -> s{_eAnd = a}) . _Default . _Coerce

-- | Return results that match either @Dimension@ object.
eOr :: Lens' Expression [Expression]
eOr = lens _eOr (\ s a -> s{_eOr = a}) . _Default . _Coerce

-- | The specific @Dimension@ to use for @Expression@ .
eDimensions :: Lens' Expression (Maybe DimensionValues)
eDimensions = lens _eDimensions (\ s a -> s{_eDimensions = a})

-- | The specific @Tag@ to use for @Expression@ .
eTags :: Lens' Expression (Maybe TagValues)
eTags = lens _eTags (\ s a -> s{_eTags = a})

instance Hashable Expression where

instance NFData Expression where

instance ToJSON Expression where
        toJSON Expression'{..}
          = object
              (catMaybes
                 [("Not" .=) <$> _eNot, ("And" .=) <$> _eAnd,
                  ("Or" .=) <$> _eOr,
                  ("Dimensions" .=) <$> _eDimensions,
                  ("Tags" .=) <$> _eTags])

-- | One level of grouped data within the results.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gMetrics :: !(Maybe (Map Text MetricValue))
  , _gKeys    :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gMetrics' - The metrics that are included in this group.
--
-- * 'gKeys' - The keys that are included in this group.
group'
    :: Group
group' = Group' {_gMetrics = Nothing, _gKeys = Nothing}


-- | The metrics that are included in this group.
gMetrics :: Lens' Group (HashMap Text MetricValue)
gMetrics = lens _gMetrics (\ s a -> s{_gMetrics = a}) . _Default . _Map

-- | The keys that are included in this group.
gKeys :: Lens' Group [Text]
gKeys = lens _gKeys (\ s a -> s{_gKeys = a}) . _Default . _Coerce

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ x ->
                 Group' <$>
                   (x .:? "Metrics" .!= mempty) <*>
                     (x .:? "Keys" .!= mempty))

instance Hashable Group where

instance NFData Group where

-- | Represents a group when you specify a group by criteria, or in the response to a query with a specific grouping.
--
--
--
-- /See:/ 'groupDefinition' smart constructor.
data GroupDefinition = GroupDefinition'
  { _gdKey  :: !(Maybe Text)
  , _gdType :: !(Maybe GroupDefinitionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdKey' - The string that represents a key for a specified group.
--
-- * 'gdType' - The string that represents the type of group.
groupDefinition
    :: GroupDefinition
groupDefinition = GroupDefinition' {_gdKey = Nothing, _gdType = Nothing}


-- | The string that represents a key for a specified group.
gdKey :: Lens' GroupDefinition (Maybe Text)
gdKey = lens _gdKey (\ s a -> s{_gdKey = a})

-- | The string that represents the type of group.
gdType :: Lens' GroupDefinition (Maybe GroupDefinitionType)
gdType = lens _gdType (\ s a -> s{_gdType = a})

instance FromJSON GroupDefinition where
        parseJSON
          = withObject "GroupDefinition"
              (\ x ->
                 GroupDefinition' <$>
                   (x .:? "Key") <*> (x .:? "Type"))

instance Hashable GroupDefinition where

instance NFData GroupDefinition where

instance ToJSON GroupDefinition where
        toJSON GroupDefinition'{..}
          = object
              (catMaybes
                 [("Key" .=) <$> _gdKey, ("Type" .=) <$> _gdType])

-- | Details about the instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'instanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { _idRDSInstanceDetails :: !(Maybe RDSInstanceDetails)
  , _idEC2InstanceDetails :: !(Maybe EC2InstanceDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idRDSInstanceDetails' - The RDS instances that AWS recommends that you purchase.
--
-- * 'idEC2InstanceDetails' - The EC2 instances that AWS recommends that you purchase.
instanceDetails
    :: InstanceDetails
instanceDetails =
  InstanceDetails'
    {_idRDSInstanceDetails = Nothing, _idEC2InstanceDetails = Nothing}


-- | The RDS instances that AWS recommends that you purchase.
idRDSInstanceDetails :: Lens' InstanceDetails (Maybe RDSInstanceDetails)
idRDSInstanceDetails = lens _idRDSInstanceDetails (\ s a -> s{_idRDSInstanceDetails = a})

-- | The EC2 instances that AWS recommends that you purchase.
idEC2InstanceDetails :: Lens' InstanceDetails (Maybe EC2InstanceDetails)
idEC2InstanceDetails = lens _idEC2InstanceDetails (\ s a -> s{_idEC2InstanceDetails = a})

instance FromJSON InstanceDetails where
        parseJSON
          = withObject "InstanceDetails"
              (\ x ->
                 InstanceDetails' <$>
                   (x .:? "RDSInstanceDetails") <*>
                     (x .:? "EC2InstanceDetails"))

instance Hashable InstanceDetails where

instance NFData InstanceDetails where

-- | The aggregated value for a metric.
--
--
--
-- /See:/ 'metricValue' smart constructor.
data MetricValue = MetricValue'
  { _mvAmount :: !(Maybe Text)
  , _mvUnit   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvAmount' - The actual number that represents the metric.
--
-- * 'mvUnit' - The unit that the metric is given in.
metricValue
    :: MetricValue
metricValue = MetricValue' {_mvAmount = Nothing, _mvUnit = Nothing}


-- | The actual number that represents the metric.
mvAmount :: Lens' MetricValue (Maybe Text)
mvAmount = lens _mvAmount (\ s a -> s{_mvAmount = a})

-- | The unit that the metric is given in.
mvUnit :: Lens' MetricValue (Maybe Text)
mvUnit = lens _mvUnit (\ s a -> s{_mvUnit = a})

instance FromJSON MetricValue where
        parseJSON
          = withObject "MetricValue"
              (\ x ->
                 MetricValue' <$> (x .:? "Amount") <*> (x .:? "Unit"))

instance Hashable MetricValue where

instance NFData MetricValue where

-- | Details about the RDS instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'rdsInstanceDetails' smart constructor.
data RDSInstanceDetails = RDSInstanceDetails'
  { _ridCurrentGeneration :: !(Maybe Bool)
  , _ridDeploymentOption  :: !(Maybe Text)
  , _ridFamily            :: !(Maybe Text)
  , _ridInstanceType      :: !(Maybe Text)
  , _ridLicenseModel      :: !(Maybe Text)
  , _ridSizeFlexEligible  :: !(Maybe Bool)
  , _ridRegion            :: !(Maybe Text)
  , _ridDatabaseEngine    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RDSInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ridCurrentGeneration' - Whether the recommendation is for a current generation instance.
--
-- * 'ridDeploymentOption' - Whether the recommendation is for a reservation in a single availability zone or a reservation with a backup in a second availability zone.
--
-- * 'ridFamily' - The instance family of the recommended reservation.
--
-- * 'ridInstanceType' - The type of instance that AWS recommends.
--
-- * 'ridLicenseModel' - The license model that the recommended reservation supports.
--
-- * 'ridSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'ridRegion' - The AWS Region of the recommended reservation.
--
-- * 'ridDatabaseEngine' - The database engine that the recommended reservation supports.
rdsInstanceDetails
    :: RDSInstanceDetails
rdsInstanceDetails =
  RDSInstanceDetails'
    { _ridCurrentGeneration = Nothing
    , _ridDeploymentOption = Nothing
    , _ridFamily = Nothing
    , _ridInstanceType = Nothing
    , _ridLicenseModel = Nothing
    , _ridSizeFlexEligible = Nothing
    , _ridRegion = Nothing
    , _ridDatabaseEngine = Nothing
    }


-- | Whether the recommendation is for a current generation instance.
ridCurrentGeneration :: Lens' RDSInstanceDetails (Maybe Bool)
ridCurrentGeneration = lens _ridCurrentGeneration (\ s a -> s{_ridCurrentGeneration = a})

-- | Whether the recommendation is for a reservation in a single availability zone or a reservation with a backup in a second availability zone.
ridDeploymentOption :: Lens' RDSInstanceDetails (Maybe Text)
ridDeploymentOption = lens _ridDeploymentOption (\ s a -> s{_ridDeploymentOption = a})

-- | The instance family of the recommended reservation.
ridFamily :: Lens' RDSInstanceDetails (Maybe Text)
ridFamily = lens _ridFamily (\ s a -> s{_ridFamily = a})

-- | The type of instance that AWS recommends.
ridInstanceType :: Lens' RDSInstanceDetails (Maybe Text)
ridInstanceType = lens _ridInstanceType (\ s a -> s{_ridInstanceType = a})

-- | The license model that the recommended reservation supports.
ridLicenseModel :: Lens' RDSInstanceDetails (Maybe Text)
ridLicenseModel = lens _ridLicenseModel (\ s a -> s{_ridLicenseModel = a})

-- | Whether the recommended reservation is size flexible.
ridSizeFlexEligible :: Lens' RDSInstanceDetails (Maybe Bool)
ridSizeFlexEligible = lens _ridSizeFlexEligible (\ s a -> s{_ridSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
ridRegion :: Lens' RDSInstanceDetails (Maybe Text)
ridRegion = lens _ridRegion (\ s a -> s{_ridRegion = a})

-- | The database engine that the recommended reservation supports.
ridDatabaseEngine :: Lens' RDSInstanceDetails (Maybe Text)
ridDatabaseEngine = lens _ridDatabaseEngine (\ s a -> s{_ridDatabaseEngine = a})

instance FromJSON RDSInstanceDetails where
        parseJSON
          = withObject "RDSInstanceDetails"
              (\ x ->
                 RDSInstanceDetails' <$>
                   (x .:? "CurrentGeneration") <*>
                     (x .:? "DeploymentOption")
                     <*> (x .:? "Family")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "LicenseModel")
                     <*> (x .:? "SizeFlexEligible")
                     <*> (x .:? "Region")
                     <*> (x .:? "DatabaseEngine"))

instance Hashable RDSInstanceDetails where

instance NFData RDSInstanceDetails where

-- | The aggregated numbers for your RI usage.
--
--
--
-- /See:/ 'reservationAggregates' smart constructor.
data ReservationAggregates = ReservationAggregates'
  { _raPurchasedHours        :: !(Maybe Text)
  , _raTotalActualHours      :: !(Maybe Text)
  , _raUtilizationPercentage :: !(Maybe Text)
  , _raUnusedHours           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationAggregates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raPurchasedHours' - How many RI hours that you purchased.
--
-- * 'raTotalActualHours' - The total number of RI hours that you used.
--
-- * 'raUtilizationPercentage' - The percentage of RI time that you used.
--
-- * 'raUnusedHours' - The number of RI hours that you didn't use.
reservationAggregates
    :: ReservationAggregates
reservationAggregates =
  ReservationAggregates'
    { _raPurchasedHours = Nothing
    , _raTotalActualHours = Nothing
    , _raUtilizationPercentage = Nothing
    , _raUnusedHours = Nothing
    }


-- | How many RI hours that you purchased.
raPurchasedHours :: Lens' ReservationAggregates (Maybe Text)
raPurchasedHours = lens _raPurchasedHours (\ s a -> s{_raPurchasedHours = a})

-- | The total number of RI hours that you used.
raTotalActualHours :: Lens' ReservationAggregates (Maybe Text)
raTotalActualHours = lens _raTotalActualHours (\ s a -> s{_raTotalActualHours = a})

-- | The percentage of RI time that you used.
raUtilizationPercentage :: Lens' ReservationAggregates (Maybe Text)
raUtilizationPercentage = lens _raUtilizationPercentage (\ s a -> s{_raUtilizationPercentage = a})

-- | The number of RI hours that you didn't use.
raUnusedHours :: Lens' ReservationAggregates (Maybe Text)
raUnusedHours = lens _raUnusedHours (\ s a -> s{_raUnusedHours = a})

instance FromJSON ReservationAggregates where
        parseJSON
          = withObject "ReservationAggregates"
              (\ x ->
                 ReservationAggregates' <$>
                   (x .:? "PurchasedHours") <*>
                     (x .:? "TotalActualHours")
                     <*> (x .:? "UtilizationPercentage")
                     <*> (x .:? "UnusedHours"))

instance Hashable ReservationAggregates where

instance NFData ReservationAggregates where

-- | A group of reservations that share a set of attributes.
--
--
--
-- /See:/ 'reservationCoverageGroup' smart constructor.
data ReservationCoverageGroup = ReservationCoverageGroup'
  { _rcgCoverage   :: !(Maybe Coverage)
  , _rcgAttributes :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationCoverageGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcgCoverage' - How much instance usage this group of reservations covered.
--
-- * 'rcgAttributes' - The attributes for this group of reservations.
reservationCoverageGroup
    :: ReservationCoverageGroup
reservationCoverageGroup =
  ReservationCoverageGroup' {_rcgCoverage = Nothing, _rcgAttributes = Nothing}


-- | How much instance usage this group of reservations covered.
rcgCoverage :: Lens' ReservationCoverageGroup (Maybe Coverage)
rcgCoverage = lens _rcgCoverage (\ s a -> s{_rcgCoverage = a})

-- | The attributes for this group of reservations.
rcgAttributes :: Lens' ReservationCoverageGroup (HashMap Text Text)
rcgAttributes = lens _rcgAttributes (\ s a -> s{_rcgAttributes = a}) . _Default . _Map

instance FromJSON ReservationCoverageGroup where
        parseJSON
          = withObject "ReservationCoverageGroup"
              (\ x ->
                 ReservationCoverageGroup' <$>
                   (x .:? "Coverage") <*>
                     (x .:? "Attributes" .!= mempty))

instance Hashable ReservationCoverageGroup where

instance NFData ReservationCoverageGroup where

-- | A specific reservation that AWS recommends for purchase.
--
--
--
-- /See:/ 'reservationPurchaseRecommendation' smart constructor.
data ReservationPurchaseRecommendation = ReservationPurchaseRecommendation'
  { _rprTermInYears :: !(Maybe TermInYears)
  , _rprRecommendationSummary :: !(Maybe ReservationPurchaseRecommendationSummary)
  , _rprServiceSpecification :: !(Maybe ServiceSpecification)
  , _rprAccountScope :: !(Maybe AccountScope)
  , _rprRecommendationDetails :: !(Maybe [ReservationPurchaseRecommendationDetail])
  , _rprLookbackPeriodInDays :: !(Maybe LookbackPeriodInDays)
  , _rprPaymentOption :: !(Maybe PaymentOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationPurchaseRecommendation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprTermInYears' - The term of the reservation that you want recommendations for, in years.
--
-- * 'rprRecommendationSummary' - A summary about the recommended purchase.
--
-- * 'rprServiceSpecification' - Hardware specifications for the service that you want recommendations for.
--
-- * 'rprAccountScope' - The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
--
-- * 'rprRecommendationDetails' - Details about the recommended purchases.
--
-- * 'rprLookbackPeriodInDays' - How many days of previous usage that AWS takes into consideration when making this recommendation.
--
-- * 'rprPaymentOption' - The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
reservationPurchaseRecommendation
    :: ReservationPurchaseRecommendation
reservationPurchaseRecommendation =
  ReservationPurchaseRecommendation'
    { _rprTermInYears = Nothing
    , _rprRecommendationSummary = Nothing
    , _rprServiceSpecification = Nothing
    , _rprAccountScope = Nothing
    , _rprRecommendationDetails = Nothing
    , _rprLookbackPeriodInDays = Nothing
    , _rprPaymentOption = Nothing
    }


-- | The term of the reservation that you want recommendations for, in years.
rprTermInYears :: Lens' ReservationPurchaseRecommendation (Maybe TermInYears)
rprTermInYears = lens _rprTermInYears (\ s a -> s{_rprTermInYears = a})

-- | A summary about the recommended purchase.
rprRecommendationSummary :: Lens' ReservationPurchaseRecommendation (Maybe ReservationPurchaseRecommendationSummary)
rprRecommendationSummary = lens _rprRecommendationSummary (\ s a -> s{_rprRecommendationSummary = a})

-- | Hardware specifications for the service that you want recommendations for.
rprServiceSpecification :: Lens' ReservationPurchaseRecommendation (Maybe ServiceSpecification)
rprServiceSpecification = lens _rprServiceSpecification (\ s a -> s{_rprServiceSpecification = a})

-- | The account scope that AWS recommends that you purchase this instance for. For example, you can purchase this reservation for an entire organization in AWS Organizations.
rprAccountScope :: Lens' ReservationPurchaseRecommendation (Maybe AccountScope)
rprAccountScope = lens _rprAccountScope (\ s a -> s{_rprAccountScope = a})

-- | Details about the recommended purchases.
rprRecommendationDetails :: Lens' ReservationPurchaseRecommendation [ReservationPurchaseRecommendationDetail]
rprRecommendationDetails = lens _rprRecommendationDetails (\ s a -> s{_rprRecommendationDetails = a}) . _Default . _Coerce

-- | How many days of previous usage that AWS takes into consideration when making this recommendation.
rprLookbackPeriodInDays :: Lens' ReservationPurchaseRecommendation (Maybe LookbackPeriodInDays)
rprLookbackPeriodInDays = lens _rprLookbackPeriodInDays (\ s a -> s{_rprLookbackPeriodInDays = a})

-- | The payment option for the reservation. For example, @AllUpfront@ or @NoUpfront@ .
rprPaymentOption :: Lens' ReservationPurchaseRecommendation (Maybe PaymentOption)
rprPaymentOption = lens _rprPaymentOption (\ s a -> s{_rprPaymentOption = a})

instance FromJSON ReservationPurchaseRecommendation
         where
        parseJSON
          = withObject "ReservationPurchaseRecommendation"
              (\ x ->
                 ReservationPurchaseRecommendation' <$>
                   (x .:? "TermInYears") <*>
                     (x .:? "RecommendationSummary")
                     <*> (x .:? "ServiceSpecification")
                     <*> (x .:? "AccountScope")
                     <*> (x .:? "RecommendationDetails" .!= mempty)
                     <*> (x .:? "LookbackPeriodInDays")
                     <*> (x .:? "PaymentOption"))

instance Hashable ReservationPurchaseRecommendation
         where

instance NFData ReservationPurchaseRecommendation
         where

-- | Details about your recommended reservation purchase.
--
--
--
-- /See:/ 'reservationPurchaseRecommendationDetail' smart constructor.
data ReservationPurchaseRecommendationDetail = ReservationPurchaseRecommendationDetail'
  { _rprdMaximumNormalizedUnitsUsedPerHour         :: !(Maybe Text)
  , _rprdRecurringStandardMonthlyCost              :: !(Maybe Text)
  , _rprdAverageNormalizedUnitsUsedPerHour         :: !(Maybe Text)
  , _rprdCurrencyCode                              :: !(Maybe Text)
  , _rprdEstimatedMonthlySavingsPercentage         :: !(Maybe Text)
  , _rprdRecommendedNormalizedUnitsToPurchase      :: !(Maybe Text)
  , _rprdAverageUtilization                        :: !(Maybe Text)
  , _rprdEstimatedMonthlySavingsAmount             :: !(Maybe Text)
  , _rprdUpfrontCost                               :: !(Maybe Text)
  , _rprdMinimumNormalizedUnitsUsedPerHour         :: !(Maybe Text)
  , _rprdEstimatedMonthlyOnDemandCost              :: !(Maybe Text)
  , _rprdRecommendedNumberOfInstancesToPurchase    :: !(Maybe Text)
  , _rprdMaximumNumberOfInstancesUsedPerHour       :: !(Maybe Text)
  , _rprdEstimatedReservationCostForLookbackPeriod :: !(Maybe Text)
  , _rprdInstanceDetails                           :: !(Maybe InstanceDetails)
  , _rprdAverageNumberOfInstancesUsedPerHour       :: !(Maybe Text)
  , _rprdMinimumNumberOfInstancesUsedPerHour       :: !(Maybe Text)
  , _rprdEstimatedBreakEvenInMonths                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationPurchaseRecommendationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprdMaximumNormalizedUnitsUsedPerHour' - The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdRecurringStandardMonthlyCost' - How much purchasing this instance costs you on a monthly basis.
--
-- * 'rprdAverageNormalizedUnitsUsedPerHour' - The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdCurrencyCode' - The currency code that AWS used to calculate the costs for this instance.
--
-- * 'rprdEstimatedMonthlySavingsPercentage' - How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
--
-- * 'rprdRecommendedNormalizedUnitsToPurchase' - The number of normalized units that AWS recommends that you purchase.
--
-- * 'rprdAverageUtilization' - The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedMonthlySavingsAmount' - How much AWS estimates that this specific recommendation could save you in a month.
--
-- * 'rprdUpfrontCost' - How much purchasing this instance costs you upfront.
--
-- * 'rprdMinimumNormalizedUnitsUsedPerHour' - The minimum number of hours that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedMonthlyOnDemandCost' - How much AWS estimates that you spend on On-Demand Instances in a month.
--
-- * 'rprdRecommendedNumberOfInstancesToPurchase' - The number of instances that AWS recommends that you purchase.
--
-- * 'rprdMaximumNumberOfInstancesUsedPerHour' - The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedReservationCostForLookbackPeriod' - How much AWS estimates that you would have spent for all usage during the specified historical period if you had had a reservation.
--
-- * 'rprdInstanceDetails' - Details about the instances that AWS recommends that you purchase.
--
-- * 'rprdAverageNumberOfInstancesUsedPerHour' - The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdMinimumNumberOfInstancesUsedPerHour' - The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
--
-- * 'rprdEstimatedBreakEvenInMonths' - How long AWS estimates that it takes for this instance to start saving you money, in months.
reservationPurchaseRecommendationDetail
    :: ReservationPurchaseRecommendationDetail
reservationPurchaseRecommendationDetail =
  ReservationPurchaseRecommendationDetail'
    { _rprdMaximumNormalizedUnitsUsedPerHour = Nothing
    , _rprdRecurringStandardMonthlyCost = Nothing
    , _rprdAverageNormalizedUnitsUsedPerHour = Nothing
    , _rprdCurrencyCode = Nothing
    , _rprdEstimatedMonthlySavingsPercentage = Nothing
    , _rprdRecommendedNormalizedUnitsToPurchase = Nothing
    , _rprdAverageUtilization = Nothing
    , _rprdEstimatedMonthlySavingsAmount = Nothing
    , _rprdUpfrontCost = Nothing
    , _rprdMinimumNormalizedUnitsUsedPerHour = Nothing
    , _rprdEstimatedMonthlyOnDemandCost = Nothing
    , _rprdRecommendedNumberOfInstancesToPurchase = Nothing
    , _rprdMaximumNumberOfInstancesUsedPerHour = Nothing
    , _rprdEstimatedReservationCostForLookbackPeriod = Nothing
    , _rprdInstanceDetails = Nothing
    , _rprdAverageNumberOfInstancesUsedPerHour = Nothing
    , _rprdMinimumNumberOfInstancesUsedPerHour = Nothing
    , _rprdEstimatedBreakEvenInMonths = Nothing
    }


-- | The maximum number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMaximumNormalizedUnitsUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMaximumNormalizedUnitsUsedPerHour = lens _rprdMaximumNormalizedUnitsUsedPerHour (\ s a -> s{_rprdMaximumNormalizedUnitsUsedPerHour = a})

-- | How much purchasing this instance costs you on a monthly basis.
rprdRecurringStandardMonthlyCost :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdRecurringStandardMonthlyCost = lens _rprdRecurringStandardMonthlyCost (\ s a -> s{_rprdRecurringStandardMonthlyCost = a})

-- | The average number of normalized units that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdAverageNormalizedUnitsUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAverageNormalizedUnitsUsedPerHour = lens _rprdAverageNormalizedUnitsUsedPerHour (\ s a -> s{_rprdAverageNormalizedUnitsUsedPerHour = a})

-- | The currency code that AWS used to calculate the costs for this instance.
rprdCurrencyCode :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdCurrencyCode = lens _rprdCurrencyCode (\ s a -> s{_rprdCurrencyCode = a})

-- | How much AWS estimates that this specific recommendation could save you in a month, as a percentage of your overall costs.
rprdEstimatedMonthlySavingsPercentage :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedMonthlySavingsPercentage = lens _rprdEstimatedMonthlySavingsPercentage (\ s a -> s{_rprdEstimatedMonthlySavingsPercentage = a})

-- | The number of normalized units that AWS recommends that you purchase.
rprdRecommendedNormalizedUnitsToPurchase :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdRecommendedNormalizedUnitsToPurchase = lens _rprdRecommendedNormalizedUnitsToPurchase (\ s a -> s{_rprdRecommendedNormalizedUnitsToPurchase = a})

-- | The average utilization of your instances. AWS uses this to calculate your recommended reservation purchases.
rprdAverageUtilization :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAverageUtilization = lens _rprdAverageUtilization (\ s a -> s{_rprdAverageUtilization = a})

-- | How much AWS estimates that this specific recommendation could save you in a month.
rprdEstimatedMonthlySavingsAmount :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedMonthlySavingsAmount = lens _rprdEstimatedMonthlySavingsAmount (\ s a -> s{_rprdEstimatedMonthlySavingsAmount = a})

-- | How much purchasing this instance costs you upfront.
rprdUpfrontCost :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdUpfrontCost = lens _rprdUpfrontCost (\ s a -> s{_rprdUpfrontCost = a})

-- | The minimum number of hours that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMinimumNormalizedUnitsUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMinimumNormalizedUnitsUsedPerHour = lens _rprdMinimumNormalizedUnitsUsedPerHour (\ s a -> s{_rprdMinimumNormalizedUnitsUsedPerHour = a})

-- | How much AWS estimates that you spend on On-Demand Instances in a month.
rprdEstimatedMonthlyOnDemandCost :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedMonthlyOnDemandCost = lens _rprdEstimatedMonthlyOnDemandCost (\ s a -> s{_rprdEstimatedMonthlyOnDemandCost = a})

-- | The number of instances that AWS recommends that you purchase.
rprdRecommendedNumberOfInstancesToPurchase :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdRecommendedNumberOfInstancesToPurchase = lens _rprdRecommendedNumberOfInstancesToPurchase (\ s a -> s{_rprdRecommendedNumberOfInstancesToPurchase = a})

-- | The maximum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMaximumNumberOfInstancesUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMaximumNumberOfInstancesUsedPerHour = lens _rprdMaximumNumberOfInstancesUsedPerHour (\ s a -> s{_rprdMaximumNumberOfInstancesUsedPerHour = a})

-- | How much AWS estimates that you would have spent for all usage during the specified historical period if you had had a reservation.
rprdEstimatedReservationCostForLookbackPeriod :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedReservationCostForLookbackPeriod = lens _rprdEstimatedReservationCostForLookbackPeriod (\ s a -> s{_rprdEstimatedReservationCostForLookbackPeriod = a})

-- | Details about the instances that AWS recommends that you purchase.
rprdInstanceDetails :: Lens' ReservationPurchaseRecommendationDetail (Maybe InstanceDetails)
rprdInstanceDetails = lens _rprdInstanceDetails (\ s a -> s{_rprdInstanceDetails = a})

-- | The average number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdAverageNumberOfInstancesUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdAverageNumberOfInstancesUsedPerHour = lens _rprdAverageNumberOfInstancesUsedPerHour (\ s a -> s{_rprdAverageNumberOfInstancesUsedPerHour = a})

-- | The minimum number of instances that you used in an hour during the historical period. AWS uses this to calculate your recommended reservation purchases.
rprdMinimumNumberOfInstancesUsedPerHour :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdMinimumNumberOfInstancesUsedPerHour = lens _rprdMinimumNumberOfInstancesUsedPerHour (\ s a -> s{_rprdMinimumNumberOfInstancesUsedPerHour = a})

-- | How long AWS estimates that it takes for this instance to start saving you money, in months.
rprdEstimatedBreakEvenInMonths :: Lens' ReservationPurchaseRecommendationDetail (Maybe Text)
rprdEstimatedBreakEvenInMonths = lens _rprdEstimatedBreakEvenInMonths (\ s a -> s{_rprdEstimatedBreakEvenInMonths = a})

instance FromJSON
           ReservationPurchaseRecommendationDetail
         where
        parseJSON
          = withObject
              "ReservationPurchaseRecommendationDetail"
              (\ x ->
                 ReservationPurchaseRecommendationDetail' <$>
                   (x .:? "MaximumNormalizedUnitsUsedPerHour") <*>
                     (x .:? "RecurringStandardMonthlyCost")
                     <*> (x .:? "AverageNormalizedUnitsUsedPerHour")
                     <*> (x .:? "CurrencyCode")
                     <*> (x .:? "EstimatedMonthlySavingsPercentage")
                     <*> (x .:? "RecommendedNormalizedUnitsToPurchase")
                     <*> (x .:? "AverageUtilization")
                     <*> (x .:? "EstimatedMonthlySavingsAmount")
                     <*> (x .:? "UpfrontCost")
                     <*> (x .:? "MinimumNormalizedUnitsUsedPerHour")
                     <*> (x .:? "EstimatedMonthlyOnDemandCost")
                     <*> (x .:? "RecommendedNumberOfInstancesToPurchase")
                     <*> (x .:? "MaximumNumberOfInstancesUsedPerHour")
                     <*>
                     (x .:? "EstimatedReservationCostForLookbackPeriod")
                     <*> (x .:? "InstanceDetails")
                     <*> (x .:? "AverageNumberOfInstancesUsedPerHour")
                     <*> (x .:? "MinimumNumberOfInstancesUsedPerHour")
                     <*> (x .:? "EstimatedBreakEvenInMonths"))

instance Hashable
           ReservationPurchaseRecommendationDetail
         where

instance NFData
           ReservationPurchaseRecommendationDetail
         where

-- | Information about this specific recommendation, such as the time stamp for when AWS made a specific recommendation.
--
--
--
-- /See:/ 'reservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { _rprmRecommendationId    :: !(Maybe Text)
  , _rprmGenerationTimestamp :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationPurchaseRecommendationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprmRecommendationId' - The ID for this specific recommendation.
--
-- * 'rprmGenerationTimestamp' - The time stamp for when AWS made this recommendation.
reservationPurchaseRecommendationMetadata
    :: ReservationPurchaseRecommendationMetadata
reservationPurchaseRecommendationMetadata =
  ReservationPurchaseRecommendationMetadata'
    {_rprmRecommendationId = Nothing, _rprmGenerationTimestamp = Nothing}


-- | The ID for this specific recommendation.
rprmRecommendationId :: Lens' ReservationPurchaseRecommendationMetadata (Maybe Text)
rprmRecommendationId = lens _rprmRecommendationId (\ s a -> s{_rprmRecommendationId = a})

-- | The time stamp for when AWS made this recommendation.
rprmGenerationTimestamp :: Lens' ReservationPurchaseRecommendationMetadata (Maybe Text)
rprmGenerationTimestamp = lens _rprmGenerationTimestamp (\ s a -> s{_rprmGenerationTimestamp = a})

instance FromJSON
           ReservationPurchaseRecommendationMetadata
         where
        parseJSON
          = withObject
              "ReservationPurchaseRecommendationMetadata"
              (\ x ->
                 ReservationPurchaseRecommendationMetadata' <$>
                   (x .:? "RecommendationId") <*>
                     (x .:? "GenerationTimestamp"))

instance Hashable
           ReservationPurchaseRecommendationMetadata
         where

instance NFData
           ReservationPurchaseRecommendationMetadata
         where

-- | A summary about this recommendation, such as the currency code, the amount that AWS estimates you could save, and the total amount of reservation to purchase.
--
--
--
-- /See:/ 'reservationPurchaseRecommendationSummary' smart constructor.
data ReservationPurchaseRecommendationSummary = ReservationPurchaseRecommendationSummary'
  { _rprsCurrencyCode                           :: !(Maybe Text)
  , _rprsTotalEstimatedMonthlySavingsPercentage :: !(Maybe Text)
  , _rprsTotalEstimatedMonthlySavingsAmount     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationPurchaseRecommendationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprsCurrencyCode' - The currency code used for this recommendation.
--
-- * 'rprsTotalEstimatedMonthlySavingsPercentage' - The total amount that AWS estimates that this recommendation could save you in a month, as a percentage of your costs.
--
-- * 'rprsTotalEstimatedMonthlySavingsAmount' - The total amount that AWS estimates that this recommendation could save you in a month.
reservationPurchaseRecommendationSummary
    :: ReservationPurchaseRecommendationSummary
reservationPurchaseRecommendationSummary =
  ReservationPurchaseRecommendationSummary'
    { _rprsCurrencyCode = Nothing
    , _rprsTotalEstimatedMonthlySavingsPercentage = Nothing
    , _rprsTotalEstimatedMonthlySavingsAmount = Nothing
    }


-- | The currency code used for this recommendation.
rprsCurrencyCode :: Lens' ReservationPurchaseRecommendationSummary (Maybe Text)
rprsCurrencyCode = lens _rprsCurrencyCode (\ s a -> s{_rprsCurrencyCode = a})

-- | The total amount that AWS estimates that this recommendation could save you in a month, as a percentage of your costs.
rprsTotalEstimatedMonthlySavingsPercentage :: Lens' ReservationPurchaseRecommendationSummary (Maybe Text)
rprsTotalEstimatedMonthlySavingsPercentage = lens _rprsTotalEstimatedMonthlySavingsPercentage (\ s a -> s{_rprsTotalEstimatedMonthlySavingsPercentage = a})

-- | The total amount that AWS estimates that this recommendation could save you in a month.
rprsTotalEstimatedMonthlySavingsAmount :: Lens' ReservationPurchaseRecommendationSummary (Maybe Text)
rprsTotalEstimatedMonthlySavingsAmount = lens _rprsTotalEstimatedMonthlySavingsAmount (\ s a -> s{_rprsTotalEstimatedMonthlySavingsAmount = a})

instance FromJSON
           ReservationPurchaseRecommendationSummary
         where
        parseJSON
          = withObject
              "ReservationPurchaseRecommendationSummary"
              (\ x ->
                 ReservationPurchaseRecommendationSummary' <$>
                   (x .:? "CurrencyCode") <*>
                     (x .:? "TotalEstimatedMonthlySavingsPercentage")
                     <*> (x .:? "TotalEstimatedMonthlySavingsAmount"))

instance Hashable
           ReservationPurchaseRecommendationSummary
         where

instance NFData
           ReservationPurchaseRecommendationSummary
         where

-- | A group of RIs that share a set of attributes.
--
--
--
-- /See:/ 'reservationUtilizationGroup' smart constructor.
data ReservationUtilizationGroup = ReservationUtilizationGroup'
  { _rugValue       :: !(Maybe Text)
  , _rugKey         :: !(Maybe Text)
  , _rugAttributes  :: !(Maybe (Map Text Text))
  , _rugUtilization :: !(Maybe ReservationAggregates)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationUtilizationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rugValue' - The value of a specific RI attribute.
--
-- * 'rugKey' - The key for a specific RI attribute.
--
-- * 'rugAttributes' - The attributes for this group of RIs.
--
-- * 'rugUtilization' - How much you used this group of RIs.
reservationUtilizationGroup
    :: ReservationUtilizationGroup
reservationUtilizationGroup =
  ReservationUtilizationGroup'
    { _rugValue = Nothing
    , _rugKey = Nothing
    , _rugAttributes = Nothing
    , _rugUtilization = Nothing
    }


-- | The value of a specific RI attribute.
rugValue :: Lens' ReservationUtilizationGroup (Maybe Text)
rugValue = lens _rugValue (\ s a -> s{_rugValue = a})

-- | The key for a specific RI attribute.
rugKey :: Lens' ReservationUtilizationGroup (Maybe Text)
rugKey = lens _rugKey (\ s a -> s{_rugKey = a})

-- | The attributes for this group of RIs.
rugAttributes :: Lens' ReservationUtilizationGroup (HashMap Text Text)
rugAttributes = lens _rugAttributes (\ s a -> s{_rugAttributes = a}) . _Default . _Map

-- | How much you used this group of RIs.
rugUtilization :: Lens' ReservationUtilizationGroup (Maybe ReservationAggregates)
rugUtilization = lens _rugUtilization (\ s a -> s{_rugUtilization = a})

instance FromJSON ReservationUtilizationGroup where
        parseJSON
          = withObject "ReservationUtilizationGroup"
              (\ x ->
                 ReservationUtilizationGroup' <$>
                   (x .:? "Value") <*> (x .:? "Key") <*>
                     (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "Utilization"))

instance Hashable ReservationUtilizationGroup where

instance NFData ReservationUtilizationGroup where

-- | The result that is associated with a time period.
--
--
--
-- /See:/ 'resultByTime' smart constructor.
data ResultByTime = ResultByTime'
  { _rbtGroups     :: !(Maybe [Group])
  , _rbtTimePeriod :: !(Maybe DateInterval)
  , _rbtTotal      :: !(Maybe (Map Text MetricValue))
  , _rbtEstimated  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rbtGroups' - The groups that are included in this time period.
--
-- * 'rbtTimePeriod' - The time period covered by a result.
--
-- * 'rbtTotal' - The total amount of cost or usage accrued during the time period.
--
-- * 'rbtEstimated' - Whether this result is estimated.
resultByTime
    :: ResultByTime
resultByTime =
  ResultByTime'
    { _rbtGroups = Nothing
    , _rbtTimePeriod = Nothing
    , _rbtTotal = Nothing
    , _rbtEstimated = Nothing
    }


-- | The groups that are included in this time period.
rbtGroups :: Lens' ResultByTime [Group]
rbtGroups = lens _rbtGroups (\ s a -> s{_rbtGroups = a}) . _Default . _Coerce

-- | The time period covered by a result.
rbtTimePeriod :: Lens' ResultByTime (Maybe DateInterval)
rbtTimePeriod = lens _rbtTimePeriod (\ s a -> s{_rbtTimePeriod = a})

-- | The total amount of cost or usage accrued during the time period.
rbtTotal :: Lens' ResultByTime (HashMap Text MetricValue)
rbtTotal = lens _rbtTotal (\ s a -> s{_rbtTotal = a}) . _Default . _Map

-- | Whether this result is estimated.
rbtEstimated :: Lens' ResultByTime (Maybe Bool)
rbtEstimated = lens _rbtEstimated (\ s a -> s{_rbtEstimated = a})

instance FromJSON ResultByTime where
        parseJSON
          = withObject "ResultByTime"
              (\ x ->
                 ResultByTime' <$>
                   (x .:? "Groups" .!= mempty) <*> (x .:? "TimePeriod")
                     <*> (x .:? "Total" .!= mempty)
                     <*> (x .:? "Estimated"))

instance Hashable ResultByTime where

instance NFData ResultByTime where

-- | Hardware specifications for the service that you want recommendations for.
--
--
--
-- /See:/ 'serviceSpecification' smart constructor.
newtype ServiceSpecification = ServiceSpecification'
  { _ssEC2Specification :: Maybe EC2Specification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssEC2Specification' - The EC2 hardware specifications that you want AWS to provide recommendations for.
serviceSpecification
    :: ServiceSpecification
serviceSpecification = ServiceSpecification' {_ssEC2Specification = Nothing}


-- | The EC2 hardware specifications that you want AWS to provide recommendations for.
ssEC2Specification :: Lens' ServiceSpecification (Maybe EC2Specification)
ssEC2Specification = lens _ssEC2Specification (\ s a -> s{_ssEC2Specification = a})

instance FromJSON ServiceSpecification where
        parseJSON
          = withObject "ServiceSpecification"
              (\ x ->
                 ServiceSpecification' <$> (x .:? "EC2Specification"))

instance Hashable ServiceSpecification where

instance NFData ServiceSpecification where

instance ToJSON ServiceSpecification where
        toJSON ServiceSpecification'{..}
          = object
              (catMaybes
                 [("EC2Specification" .=) <$> _ssEC2Specification])

-- | The values that are available for a tag.
--
--
--
-- /See:/ 'tagValues' smart constructor.
data TagValues = TagValues'
  { _tvValues :: !(Maybe [Text])
  , _tvKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvValues' - The specific value of a tag.
--
-- * 'tvKey' - The key for a tag.
tagValues
    :: TagValues
tagValues = TagValues' {_tvValues = Nothing, _tvKey = Nothing}


-- | The specific value of a tag.
tvValues :: Lens' TagValues [Text]
tvValues = lens _tvValues (\ s a -> s{_tvValues = a}) . _Default . _Coerce

-- | The key for a tag.
tvKey :: Lens' TagValues (Maybe Text)
tvKey = lens _tvKey (\ s a -> s{_tvKey = a})

instance Hashable TagValues where

instance NFData TagValues where

instance ToJSON TagValues where
        toJSON TagValues'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _tvValues, ("Key" .=) <$> _tvKey])

-- | The amount of utilization, in hours.
--
--
--
-- /See:/ 'utilizationByTime' smart constructor.
data UtilizationByTime = UtilizationByTime'
  { _ubtGroups     :: !(Maybe [ReservationUtilizationGroup])
  , _ubtTimePeriod :: !(Maybe DateInterval)
  , _ubtTotal      :: !(Maybe ReservationAggregates)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UtilizationByTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubtGroups' - The groups that are included in this utilization result.
--
-- * 'ubtTimePeriod' - The period of time over which this utilization was used.
--
-- * 'ubtTotal' - The total number of RI hours that were used.
utilizationByTime
    :: UtilizationByTime
utilizationByTime =
  UtilizationByTime'
    {_ubtGroups = Nothing, _ubtTimePeriod = Nothing, _ubtTotal = Nothing}


-- | The groups that are included in this utilization result.
ubtGroups :: Lens' UtilizationByTime [ReservationUtilizationGroup]
ubtGroups = lens _ubtGroups (\ s a -> s{_ubtGroups = a}) . _Default . _Coerce

-- | The period of time over which this utilization was used.
ubtTimePeriod :: Lens' UtilizationByTime (Maybe DateInterval)
ubtTimePeriod = lens _ubtTimePeriod (\ s a -> s{_ubtTimePeriod = a})

-- | The total number of RI hours that were used.
ubtTotal :: Lens' UtilizationByTime (Maybe ReservationAggregates)
ubtTotal = lens _ubtTotal (\ s a -> s{_ubtTotal = a})

instance FromJSON UtilizationByTime where
        parseJSON
          = withObject "UtilizationByTime"
              (\ x ->
                 UtilizationByTime' <$>
                   (x .:? "Groups" .!= mempty) <*> (x .:? "TimePeriod")
                     <*> (x .:? "Total"))

instance Hashable UtilizationByTime where

instance NFData UtilizationByTime where
