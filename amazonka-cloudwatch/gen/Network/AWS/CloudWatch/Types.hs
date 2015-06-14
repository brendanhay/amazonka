{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudWatch.Types
    (
    -- * Service
      CloudWatch
    -- ** Errors
    , RESTError

    -- * AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiTimestamp
    , ahiHistorySummary

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * Datapoint
    , Datapoint
    , datapoint
    , datSampleCount
    , datMaximum
    , datAverage
    , datMinimum
    , datSum
    , datTimestamp
    , datUnit

    -- * Dimension
    , Dimension
    , dimension
    , dimName
    , dimValue

    -- * DimensionFilter
    , DimensionFilter
    , dimensionFilter
    , dfValue
    , dfName

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * Metric
    , Metric
    , metric
    , metMetricName
    , metNamespace
    , metDimensions

    -- * MetricAlarm
    , MetricAlarm
    , metricAlarm
    , maAlarmName
    , maStateUpdatedTimestamp
    , maAlarmDescription
    , maPeriod
    , maEvaluationPeriods
    , maMetricName
    , maNamespace
    , maOKActions
    , maComparisonOperator
    , maStateValue
    , maThreshold
    , maActionsEnabled
    , maAlarmConfigurationUpdatedTimestamp
    , maInsufficientDataActions
    , maDimensions
    , maStateReasonData
    , maStateReason
    , maAlarmARN
    , maAlarmActions
    , maStatistic
    , maUnit

    -- * MetricDatum
    , MetricDatum
    , metricDatum
    , mdValue
    , mdDimensions
    , mdTimestamp
    , mdStatisticValues
    , mdUnit
    , mdMetricName

    -- * StandardUnit
    , StandardUnit (..)

    -- * StateValue
    , StateValue (..)

    -- * Statistic
    , Statistic (..)

    -- * StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2010-08-01@ of the Amazon CloudWatch SDK.
data CloudWatch

instance AWSService CloudWatch where
    type Sg CloudWatch = V4
    type Er CloudWatch = RESTError

    service = service'
      where
        service' :: Service CloudWatch
        service' = Service
            { _svcAbbrev  = "CloudWatch"
            , _svcPrefix  = "monitoring"
            , _svcVersion = "2010-08-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry CloudWatch
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'alarmHistoryItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ahiAlarmName'
--
-- * 'ahiHistoryItemType'
--
-- * 'ahiHistoryData'
--
-- * 'ahiTimestamp'
--
-- * 'ahiHistorySummary'
data AlarmHistoryItem = AlarmHistoryItem'{_ahiAlarmName :: Maybe Text, _ahiHistoryItemType :: Maybe HistoryItemType, _ahiHistoryData :: Maybe Text, _ahiTimestamp :: Maybe ISO8601, _ahiHistorySummary :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AlarmHistoryItem' smart constructor.
alarmHistoryItem :: AlarmHistoryItem
alarmHistoryItem = AlarmHistoryItem'{_ahiAlarmName = Nothing, _ahiHistoryItemType = Nothing, _ahiHistoryData = Nothing, _ahiTimestamp = Nothing, _ahiHistorySummary = Nothing};

-- | The descriptive name for the alarm.
ahiAlarmName :: Lens' AlarmHistoryItem (Maybe Text)
ahiAlarmName = lens _ahiAlarmName (\ s a -> s{_ahiAlarmName = a});

-- | The type of alarm history item.
ahiHistoryItemType :: Lens' AlarmHistoryItem (Maybe HistoryItemType)
ahiHistoryItemType = lens _ahiHistoryItemType (\ s a -> s{_ahiHistoryItemType = a});

-- | Machine-readable data about the alarm in JSON format.
ahiHistoryData :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistoryData = lens _ahiHistoryData (\ s a -> s{_ahiHistoryData = a});

-- | The time stamp for the alarm history item. Amazon CloudWatch uses
-- Coordinated Universal Time (UTC) when returning time stamps, which do
-- not accommodate seasonal adjustments such as daylight savings time. For
-- more information, see
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps>
-- in the /Amazon CloudWatch Developer Guide/.
ahiTimestamp :: Lens' AlarmHistoryItem (Maybe UTCTime)
ahiTimestamp = lens _ahiTimestamp (\ s a -> s{_ahiTimestamp = a}) . mapping _Time;

-- | A human-readable summary of the alarm history.
ahiHistorySummary :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistorySummary = lens _ahiHistorySummary (\ s a -> s{_ahiHistorySummary = a});

instance FromXML AlarmHistoryItem where
        parseXML x
          = AlarmHistoryItem' <$>
              x .@? "AlarmName" <*> x .@? "HistoryItemType" <*>
                x .@? "HistoryData"
                <*> x .@? "Timestamp"
                <*> x .@? "HistorySummary"

data ComparisonOperator = GreaterThanOrEqualToThreshold | GreaterThanThreshold | LessThanOrEqualToThreshold | LessThanThreshold deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "GreaterThanOrEqualToThreshold" -> pure GreaterThanOrEqualToThreshold
        "GreaterThanThreshold" -> pure GreaterThanThreshold
        "LessThanOrEqualToThreshold" -> pure LessThanOrEqualToThreshold
        "LessThanThreshold" -> pure LessThanThreshold
        e -> fail ("Failure parsing ComparisonOperator from " ++ show e)

instance ToText ComparisonOperator where
    toText = \case
        GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
        GreaterThanThreshold -> "GreaterThanThreshold"
        LessThanOrEqualToThreshold -> "LessThanOrEqualToThreshold"
        LessThanThreshold -> "LessThanThreshold"

instance Hashable ComparisonOperator
instance ToQuery ComparisonOperator
instance ToHeader ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"

-- | /See:/ 'datapoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datSampleCount'
--
-- * 'datMaximum'
--
-- * 'datAverage'
--
-- * 'datMinimum'
--
-- * 'datSum'
--
-- * 'datTimestamp'
--
-- * 'datUnit'
data Datapoint = Datapoint'{_datSampleCount :: Maybe Double, _datMaximum :: Maybe Double, _datAverage :: Maybe Double, _datMinimum :: Maybe Double, _datSum :: Maybe Double, _datTimestamp :: Maybe ISO8601, _datUnit :: Maybe StandardUnit} deriving (Eq, Read, Show)

-- | 'Datapoint' smart constructor.
datapoint :: Datapoint
datapoint = Datapoint'{_datSampleCount = Nothing, _datMaximum = Nothing, _datAverage = Nothing, _datMinimum = Nothing, _datSum = Nothing, _datTimestamp = Nothing, _datUnit = Nothing};

-- | The number of metric values that contributed to the aggregate value of
-- this datapoint.
datSampleCount :: Lens' Datapoint (Maybe Double)
datSampleCount = lens _datSampleCount (\ s a -> s{_datSampleCount = a});

-- | The maximum of the metric value used for the datapoint.
datMaximum :: Lens' Datapoint (Maybe Double)
datMaximum = lens _datMaximum (\ s a -> s{_datMaximum = a});

-- | The average of metric values that correspond to the datapoint.
datAverage :: Lens' Datapoint (Maybe Double)
datAverage = lens _datAverage (\ s a -> s{_datAverage = a});

-- | The minimum metric value used for the datapoint.
datMinimum :: Lens' Datapoint (Maybe Double)
datMinimum = lens _datMinimum (\ s a -> s{_datMinimum = a});

-- | The sum of metric values used for the datapoint.
datSum :: Lens' Datapoint (Maybe Double)
datSum = lens _datSum (\ s a -> s{_datSum = a});

-- | The time stamp used for the datapoint. Amazon CloudWatch uses
-- Coordinated Universal Time (UTC) when returning time stamps, which do
-- not accommodate seasonal adjustments such as daylight savings time. For
-- more information, see
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps>
-- in the /Amazon CloudWatch Developer Guide/.
datTimestamp :: Lens' Datapoint (Maybe UTCTime)
datTimestamp = lens _datTimestamp (\ s a -> s{_datTimestamp = a}) . mapping _Time;

-- | The standard unit used for the datapoint.
datUnit :: Lens' Datapoint (Maybe StandardUnit)
datUnit = lens _datUnit (\ s a -> s{_datUnit = a});

instance FromXML Datapoint where
        parseXML x
          = Datapoint' <$>
              x .@? "SampleCount" <*> x .@? "Maximum" <*>
                x .@? "Average"
                <*> x .@? "Minimum"
                <*> x .@? "Sum"
                <*> x .@? "Timestamp"
                <*> x .@? "Unit"

-- | /See:/ 'dimension' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dimName'
--
-- * 'dimValue'
data Dimension = Dimension'{_dimName :: Text, _dimValue :: Text} deriving (Eq, Read, Show)

-- | 'Dimension' smart constructor.
dimension :: Text -> Text -> Dimension
dimension pName pValue = Dimension'{_dimName = pName, _dimValue = pValue};

-- | The name of the dimension.
dimName :: Lens' Dimension Text
dimName = lens _dimName (\ s a -> s{_dimName = a});

-- | The value representing the dimension measurement
dimValue :: Lens' Dimension Text
dimValue = lens _dimValue (\ s a -> s{_dimValue = a});

instance FromXML Dimension where
        parseXML x
          = Dimension' <$> x .@ "Name" <*> x .@ "Value"

instance ToQuery Dimension where
        toQuery Dimension'{..}
          = mconcat ["Name" =: _dimName, "Value" =: _dimValue]

-- | /See:/ 'dimensionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfValue'
--
-- * 'dfName'
data DimensionFilter = DimensionFilter'{_dfValue :: Maybe Text, _dfName :: Text} deriving (Eq, Read, Show)

-- | 'DimensionFilter' smart constructor.
dimensionFilter :: Text -> DimensionFilter
dimensionFilter pName = DimensionFilter'{_dfValue = Nothing, _dfName = pName};

-- | The value of the dimension to be matched.
dfValue :: Lens' DimensionFilter (Maybe Text)
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\ s a -> s{_dfName = a});

instance ToQuery DimensionFilter where
        toQuery DimensionFilter'{..}
          = mconcat ["Value" =: _dfValue, "Name" =: _dfName]

data HistoryItemType = StateUpdate | Action | ConfigurationUpdate deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText HistoryItemType where
    parser = takeLowerText >>= \case
        "Action" -> pure Action
        "ConfigurationUpdate" -> pure ConfigurationUpdate
        "StateUpdate" -> pure StateUpdate
        e -> fail ("Failure parsing HistoryItemType from " ++ show e)

instance ToText HistoryItemType where
    toText = \case
        Action -> "Action"
        ConfigurationUpdate -> "ConfigurationUpdate"
        StateUpdate -> "StateUpdate"

instance Hashable HistoryItemType
instance ToQuery HistoryItemType
instance ToHeader HistoryItemType

instance FromXML HistoryItemType where
    parseXML = parseXMLText "HistoryItemType"

-- | /See:/ 'metric' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'metMetricName'
--
-- * 'metNamespace'
--
-- * 'metDimensions'
data Metric = Metric'{_metMetricName :: Maybe Text, _metNamespace :: Maybe Text, _metDimensions :: Maybe [Dimension]} deriving (Eq, Read, Show)

-- | 'Metric' smart constructor.
metric :: Metric
metric = Metric'{_metMetricName = Nothing, _metNamespace = Nothing, _metDimensions = Nothing};

-- | The name of the metric.
metMetricName :: Lens' Metric (Maybe Text)
metMetricName = lens _metMetricName (\ s a -> s{_metMetricName = a});

-- | The namespace of the metric.
metNamespace :: Lens' Metric (Maybe Text)
metNamespace = lens _metNamespace (\ s a -> s{_metNamespace = a});

-- | A list of dimensions associated with the metric.
metDimensions :: Lens' Metric (Maybe [Dimension])
metDimensions = lens _metDimensions (\ s a -> s{_metDimensions = a});

instance FromXML Metric where
        parseXML x
          = Metric' <$>
              x .@? "MetricName" <*> x .@? "Namespace" <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   parseXMLList "member")

-- | /See:/ 'metricAlarm' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'maAlarmName'
--
-- * 'maStateUpdatedTimestamp'
--
-- * 'maAlarmDescription'
--
-- * 'maPeriod'
--
-- * 'maEvaluationPeriods'
--
-- * 'maMetricName'
--
-- * 'maNamespace'
--
-- * 'maOKActions'
--
-- * 'maComparisonOperator'
--
-- * 'maStateValue'
--
-- * 'maThreshold'
--
-- * 'maActionsEnabled'
--
-- * 'maAlarmConfigurationUpdatedTimestamp'
--
-- * 'maInsufficientDataActions'
--
-- * 'maDimensions'
--
-- * 'maStateReasonData'
--
-- * 'maStateReason'
--
-- * 'maAlarmARN'
--
-- * 'maAlarmActions'
--
-- * 'maStatistic'
--
-- * 'maUnit'
data MetricAlarm = MetricAlarm'{_maAlarmName :: Maybe Text, _maStateUpdatedTimestamp :: Maybe ISO8601, _maAlarmDescription :: Maybe Text, _maPeriod :: Maybe Nat, _maEvaluationPeriods :: Maybe Nat, _maMetricName :: Maybe Text, _maNamespace :: Maybe Text, _maOKActions :: Maybe [Text], _maComparisonOperator :: Maybe ComparisonOperator, _maStateValue :: Maybe StateValue, _maThreshold :: Maybe Double, _maActionsEnabled :: Maybe Bool, _maAlarmConfigurationUpdatedTimestamp :: Maybe ISO8601, _maInsufficientDataActions :: Maybe [Text], _maDimensions :: Maybe [Dimension], _maStateReasonData :: Maybe Text, _maStateReason :: Maybe Text, _maAlarmARN :: Maybe Text, _maAlarmActions :: Maybe [Text], _maStatistic :: Maybe Statistic, _maUnit :: Maybe StandardUnit} deriving (Eq, Read, Show)

-- | 'MetricAlarm' smart constructor.
metricAlarm :: MetricAlarm
metricAlarm = MetricAlarm'{_maAlarmName = Nothing, _maStateUpdatedTimestamp = Nothing, _maAlarmDescription = Nothing, _maPeriod = Nothing, _maEvaluationPeriods = Nothing, _maMetricName = Nothing, _maNamespace = Nothing, _maOKActions = Nothing, _maComparisonOperator = Nothing, _maStateValue = Nothing, _maThreshold = Nothing, _maActionsEnabled = Nothing, _maAlarmConfigurationUpdatedTimestamp = Nothing, _maInsufficientDataActions = Nothing, _maDimensions = Nothing, _maStateReasonData = Nothing, _maStateReason = Nothing, _maAlarmARN = Nothing, _maAlarmActions = Nothing, _maStatistic = Nothing, _maUnit = Nothing};

-- | The name of the alarm.
maAlarmName :: Lens' MetricAlarm (Maybe Text)
maAlarmName = lens _maAlarmName (\ s a -> s{_maAlarmName = a});

-- | The time stamp of the last update to the alarm\'s state. Amazon
-- CloudWatch uses Coordinated Universal Time (UTC) when returning time
-- stamps, which do not accommodate seasonal adjustments such as daylight
-- savings time. For more information, see
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps>
-- in the /Amazon CloudWatch Developer Guide/.
maStateUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maStateUpdatedTimestamp = lens _maStateUpdatedTimestamp (\ s a -> s{_maStateUpdatedTimestamp = a}) . mapping _Time;

-- | The description for the alarm.
maAlarmDescription :: Lens' MetricAlarm (Maybe Text)
maAlarmDescription = lens _maAlarmDescription (\ s a -> s{_maAlarmDescription = a});

-- | The period in seconds over which the statistic is applied.
maPeriod :: Lens' MetricAlarm (Maybe Natural)
maPeriod = lens _maPeriod (\ s a -> s{_maPeriod = a}) . mapping _Nat;

-- | The number of periods over which data is compared to the specified
-- threshold.
maEvaluationPeriods :: Lens' MetricAlarm (Maybe Natural)
maEvaluationPeriods = lens _maEvaluationPeriods (\ s a -> s{_maEvaluationPeriods = a}) . mapping _Nat;

-- | The name of the alarm\'s metric.
maMetricName :: Lens' MetricAlarm (Maybe Text)
maMetricName = lens _maMetricName (\ s a -> s{_maMetricName = a});

-- | The namespace of alarm\'s associated metric.
maNamespace :: Lens' MetricAlarm (Maybe Text)
maNamespace = lens _maNamespace (\ s a -> s{_maNamespace = a});

-- | The list of actions to execute when this alarm transitions into an @OK@
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only actions supported are
-- publishing to an Amazon SNS topic and triggering an Auto Scaling policy.
maOKActions :: Lens' MetricAlarm (Maybe [Text])
maOKActions = lens _maOKActions (\ s a -> s{_maOKActions = a});

-- | The arithmetic operation to use when comparing the specified @Statistic@
-- and @Threshold@. The specified @Statistic@ value is used as the first
-- operand.
maComparisonOperator :: Lens' MetricAlarm (Maybe ComparisonOperator)
maComparisonOperator = lens _maComparisonOperator (\ s a -> s{_maComparisonOperator = a});

-- | The state value for the alarm.
maStateValue :: Lens' MetricAlarm (Maybe StateValue)
maStateValue = lens _maStateValue (\ s a -> s{_maStateValue = a});

-- | The value against which the specified statistic is compared.
maThreshold :: Lens' MetricAlarm (Maybe Double)
maThreshold = lens _maThreshold (\ s a -> s{_maThreshold = a});

-- | Indicates whether actions should be executed during any changes to the
-- alarm\'s state.
maActionsEnabled :: Lens' MetricAlarm (Maybe Bool)
maActionsEnabled = lens _maActionsEnabled (\ s a -> s{_maActionsEnabled = a});

-- | The time stamp of the last update to the alarm configuration. Amazon
-- CloudWatch uses Coordinated Universal Time (UTC) when returning time
-- stamps, which do not accommodate seasonal adjustments such as daylight
-- savings time. For more information, see
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps>
-- in the /Amazon CloudWatch Developer Guide/.
maAlarmConfigurationUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maAlarmConfigurationUpdatedTimestamp = lens _maAlarmConfigurationUpdatedTimestamp (\ s a -> s{_maAlarmConfigurationUpdatedTimestamp = a}) . mapping _Time;

-- | The list of actions to execute when this alarm transitions into an
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Number (ARN). Currently the only actions supported
-- are publishing to an Amazon SNS topic or triggering an Auto Scaling
-- policy.
--
-- The current WSDL lists this attribute as @UnknownActions@.
maInsufficientDataActions :: Lens' MetricAlarm (Maybe [Text])
maInsufficientDataActions = lens _maInsufficientDataActions (\ s a -> s{_maInsufficientDataActions = a});

-- | The list of dimensions associated with the alarm\'s associated metric.
maDimensions :: Lens' MetricAlarm (Maybe [Dimension])
maDimensions = lens _maDimensions (\ s a -> s{_maDimensions = a});

-- | An explanation for the alarm\'s state in machine-readable JSON format
maStateReasonData :: Lens' MetricAlarm (Maybe Text)
maStateReasonData = lens _maStateReasonData (\ s a -> s{_maStateReasonData = a});

-- | A human-readable explanation for the alarm\'s state.
maStateReason :: Lens' MetricAlarm (Maybe Text)
maStateReason = lens _maStateReason (\ s a -> s{_maStateReason = a});

-- | The Amazon Resource Name (ARN) of the alarm.
maAlarmARN :: Lens' MetricAlarm (Maybe Text)
maAlarmARN = lens _maAlarmARN (\ s a -> s{_maAlarmARN = a});

-- | The list of actions to execute when this alarm transitions into an
-- @ALARM@ state from any other state. Each action is specified as an
-- Amazon Resource Number (ARN). Currently the only actions supported are
-- publishing to an Amazon SNS topic and triggering an Auto Scaling policy.
maAlarmActions :: Lens' MetricAlarm (Maybe [Text])
maAlarmActions = lens _maAlarmActions (\ s a -> s{_maAlarmActions = a});

-- | The statistic to apply to the alarm\'s associated metric.
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\ s a -> s{_maStatistic = a});

-- | The unit of the alarm\'s associated metric.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\ s a -> s{_maUnit = a});

instance FromXML MetricAlarm where
        parseXML x
          = MetricAlarm' <$>
              x .@? "AlarmName" <*> x .@? "StateUpdatedTimestamp"
                <*> x .@? "AlarmDescription"
                <*> x .@? "Period"
                <*> x .@? "EvaluationPeriods"
                <*> x .@? "MetricName"
                <*> x .@? "Namespace"
                <*>
                (x .@? "OKActions" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "ComparisonOperator"
                <*> x .@? "StateValue"
                <*> x .@? "Threshold"
                <*> x .@? "ActionsEnabled"
                <*> x .@? "AlarmConfigurationUpdatedTimestamp"
                <*>
                (x .@? "InsufficientDataActions" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "StateReasonData"
                <*> x .@? "StateReason"
                <*> x .@? "AlarmArn"
                <*>
                (x .@? "AlarmActions" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "Statistic"
                <*> x .@? "Unit"

-- | /See:/ 'metricDatum' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdValue'
--
-- * 'mdDimensions'
--
-- * 'mdTimestamp'
--
-- * 'mdStatisticValues'
--
-- * 'mdUnit'
--
-- * 'mdMetricName'
data MetricDatum = MetricDatum'{_mdValue :: Maybe Double, _mdDimensions :: Maybe [Dimension], _mdTimestamp :: Maybe ISO8601, _mdStatisticValues :: Maybe StatisticSet, _mdUnit :: Maybe StandardUnit, _mdMetricName :: Text} deriving (Eq, Read, Show)

-- | 'MetricDatum' smart constructor.
metricDatum :: Text -> MetricDatum
metricDatum pMetricName = MetricDatum'{_mdValue = Nothing, _mdDimensions = Nothing, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdUnit = Nothing, _mdMetricName = pMetricName};

-- | The value for the metric.
--
-- Although the @Value@ parameter accepts numbers of type @Double@, Amazon
-- CloudWatch truncates values with very large exponents. Values with
-- base-10 exponents greater than 126 (1 x 10^126) are truncated. Likewise,
-- values with base-10 exponents less than -130 (1 x 10^-130) are also
-- truncated.
mdValue :: Lens' MetricDatum (Maybe Double)
mdValue = lens _mdValue (\ s a -> s{_mdValue = a});

-- | A list of dimensions associated with the metric. Note, when using the
-- Dimensions value in a query, you need to append .member.N to it (e.g.,
-- Dimensions.member.N).
mdDimensions :: Lens' MetricDatum (Maybe [Dimension])
mdDimensions = lens _mdDimensions (\ s a -> s{_mdDimensions = a});

-- | The time stamp used for the metric. If not specified, the default value
-- is set to the time the metric data was received. Amazon CloudWatch uses
-- Coordinated Universal Time (UTC) when returning time stamps, which do
-- not accommodate seasonal adjustments such as daylight savings time. For
-- more information, see
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps>
-- in the /Amazon CloudWatch Developer Guide/.
mdTimestamp :: Lens' MetricDatum (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\ s a -> s{_mdTimestamp = a}) . mapping _Time;

-- | A set of statistical values describing the metric.
mdStatisticValues :: Lens' MetricDatum (Maybe StatisticSet)
mdStatisticValues = lens _mdStatisticValues (\ s a -> s{_mdStatisticValues = a});

-- | The unit of the metric.
mdUnit :: Lens' MetricDatum (Maybe StandardUnit)
mdUnit = lens _mdUnit (\ s a -> s{_mdUnit = a});

-- | The name of the metric.
mdMetricName :: Lens' MetricDatum Text
mdMetricName = lens _mdMetricName (\ s a -> s{_mdMetricName = a});

instance ToQuery MetricDatum where
        toQuery MetricDatum'{..}
          = mconcat
              ["Value" =: _mdValue,
               "Dimensions" =: "member" =: _mdDimensions,
               "Timestamp" =: _mdTimestamp,
               "StatisticValues" =: _mdStatisticValues,
               "Unit" =: _mdUnit, "MetricName" =: _mdMetricName]

data StandardUnit = Bits | BitsSecond | MegabytesSecond | Megabytes | None | Count | Terabytes | TerabytesSecond | Percent | CountSecond | TerabitsSecond | Terabits | Milliseconds | GigabytesSecond | Microseconds | Gigabytes | GigabitsSecond | Gigabits | Megabits | MegabitsSecond | Kilobits | KilobitsSecond | Kilobytes | KilobytesSecond | Seconds | BytesSecond | Bytes deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StandardUnit where
    parser = takeLowerText >>= \case
        "Bits" -> pure Bits
        "Bits/Second" -> pure BitsSecond
        "Bytes" -> pure Bytes
        "Bytes/Second" -> pure BytesSecond
        "Count" -> pure Count
        "Count/Second" -> pure CountSecond
        "Gigabits" -> pure Gigabits
        "Gigabits/Second" -> pure GigabitsSecond
        "Gigabytes" -> pure Gigabytes
        "Gigabytes/Second" -> pure GigabytesSecond
        "Kilobits" -> pure Kilobits
        "Kilobits/Second" -> pure KilobitsSecond
        "Kilobytes" -> pure Kilobytes
        "Kilobytes/Second" -> pure KilobytesSecond
        "Megabits" -> pure Megabits
        "Megabits/Second" -> pure MegabitsSecond
        "Megabytes" -> pure Megabytes
        "Megabytes/Second" -> pure MegabytesSecond
        "Microseconds" -> pure Microseconds
        "Milliseconds" -> pure Milliseconds
        "None" -> pure None
        "Percent" -> pure Percent
        "Seconds" -> pure Seconds
        "Terabits" -> pure Terabits
        "Terabits/Second" -> pure TerabitsSecond
        "Terabytes" -> pure Terabytes
        "Terabytes/Second" -> pure TerabytesSecond
        e -> fail ("Failure parsing StandardUnit from " ++ show e)

instance ToText StandardUnit where
    toText = \case
        Bits -> "Bits"
        BitsSecond -> "Bits/Second"
        Bytes -> "Bytes"
        BytesSecond -> "Bytes/Second"
        Count -> "Count"
        CountSecond -> "Count/Second"
        Gigabits -> "Gigabits"
        GigabitsSecond -> "Gigabits/Second"
        Gigabytes -> "Gigabytes"
        GigabytesSecond -> "Gigabytes/Second"
        Kilobits -> "Kilobits"
        KilobitsSecond -> "Kilobits/Second"
        Kilobytes -> "Kilobytes"
        KilobytesSecond -> "Kilobytes/Second"
        Megabits -> "Megabits"
        MegabitsSecond -> "Megabits/Second"
        Megabytes -> "Megabytes"
        MegabytesSecond -> "Megabytes/Second"
        Microseconds -> "Microseconds"
        Milliseconds -> "Milliseconds"
        None -> "None"
        Percent -> "Percent"
        Seconds -> "Seconds"
        Terabits -> "Terabits"
        TerabitsSecond -> "Terabits/Second"
        Terabytes -> "Terabytes"
        TerabytesSecond -> "Terabytes/Second"

instance Hashable StandardUnit
instance ToQuery StandardUnit
instance ToHeader StandardUnit

instance FromXML StandardUnit where
    parseXML = parseXMLText "StandardUnit"

data StateValue = OK | InsufficientData | Alarm deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StateValue where
    parser = takeLowerText >>= \case
        "ALARM" -> pure Alarm
        "INSUFFICIENT_DATA" -> pure InsufficientData
        "OK" -> pure OK
        e -> fail ("Failure parsing StateValue from " ++ show e)

instance ToText StateValue where
    toText = \case
        Alarm -> "ALARM"
        InsufficientData -> "INSUFFICIENT_DATA"
        OK -> "OK"

instance Hashable StateValue
instance ToQuery StateValue
instance ToHeader StateValue

instance FromXML StateValue where
    parseXML = parseXMLText "StateValue"

data Statistic = SampleCount | Maximum | Average | Minimum | Sum deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText Statistic where
    parser = takeLowerText >>= \case
        "Average" -> pure Average
        "Maximum" -> pure Maximum
        "Minimum" -> pure Minimum
        "SampleCount" -> pure SampleCount
        "Sum" -> pure Sum
        e -> fail ("Failure parsing Statistic from " ++ show e)

instance ToText Statistic where
    toText = \case
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"
        SampleCount -> "SampleCount"
        Sum -> "Sum"

instance Hashable Statistic
instance ToQuery Statistic
instance ToHeader Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"

-- | /See:/ 'statisticSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssSampleCount'
--
-- * 'ssSum'
--
-- * 'ssMinimum'
--
-- * 'ssMaximum'
data StatisticSet = StatisticSet'{_ssSampleCount :: Double, _ssSum :: Double, _ssMinimum :: Double, _ssMaximum :: Double} deriving (Eq, Read, Show)

-- | 'StatisticSet' smart constructor.
statisticSet :: Double -> Double -> Double -> Double -> StatisticSet
statisticSet pSampleCount pSum pMinimum pMaximum = StatisticSet'{_ssSampleCount = pSampleCount, _ssSum = pSum, _ssMinimum = pMinimum, _ssMaximum = pMaximum};

-- | The number of samples used for the statistic set.
ssSampleCount :: Lens' StatisticSet Double
ssSampleCount = lens _ssSampleCount (\ s a -> s{_ssSampleCount = a});

-- | The sum of values for the sample set.
ssSum :: Lens' StatisticSet Double
ssSum = lens _ssSum (\ s a -> s{_ssSum = a});

-- | The minimum value of the sample set.
ssMinimum :: Lens' StatisticSet Double
ssMinimum = lens _ssMinimum (\ s a -> s{_ssMinimum = a});

-- | The maximum value of the sample set.
ssMaximum :: Lens' StatisticSet Double
ssMaximum = lens _ssMaximum (\ s a -> s{_ssMaximum = a});

instance ToQuery StatisticSet where
        toQuery StatisticSet'{..}
          = mconcat
              ["SampleCount" =: _ssSampleCount, "Sum" =: _ssSum,
               "Minimum" =: _ssMinimum, "Maximum" =: _ssMaximum]
