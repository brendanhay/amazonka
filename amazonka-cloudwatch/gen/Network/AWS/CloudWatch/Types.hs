{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types
    (
    -- * Service
      CloudWatch

    -- * Errors
    , _LimitExceededFault
    , _InvalidNextToken
    , _InternalServiceFault
    , _InvalidParameterValueException
    , _InvalidFormatFault
    , _MissingRequiredParameterException
    , _InvalidParameterCombinationException
    , _ResourceNotFound

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * StandardUnit
    , StandardUnit (..)

    -- * StateValue
    , StateValue (..)

    -- * Statistic
    , Statistic (..)

    -- * AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiTimestamp
    , ahiHistorySummary

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

    -- * StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-08-01@ of the Amazon CloudWatch SDK.
data CloudWatch

instance AWSService CloudWatch where
    type Sg CloudWatch = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudWatch"
            , _svcPrefix = "monitoring"
            , _svcVersion = "2010-08-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The quota for alarms for this customer has already been reached.
_LimitExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault = _ServiceError . hasStatus 400 . hasCode "LimitExceeded"

-- | The next token specified is invalid.
_InvalidNextToken :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | Indicates that the request processing has failed due to some unknown
-- error, exception, or failure.
_InternalServiceFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServiceFault =
    _ServiceError . hasStatus 500 . hasCode "InternalServiceError"

-- | Bad or out-of-range value was supplied for the input parameter.
_InvalidParameterValueException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValue"

-- | Data was not syntactically valid JSON.
_InvalidFormatFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidFormatFault = _ServiceError . hasStatus 400 . hasCode "InvalidFormat"

-- | An input parameter that is mandatory for processing the request is not
-- supplied.
_MissingRequiredParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_MissingRequiredParameterException =
    _ServiceError . hasStatus 400 . hasCode "MissingParameter"

-- | Parameters that must not be used together were used together.
_InvalidParameterCombinationException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterCombination"

-- | The named resource does not exist.
_ResourceNotFound :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFound = _ServiceError . hasStatus 404 . hasCode "ResourceNotFound"

data ComparisonOperator
    = GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanOrEqualToThreshold
    | LessThanThreshold
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
        "greaterthanthreshold" -> pure GreaterThanThreshold
        "lessthanorequaltothreshold" -> pure LessThanOrEqualToThreshold
        "lessthanthreshold" -> pure LessThanThreshold
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: greaterthanorequaltothreshold, greaterthanthreshold, lessthanorequaltothreshold, lessthanthreshold"

instance ToText ComparisonOperator where
    toText = \case
        GreaterThanOrEqualToThreshold -> "greaterthanorequaltothreshold"
        GreaterThanThreshold -> "greaterthanthreshold"
        LessThanOrEqualToThreshold -> "lessthanorequaltothreshold"
        LessThanThreshold -> "lessthanthreshold"

instance Hashable ComparisonOperator
instance ToQuery ComparisonOperator
instance ToHeader ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"

data HistoryItemType
    = StateUpdate
    | Action
    | ConfigurationUpdate
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText HistoryItemType where
    parser = takeLowerText >>= \case
        "action" -> pure Action
        "configurationupdate" -> pure ConfigurationUpdate
        "stateupdate" -> pure StateUpdate
        e -> fromTextError $ "Failure parsing HistoryItemType from value: '" <> e
           <> "'. Accepted values: action, configurationupdate, stateupdate"

instance ToText HistoryItemType where
    toText = \case
        Action -> "action"
        ConfigurationUpdate -> "configurationupdate"
        StateUpdate -> "stateupdate"

instance Hashable HistoryItemType
instance ToQuery HistoryItemType
instance ToHeader HistoryItemType

instance FromXML HistoryItemType where
    parseXML = parseXMLText "HistoryItemType"

data StandardUnit
    = Bits
    | BitsSecond
    | MegabytesSecond
    | Megabytes
    | None
    | Count
    | Terabytes
    | TerabytesSecond
    | Percent
    | CountSecond
    | TerabitsSecond
    | Terabits
    | Milliseconds
    | GigabytesSecond
    | Microseconds
    | Gigabytes
    | GigabitsSecond
    | Gigabits
    | Megabits
    | MegabitsSecond
    | Kilobits
    | KilobitsSecond
    | Kilobytes
    | KilobytesSecond
    | Seconds
    | BytesSecond
    | Bytes
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StandardUnit where
    parser = takeLowerText >>= \case
        "bits" -> pure Bits
        "bits/second" -> pure BitsSecond
        "bytes" -> pure Bytes
        "bytes/second" -> pure BytesSecond
        "count" -> pure Count
        "count/second" -> pure CountSecond
        "gigabits" -> pure Gigabits
        "gigabits/second" -> pure GigabitsSecond
        "gigabytes" -> pure Gigabytes
        "gigabytes/second" -> pure GigabytesSecond
        "kilobits" -> pure Kilobits
        "kilobits/second" -> pure KilobitsSecond
        "kilobytes" -> pure Kilobytes
        "kilobytes/second" -> pure KilobytesSecond
        "megabits" -> pure Megabits
        "megabits/second" -> pure MegabitsSecond
        "megabytes" -> pure Megabytes
        "megabytes/second" -> pure MegabytesSecond
        "microseconds" -> pure Microseconds
        "milliseconds" -> pure Milliseconds
        "none" -> pure None
        "percent" -> pure Percent
        "seconds" -> pure Seconds
        "terabits" -> pure Terabits
        "terabits/second" -> pure TerabitsSecond
        "terabytes" -> pure Terabytes
        "terabytes/second" -> pure TerabytesSecond
        e -> fromTextError $ "Failure parsing StandardUnit from value: '" <> e
           <> "'. Accepted values: bits, bits/second, bytes, bytes/second, count, count/second, gigabits, gigabits/second, gigabytes, gigabytes/second, kilobits, kilobits/second, kilobytes, kilobytes/second, megabits, megabits/second, megabytes, megabytes/second, microseconds, milliseconds, none, percent, seconds, terabits, terabits/second, terabytes, terabytes/second"

instance ToText StandardUnit where
    toText = \case
        Bits -> "bits"
        BitsSecond -> "bits/second"
        Bytes -> "bytes"
        BytesSecond -> "bytes/second"
        Count -> "count"
        CountSecond -> "count/second"
        Gigabits -> "gigabits"
        GigabitsSecond -> "gigabits/second"
        Gigabytes -> "gigabytes"
        GigabytesSecond -> "gigabytes/second"
        Kilobits -> "kilobits"
        KilobitsSecond -> "kilobits/second"
        Kilobytes -> "kilobytes"
        KilobytesSecond -> "kilobytes/second"
        Megabits -> "megabits"
        MegabitsSecond -> "megabits/second"
        Megabytes -> "megabytes"
        MegabytesSecond -> "megabytes/second"
        Microseconds -> "microseconds"
        Milliseconds -> "milliseconds"
        None -> "none"
        Percent -> "percent"
        Seconds -> "seconds"
        Terabits -> "terabits"
        TerabitsSecond -> "terabits/second"
        Terabytes -> "terabytes"
        TerabytesSecond -> "terabytes/second"

instance Hashable StandardUnit
instance ToQuery StandardUnit
instance ToHeader StandardUnit

instance FromXML StandardUnit where
    parseXML = parseXMLText "StandardUnit"

data StateValue
    = OK
    | InsufficientData
    | Alarm
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StateValue where
    parser = takeLowerText >>= \case
        "alarm" -> pure Alarm
        "insufficient_data" -> pure InsufficientData
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing StateValue from value: '" <> e
           <> "'. Accepted values: alarm, insufficient_data, ok"

instance ToText StateValue where
    toText = \case
        Alarm -> "alarm"
        InsufficientData -> "insufficient_data"
        OK -> "ok"

instance Hashable StateValue
instance ToQuery StateValue
instance ToHeader StateValue

instance FromXML StateValue where
    parseXML = parseXMLText "StateValue"

data Statistic
    = SampleCount
    | Maximum
    | Average
    | Minimum
    | Sum
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Statistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing Statistic from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum, samplecount, sum"

instance ToText Statistic where
    toText = \case
        Average -> "average"
        Maximum -> "maximum"
        Minimum -> "minimum"
        SampleCount -> "samplecount"
        Sum -> "sum"

instance Hashable Statistic
instance ToQuery Statistic
instance ToHeader Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"

-- | The @AlarmHistoryItem@ data type contains descriptive information about
-- the history of a specific alarm. If you call DescribeAlarmHistory,
-- Amazon CloudWatch returns this data type as part of the
-- DescribeAlarmHistoryResult data type.
--
-- /See:/ 'alarmHistoryItem' smart constructor.
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
data AlarmHistoryItem = AlarmHistoryItem'
    { _ahiAlarmName       :: !(Maybe Text)
    , _ahiHistoryItemType :: !(Maybe HistoryItemType)
    , _ahiHistoryData     :: !(Maybe Text)
    , _ahiTimestamp       :: !(Maybe ISO8601)
    , _ahiHistorySummary  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AlarmHistoryItem' smart constructor.
alarmHistoryItem :: AlarmHistoryItem
alarmHistoryItem =
    AlarmHistoryItem'
    { _ahiAlarmName = Nothing
    , _ahiHistoryItemType = Nothing
    , _ahiHistoryData = Nothing
    , _ahiTimestamp = Nothing
    , _ahiHistorySummary = Nothing
    }

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
              (x .@? "AlarmName") <*> (x .@? "HistoryItemType") <*>
                (x .@? "HistoryData")
                <*> (x .@? "Timestamp")
                <*> (x .@? "HistorySummary")

-- | The @Datapoint@ data type encapsulates the statistical data that Amazon
-- CloudWatch computes from metric data.
--
-- /See:/ 'datapoint' smart constructor.
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
data Datapoint = Datapoint'
    { _datSampleCount :: !(Maybe Double)
    , _datMaximum     :: !(Maybe Double)
    , _datAverage     :: !(Maybe Double)
    , _datMinimum     :: !(Maybe Double)
    , _datSum         :: !(Maybe Double)
    , _datTimestamp   :: !(Maybe ISO8601)
    , _datUnit        :: !(Maybe StandardUnit)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Datapoint' smart constructor.
datapoint :: Datapoint
datapoint =
    Datapoint'
    { _datSampleCount = Nothing
    , _datMaximum = Nothing
    , _datAverage = Nothing
    , _datMinimum = Nothing
    , _datSum = Nothing
    , _datTimestamp = Nothing
    , _datUnit = Nothing
    }

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
              (x .@? "SampleCount") <*> (x .@? "Maximum") <*>
                (x .@? "Average")
                <*> (x .@? "Minimum")
                <*> (x .@? "Sum")
                <*> (x .@? "Timestamp")
                <*> (x .@? "Unit")

-- | The @Dimension@ data type further expands on the identity of a metric
-- using a Name, Value pair.
--
-- For examples that use one or more dimensions, see PutMetricData.
--
-- /See:/ 'dimension' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dimName'
--
-- * 'dimValue'
data Dimension = Dimension'
    { _dimName  :: !Text
    , _dimValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Dimension' smart constructor.
dimension :: Text -> Text -> Dimension
dimension pName pValue =
    Dimension'
    { _dimName = pName
    , _dimValue = pValue
    }

-- | The name of the dimension.
dimName :: Lens' Dimension Text
dimName = lens _dimName (\ s a -> s{_dimName = a});

-- | The value representing the dimension measurement
dimValue :: Lens' Dimension Text
dimValue = lens _dimValue (\ s a -> s{_dimValue = a});

instance FromXML Dimension where
        parseXML x
          = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance ToQuery Dimension where
        toQuery Dimension'{..}
          = mconcat ["Name" =: _dimName, "Value" =: _dimValue]

-- | The @DimensionFilter@ data type is used to filter ListMetrics results.
--
-- /See:/ 'dimensionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfValue'
--
-- * 'dfName'
data DimensionFilter = DimensionFilter'
    { _dfValue :: !(Maybe Text)
    , _dfName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DimensionFilter' smart constructor.
dimensionFilter :: Text -> DimensionFilter
dimensionFilter pName =
    DimensionFilter'
    { _dfValue = Nothing
    , _dfName = pName
    }

-- | The value of the dimension to be matched.
dfValue :: Lens' DimensionFilter (Maybe Text)
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\ s a -> s{_dfName = a});

instance ToQuery DimensionFilter where
        toQuery DimensionFilter'{..}
          = mconcat ["Value" =: _dfValue, "Name" =: _dfName]

-- | The @Metric@ data type contains information about a specific metric. If
-- you call ListMetrics, Amazon CloudWatch returns information contained by
-- this data type.
--
-- The example in the Examples section publishes two metrics named buffers
-- and latency. Both metrics are in the examples namespace. Both metrics
-- have two dimensions, InstanceID and InstanceType.
--
-- /See:/ 'metric' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'metMetricName'
--
-- * 'metNamespace'
--
-- * 'metDimensions'
data Metric = Metric'
    { _metMetricName :: !(Maybe Text)
    , _metNamespace  :: !(Maybe Text)
    , _metDimensions :: !(Maybe [Dimension])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Metric' smart constructor.
metric :: Metric
metric =
    Metric'
    { _metMetricName = Nothing
    , _metNamespace = Nothing
    , _metDimensions = Nothing
    }

-- | The name of the metric.
metMetricName :: Lens' Metric (Maybe Text)
metMetricName = lens _metMetricName (\ s a -> s{_metMetricName = a});

-- | The namespace of the metric.
metNamespace :: Lens' Metric (Maybe Text)
metNamespace = lens _metNamespace (\ s a -> s{_metNamespace = a});

-- | A list of dimensions associated with the metric.
metDimensions :: Lens' Metric [Dimension]
metDimensions = lens _metDimensions (\ s a -> s{_metDimensions = a}) . _Default;

instance FromXML Metric where
        parseXML x
          = Metric' <$>
              (x .@? "MetricName") <*> (x .@? "Namespace") <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | The MetricAlarm data type represents an alarm. You can use
-- PutMetricAlarm to create or update an alarm.
--
-- /See:/ 'metricAlarm' smart constructor.
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
data MetricAlarm = MetricAlarm'
    { _maAlarmName                          :: !(Maybe Text)
    , _maStateUpdatedTimestamp              :: !(Maybe ISO8601)
    , _maAlarmDescription                   :: !(Maybe Text)
    , _maPeriod                             :: !(Maybe Nat)
    , _maEvaluationPeriods                  :: !(Maybe Nat)
    , _maMetricName                         :: !(Maybe Text)
    , _maNamespace                          :: !(Maybe Text)
    , _maOKActions                          :: !(Maybe [Text])
    , _maComparisonOperator                 :: !(Maybe ComparisonOperator)
    , _maStateValue                         :: !(Maybe StateValue)
    , _maThreshold                          :: !(Maybe Double)
    , _maActionsEnabled                     :: !(Maybe Bool)
    , _maAlarmConfigurationUpdatedTimestamp :: !(Maybe ISO8601)
    , _maInsufficientDataActions            :: !(Maybe [Text])
    , _maDimensions                         :: !(Maybe [Dimension])
    , _maStateReasonData                    :: !(Maybe Text)
    , _maStateReason                        :: !(Maybe Text)
    , _maAlarmARN                           :: !(Maybe Text)
    , _maAlarmActions                       :: !(Maybe [Text])
    , _maStatistic                          :: !(Maybe Statistic)
    , _maUnit                               :: !(Maybe StandardUnit)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MetricAlarm' smart constructor.
metricAlarm :: MetricAlarm
metricAlarm =
    MetricAlarm'
    { _maAlarmName = Nothing
    , _maStateUpdatedTimestamp = Nothing
    , _maAlarmDescription = Nothing
    , _maPeriod = Nothing
    , _maEvaluationPeriods = Nothing
    , _maMetricName = Nothing
    , _maNamespace = Nothing
    , _maOKActions = Nothing
    , _maComparisonOperator = Nothing
    , _maStateValue = Nothing
    , _maThreshold = Nothing
    , _maActionsEnabled = Nothing
    , _maAlarmConfigurationUpdatedTimestamp = Nothing
    , _maInsufficientDataActions = Nothing
    , _maDimensions = Nothing
    , _maStateReasonData = Nothing
    , _maStateReason = Nothing
    , _maAlarmARN = Nothing
    , _maAlarmActions = Nothing
    , _maStatistic = Nothing
    , _maUnit = Nothing
    }

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
maOKActions :: Lens' MetricAlarm [Text]
maOKActions = lens _maOKActions (\ s a -> s{_maOKActions = a}) . _Default;

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
maInsufficientDataActions :: Lens' MetricAlarm [Text]
maInsufficientDataActions = lens _maInsufficientDataActions (\ s a -> s{_maInsufficientDataActions = a}) . _Default;

-- | The list of dimensions associated with the alarm\'s associated metric.
maDimensions :: Lens' MetricAlarm [Dimension]
maDimensions = lens _maDimensions (\ s a -> s{_maDimensions = a}) . _Default;

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
maAlarmActions :: Lens' MetricAlarm [Text]
maAlarmActions = lens _maAlarmActions (\ s a -> s{_maAlarmActions = a}) . _Default;

-- | The statistic to apply to the alarm\'s associated metric.
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\ s a -> s{_maStatistic = a});

-- | The unit of the alarm\'s associated metric.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\ s a -> s{_maUnit = a});

instance FromXML MetricAlarm where
        parseXML x
          = MetricAlarm' <$>
              (x .@? "AlarmName") <*>
                (x .@? "StateUpdatedTimestamp")
                <*> (x .@? "AlarmDescription")
                <*> (x .@? "Period")
                <*> (x .@? "EvaluationPeriods")
                <*> (x .@? "MetricName")
                <*> (x .@? "Namespace")
                <*>
                (x .@? "OKActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "ComparisonOperator")
                <*> (x .@? "StateValue")
                <*> (x .@? "Threshold")
                <*> (x .@? "ActionsEnabled")
                <*> (x .@? "AlarmConfigurationUpdatedTimestamp")
                <*>
                (x .@? "InsufficientDataActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Dimensions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StateReasonData")
                <*> (x .@? "StateReason")
                <*> (x .@? "AlarmArn")
                <*>
                (x .@? "AlarmActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Statistic")
                <*> (x .@? "Unit")

-- | The @MetricDatum@ data type encapsulates the information sent with
-- PutMetricData to either create a new metric or add new values to be
-- aggregated into an existing metric.
--
-- /See:/ 'metricDatum' smart constructor.
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
data MetricDatum = MetricDatum'
    { _mdValue           :: !(Maybe Double)
    , _mdDimensions      :: !(Maybe [Dimension])
    , _mdTimestamp       :: !(Maybe ISO8601)
    , _mdStatisticValues :: !(Maybe StatisticSet)
    , _mdUnit            :: !(Maybe StandardUnit)
    , _mdMetricName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MetricDatum' smart constructor.
metricDatum :: Text -> MetricDatum
metricDatum pMetricName =
    MetricDatum'
    { _mdValue = Nothing
    , _mdDimensions = Nothing
    , _mdTimestamp = Nothing
    , _mdStatisticValues = Nothing
    , _mdUnit = Nothing
    , _mdMetricName = pMetricName
    }

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
mdDimensions :: Lens' MetricDatum [Dimension]
mdDimensions = lens _mdDimensions (\ s a -> s{_mdDimensions = a}) . _Default;

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
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _mdDimensions),
               "Timestamp" =: _mdTimestamp,
               "StatisticValues" =: _mdStatisticValues,
               "Unit" =: _mdUnit, "MetricName" =: _mdMetricName]

-- | The @StatisticSet@ data type describes the @StatisticValues@ component
-- of MetricDatum, and represents a set of statistics that describes a
-- specific metric.
--
-- /See:/ 'statisticSet' smart constructor.
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
data StatisticSet = StatisticSet'
    { _ssSampleCount :: !Double
    , _ssSum         :: !Double
    , _ssMinimum     :: !Double
    , _ssMaximum     :: !Double
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StatisticSet' smart constructor.
statisticSet :: Double -> Double -> Double -> Double -> StatisticSet
statisticSet pSampleCount pSum pMinimum pMaximum =
    StatisticSet'
    { _ssSampleCount = pSampleCount
    , _ssSum = pSum
    , _ssMinimum = pMinimum
    , _ssMaximum = pMaximum
    }

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
