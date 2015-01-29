{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * StatisticSet
    , StatisticSet
    , statisticSet
    , ssMaximum
    , ssMinimum
    , ssSampleCount
    , ssSum

    -- * MetricAlarm
    , MetricAlarm
    , metricAlarm
    , maActionsEnabled
    , maAlarmActions
    , maAlarmArn
    , maAlarmConfigurationUpdatedTimestamp
    , maAlarmDescription
    , maAlarmName
    , maComparisonOperator
    , maDimensions
    , maEvaluationPeriods
    , maInsufficientDataActions
    , maMetricName
    , maNamespace
    , maOKActions
    , maPeriod
    , maStateReason
    , maStateReasonData
    , maStateUpdatedTimestamp
    , maStateValue
    , maStatistic
    , maThreshold
    , maUnit

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * MetricDatum
    , MetricDatum
    , metricDatum
    , mdDimensions
    , mdMetricName
    , mdStatisticValues
    , mdTimestamp
    , mdUnit
    , mdValue

    -- * StandardUnit
    , StandardUnit (..)

    -- * Dimension
    , Dimension
    , dimension
    , dName
    , dValue

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryData
    , ahiHistoryItemType
    , ahiHistorySummary
    , ahiTimestamp

    -- * Metric
    , Metric
    , metric
    , mDimensions
    , mMetricName
    , mNamespace

    -- * StateValue
    , StateValue (..)

    -- * Datapoint
    , Datapoint
    , datapoint
    , dAverage
    , dMaximum
    , dMinimum
    , dSampleCount
    , dSum
    , dTimestamp
    , dUnit

    -- * DimensionFilter
    , DimensionFilter
    , dimensionFilter
    , dfName
    , dfValue

    -- * Statistic
    , Statistic (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2010-08-01@ of the Amazon CloudWatch service.
data CloudWatch

instance AWSService CloudWatch where
    type Sg CloudWatch = V4
    type Er CloudWatch = RESTError

    service = service'
      where
        service' :: Service CloudWatch
        service' = Service
            { _svcAbbrev       = "CloudWatch"
            , _svcPrefix       = "monitoring"
            , _svcVersion      = "2010-08-01"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry CloudWatch
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 400 && "Throttling" == e = True -- Throttling
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://monitoring.amazonaws.com/doc/2010-08-01/"
{-# INLINE ns #-}

data StatisticSet = StatisticSet
    { _ssMaximum     :: Double
    , _ssMinimum     :: Double
    , _ssSampleCount :: Double
    , _ssSum         :: Double
    } deriving (Eq, Ord, Read, Show)

-- | 'StatisticSet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssMaximum' @::@ 'Double'
--
-- * 'ssMinimum' @::@ 'Double'
--
-- * 'ssSampleCount' @::@ 'Double'
--
-- * 'ssSum' @::@ 'Double'
--
statisticSet :: Double -- ^ 'ssSampleCount'
             -> Double -- ^ 'ssSum'
             -> Double -- ^ 'ssMinimum'
             -> Double -- ^ 'ssMaximum'
             -> StatisticSet
statisticSet p1 p2 p3 p4 = StatisticSet
    { _ssSampleCount = p1
    , _ssSum         = p2
    , _ssMinimum     = p3
    , _ssMaximum     = p4
    }

-- | The maximum value of the sample set.
ssMaximum :: Lens' StatisticSet Double
ssMaximum = lens _ssMaximum (\s a -> s { _ssMaximum = a })

-- | The minimum value of the sample set.
ssMinimum :: Lens' StatisticSet Double
ssMinimum = lens _ssMinimum (\s a -> s { _ssMinimum = a })

-- | The number of samples used for the statistic set.
ssSampleCount :: Lens' StatisticSet Double
ssSampleCount = lens _ssSampleCount (\s a -> s { _ssSampleCount = a })

-- | The sum of values for the sample set.
ssSum :: Lens' StatisticSet Double
ssSum = lens _ssSum (\s a -> s { _ssSum = a })

instance FromXML StatisticSet where
    parseXML x = StatisticSet
        <$> x .@  "Maximum"
        <*> x .@  "Minimum"
        <*> x .@  "SampleCount"
        <*> x .@  "Sum"

instance ToQuery StatisticSet where
    toQuery StatisticSet{..} = mconcat
        [ "Maximum"     =? _ssMaximum
        , "Minimum"     =? _ssMinimum
        , "SampleCount" =? _ssSampleCount
        , "Sum"         =? _ssSum
        ]

data MetricAlarm = MetricAlarm
    { _maActionsEnabled                     :: Maybe Bool
    , _maAlarmActions                       :: List "member" Text
    , _maAlarmArn                           :: Maybe Text
    , _maAlarmConfigurationUpdatedTimestamp :: Maybe ISO8601
    , _maAlarmDescription                   :: Maybe Text
    , _maAlarmName                          :: Maybe Text
    , _maComparisonOperator                 :: Maybe ComparisonOperator
    , _maDimensions                         :: List "member" Dimension
    , _maEvaluationPeriods                  :: Maybe Nat
    , _maInsufficientDataActions            :: List "member" Text
    , _maMetricName                         :: Maybe Text
    , _maNamespace                          :: Maybe Text
    , _maOKActions                          :: List "member" Text
    , _maPeriod                             :: Maybe Nat
    , _maStateReason                        :: Maybe Text
    , _maStateReasonData                    :: Maybe Text
    , _maStateUpdatedTimestamp              :: Maybe ISO8601
    , _maStateValue                         :: Maybe StateValue
    , _maStatistic                          :: Maybe Statistic
    , _maThreshold                          :: Maybe Double
    , _maUnit                               :: Maybe StandardUnit
    } deriving (Eq, Read, Show)

-- | 'MetricAlarm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'maActionsEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'maAlarmActions' @::@ ['Text']
--
-- * 'maAlarmArn' @::@ 'Maybe' 'Text'
--
-- * 'maAlarmConfigurationUpdatedTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'maAlarmDescription' @::@ 'Maybe' 'Text'
--
-- * 'maAlarmName' @::@ 'Maybe' 'Text'
--
-- * 'maComparisonOperator' @::@ 'Maybe' 'ComparisonOperator'
--
-- * 'maDimensions' @::@ ['Dimension']
--
-- * 'maEvaluationPeriods' @::@ 'Maybe' 'Natural'
--
-- * 'maInsufficientDataActions' @::@ ['Text']
--
-- * 'maMetricName' @::@ 'Maybe' 'Text'
--
-- * 'maNamespace' @::@ 'Maybe' 'Text'
--
-- * 'maOKActions' @::@ ['Text']
--
-- * 'maPeriod' @::@ 'Maybe' 'Natural'
--
-- * 'maStateReason' @::@ 'Maybe' 'Text'
--
-- * 'maStateReasonData' @::@ 'Maybe' 'Text'
--
-- * 'maStateUpdatedTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'maStateValue' @::@ 'Maybe' 'StateValue'
--
-- * 'maStatistic' @::@ 'Maybe' 'Statistic'
--
-- * 'maThreshold' @::@ 'Maybe' 'Double'
--
-- * 'maUnit' @::@ 'Maybe' 'StandardUnit'
--
metricAlarm :: MetricAlarm
metricAlarm = MetricAlarm
    { _maAlarmName                          = Nothing
    , _maAlarmArn                           = Nothing
    , _maAlarmDescription                   = Nothing
    , _maAlarmConfigurationUpdatedTimestamp = Nothing
    , _maActionsEnabled                     = Nothing
    , _maOKActions                          = mempty
    , _maAlarmActions                       = mempty
    , _maInsufficientDataActions            = mempty
    , _maStateValue                         = Nothing
    , _maStateReason                        = Nothing
    , _maStateReasonData                    = Nothing
    , _maStateUpdatedTimestamp              = Nothing
    , _maMetricName                         = Nothing
    , _maNamespace                          = Nothing
    , _maStatistic                          = Nothing
    , _maDimensions                         = mempty
    , _maPeriod                             = Nothing
    , _maUnit                               = Nothing
    , _maEvaluationPeriods                  = Nothing
    , _maThreshold                          = Nothing
    , _maComparisonOperator                 = Nothing
    }

-- | Indicates whether actions should be executed during any changes to the
-- alarm's state.
maActionsEnabled :: Lens' MetricAlarm (Maybe Bool)
maActionsEnabled = lens _maActionsEnabled (\s a -> s { _maActionsEnabled = a })

-- | The list of actions to execute when this alarm transitions into an 'ALARM'
-- state from any other state. Each action is specified as an Amazon Resource
-- Number (ARN). Currently the only actions supported are publishing to an
-- Amazon SNS topic and triggering an Auto Scaling policy.
maAlarmActions :: Lens' MetricAlarm [Text]
maAlarmActions = lens _maAlarmActions (\s a -> s { _maAlarmActions = a }) . _List

-- | The Amazon Resource Name (ARN) of the alarm.
maAlarmArn :: Lens' MetricAlarm (Maybe Text)
maAlarmArn = lens _maAlarmArn (\s a -> s { _maAlarmArn = a })

-- | The time stamp of the last update to the alarm configuration. Amazon
-- CloudWatch uses Coordinated Universal Time (UTC) when returning time stamps,
-- which do not accommodate seasonal adjustments such as daylight savings time.
-- For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps> in the /Amazon CloudWatch Developer Guide/
-- .
maAlarmConfigurationUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maAlarmConfigurationUpdatedTimestamp =
    lens _maAlarmConfigurationUpdatedTimestamp
        (\s a -> s { _maAlarmConfigurationUpdatedTimestamp = a })
            . mapping _Time

-- | The description for the alarm.
maAlarmDescription :: Lens' MetricAlarm (Maybe Text)
maAlarmDescription =
    lens _maAlarmDescription (\s a -> s { _maAlarmDescription = a })

-- | The name of the alarm.
maAlarmName :: Lens' MetricAlarm (Maybe Text)
maAlarmName = lens _maAlarmName (\s a -> s { _maAlarmName = a })

-- | The arithmetic operation to use when comparing the specified 'Statistic' and 'Threshold'. The specified 'Statistic' value is used as the first operand.
maComparisonOperator :: Lens' MetricAlarm (Maybe ComparisonOperator)
maComparisonOperator =
    lens _maComparisonOperator (\s a -> s { _maComparisonOperator = a })

-- | The list of dimensions associated with the alarm's associated metric.
maDimensions :: Lens' MetricAlarm [Dimension]
maDimensions = lens _maDimensions (\s a -> s { _maDimensions = a }) . _List

-- | The number of periods over which data is compared to the specified
-- threshold.
maEvaluationPeriods :: Lens' MetricAlarm (Maybe Natural)
maEvaluationPeriods =
    lens _maEvaluationPeriods (\s a -> s { _maEvaluationPeriods = a })
        . mapping _Nat

-- | The list of actions to execute when this alarm transitions into an 'INSUFFICIENT_DATA' state from any other state. Each action is specified as an Amazon Resource
-- Number (ARN). Currently the only actions supported are publishing to an
-- Amazon SNS topic or triggering an Auto Scaling policy.
--
-- The current WSDL lists this attribute as 'UnknownActions'.
maInsufficientDataActions :: Lens' MetricAlarm [Text]
maInsufficientDataActions =
    lens _maInsufficientDataActions
        (\s a -> s { _maInsufficientDataActions = a })
            . _List

-- | The name of the alarm's metric.
maMetricName :: Lens' MetricAlarm (Maybe Text)
maMetricName = lens _maMetricName (\s a -> s { _maMetricName = a })

-- | The namespace of alarm's associated metric.
maNamespace :: Lens' MetricAlarm (Maybe Text)
maNamespace = lens _maNamespace (\s a -> s { _maNamespace = a })

-- | The list of actions to execute when this alarm transitions into an 'OK' state
-- from any other state. Each action is specified as an Amazon Resource Number
-- (ARN). Currently the only actions supported are publishing to an Amazon SNS
-- topic and triggering an Auto Scaling policy.
maOKActions :: Lens' MetricAlarm [Text]
maOKActions = lens _maOKActions (\s a -> s { _maOKActions = a }) . _List

-- | The period in seconds over which the statistic is applied.
maPeriod :: Lens' MetricAlarm (Maybe Natural)
maPeriod = lens _maPeriod (\s a -> s { _maPeriod = a }) . mapping _Nat

-- | A human-readable explanation for the alarm's state.
maStateReason :: Lens' MetricAlarm (Maybe Text)
maStateReason = lens _maStateReason (\s a -> s { _maStateReason = a })

-- | An explanation for the alarm's state in machine-readable JSON format
maStateReasonData :: Lens' MetricAlarm (Maybe Text)
maStateReasonData =
    lens _maStateReasonData (\s a -> s { _maStateReasonData = a })

-- | The time stamp of the last update to the alarm's state. Amazon CloudWatch
-- uses Coordinated Universal Time (UTC) when returning time stamps, which do
-- not accommodate seasonal adjustments such as daylight savings time. For more
-- information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps> in the /Amazon CloudWatch Developer Guide/.
maStateUpdatedTimestamp :: Lens' MetricAlarm (Maybe UTCTime)
maStateUpdatedTimestamp =
    lens _maStateUpdatedTimestamp (\s a -> s { _maStateUpdatedTimestamp = a })
        . mapping _Time

-- | The state value for the alarm.
maStateValue :: Lens' MetricAlarm (Maybe StateValue)
maStateValue = lens _maStateValue (\s a -> s { _maStateValue = a })

-- | The statistic to apply to the alarm's associated metric.
maStatistic :: Lens' MetricAlarm (Maybe Statistic)
maStatistic = lens _maStatistic (\s a -> s { _maStatistic = a })

-- | The value against which the specified statistic is compared.
maThreshold :: Lens' MetricAlarm (Maybe Double)
maThreshold = lens _maThreshold (\s a -> s { _maThreshold = a })

-- | The unit of the alarm's associated metric.
maUnit :: Lens' MetricAlarm (Maybe StandardUnit)
maUnit = lens _maUnit (\s a -> s { _maUnit = a })

instance FromXML MetricAlarm where
    parseXML x = MetricAlarm
        <$> x .@? "ActionsEnabled"
        <*> x .@? "AlarmActions" .!@ mempty
        <*> x .@? "AlarmArn"
        <*> x .@? "AlarmConfigurationUpdatedTimestamp"
        <*> x .@? "AlarmDescription"
        <*> x .@? "AlarmName"
        <*> x .@? "ComparisonOperator"
        <*> x .@? "Dimensions" .!@ mempty
        <*> x .@? "EvaluationPeriods"
        <*> x .@? "InsufficientDataActions" .!@ mempty
        <*> x .@? "MetricName"
        <*> x .@? "Namespace"
        <*> x .@? "OKActions" .!@ mempty
        <*> x .@? "Period"
        <*> x .@? "StateReason"
        <*> x .@? "StateReasonData"
        <*> x .@? "StateUpdatedTimestamp"
        <*> x .@? "StateValue"
        <*> x .@? "Statistic"
        <*> x .@? "Threshold"
        <*> x .@? "Unit"

instance ToQuery MetricAlarm where
    toQuery MetricAlarm{..} = mconcat
        [ "ActionsEnabled"                     =? _maActionsEnabled
        , "AlarmActions"                       =? _maAlarmActions
        , "AlarmArn"                           =? _maAlarmArn
        , "AlarmConfigurationUpdatedTimestamp" =? _maAlarmConfigurationUpdatedTimestamp
        , "AlarmDescription"                   =? _maAlarmDescription
        , "AlarmName"                          =? _maAlarmName
        , "ComparisonOperator"                 =? _maComparisonOperator
        , "Dimensions"                         =? _maDimensions
        , "EvaluationPeriods"                  =? _maEvaluationPeriods
        , "InsufficientDataActions"            =? _maInsufficientDataActions
        , "MetricName"                         =? _maMetricName
        , "Namespace"                          =? _maNamespace
        , "OKActions"                          =? _maOKActions
        , "Period"                             =? _maPeriod
        , "StateReason"                        =? _maStateReason
        , "StateReasonData"                    =? _maStateReasonData
        , "StateUpdatedTimestamp"              =? _maStateUpdatedTimestamp
        , "StateValue"                         =? _maStateValue
        , "Statistic"                          =? _maStatistic
        , "Threshold"                          =? _maThreshold
        , "Unit"                               =? _maUnit
        ]

data HistoryItemType
    = Action              -- ^ Action
    | ConfigurationUpdate -- ^ ConfigurationUpdate
    | StateUpdate         -- ^ StateUpdate
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable HistoryItemType

instance FromText HistoryItemType where
    parser = takeLowerText >>= \case
        "action"              -> pure Action
        "configurationupdate" -> pure ConfigurationUpdate
        "stateupdate"         -> pure StateUpdate
        e                     -> fail $
            "Failure parsing HistoryItemType from " ++ show e

instance ToText HistoryItemType where
    toText = \case
        Action              -> "Action"
        ConfigurationUpdate -> "ConfigurationUpdate"
        StateUpdate         -> "StateUpdate"

instance ToByteString HistoryItemType
instance ToHeader     HistoryItemType
instance ToQuery      HistoryItemType

instance FromXML HistoryItemType where
    parseXML = parseXMLText "HistoryItemType"

data MetricDatum = MetricDatum
    { _mdDimensions      :: List "member" Dimension
    , _mdMetricName      :: Text
    , _mdStatisticValues :: Maybe StatisticSet
    , _mdTimestamp       :: Maybe ISO8601
    , _mdUnit            :: Maybe StandardUnit
    , _mdValue           :: Maybe Double
    } deriving (Eq, Read, Show)

-- | 'MetricDatum' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdDimensions' @::@ ['Dimension']
--
-- * 'mdMetricName' @::@ 'Text'
--
-- * 'mdStatisticValues' @::@ 'Maybe' 'StatisticSet'
--
-- * 'mdTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'mdUnit' @::@ 'Maybe' 'StandardUnit'
--
-- * 'mdValue' @::@ 'Maybe' 'Double'
--
metricDatum :: Text -- ^ 'mdMetricName'
            -> MetricDatum
metricDatum p1 = MetricDatum
    { _mdMetricName      = p1
    , _mdDimensions      = mempty
    , _mdTimestamp       = Nothing
    , _mdValue           = Nothing
    , _mdStatisticValues = Nothing
    , _mdUnit            = Nothing
    }

-- | A list of dimensions associated with the metric. Note, when using the
-- Dimensions value in a query, you need to append .member.N to it (e.g.,
-- Dimensions.member.N).
mdDimensions :: Lens' MetricDatum [Dimension]
mdDimensions = lens _mdDimensions (\s a -> s { _mdDimensions = a }) . _List

-- | The name of the metric.
mdMetricName :: Lens' MetricDatum Text
mdMetricName = lens _mdMetricName (\s a -> s { _mdMetricName = a })

-- | A set of statistical values describing the metric.
mdStatisticValues :: Lens' MetricDatum (Maybe StatisticSet)
mdStatisticValues =
    lens _mdStatisticValues (\s a -> s { _mdStatisticValues = a })

-- | The time stamp used for the metric. If not specified, the default value is
-- set to the time the metric data was received. Amazon CloudWatch uses
-- Coordinated Universal Time (UTC) when returning time stamps, which do not
-- accommodate seasonal adjustments such as daylight savings time. For more
-- information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps> in the /Amazon CloudWatch Developer Guide/.
mdTimestamp :: Lens' MetricDatum (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\s a -> s { _mdTimestamp = a }) . mapping _Time

-- | The unit of the metric.
mdUnit :: Lens' MetricDatum (Maybe StandardUnit)
mdUnit = lens _mdUnit (\s a -> s { _mdUnit = a })

-- | The value for the metric.
--
-- Although the 'Value' parameter accepts numbers of type 'Double', Amazon
-- CloudWatch truncates values with very large exponents. Values with base-10
-- exponents greater than 126 (1 x 10^126) are truncated. Likewise, values with
-- base-10 exponents less than -130 (1 x 10^-130) are also truncated.
mdValue :: Lens' MetricDatum (Maybe Double)
mdValue = lens _mdValue (\s a -> s { _mdValue = a })

instance FromXML MetricDatum where
    parseXML x = MetricDatum
        <$> x .@? "Dimensions" .!@ mempty
        <*> x .@  "MetricName"
        <*> x .@? "StatisticValues"
        <*> x .@? "Timestamp"
        <*> x .@? "Unit"
        <*> x .@? "Value"

instance ToQuery MetricDatum where
    toQuery MetricDatum{..} = mconcat
        [ "Dimensions"      =? _mdDimensions
        , "MetricName"      =? _mdMetricName
        , "StatisticValues" =? _mdStatisticValues
        , "Timestamp"       =? _mdTimestamp
        , "Unit"            =? _mdUnit
        , "Value"           =? _mdValue
        ]

data StandardUnit
    = Bits            -- ^ Bits
    | BitsSecond      -- ^ Bits/Second
    | Bytes           -- ^ Bytes
    | BytesSecond     -- ^ Bytes/Second
    | Count           -- ^ Count
    | CountSecond     -- ^ Count/Second
    | Gigabits        -- ^ Gigabits
    | GigabitsSecond  -- ^ Gigabits/Second
    | Gigabytes       -- ^ Gigabytes
    | GigabytesSecond -- ^ Gigabytes/Second
    | Kilobits        -- ^ Kilobits
    | KilobitsSecond  -- ^ Kilobits/Second
    | Kilobytes       -- ^ Kilobytes
    | KilobytesSecond -- ^ Kilobytes/Second
    | Megabits        -- ^ Megabits
    | MegabitsSecond  -- ^ Megabits/Second
    | Megabytes       -- ^ Megabytes
    | MegabytesSecond -- ^ Megabytes/Second
    | Microseconds    -- ^ Microseconds
    | Milliseconds    -- ^ Milliseconds
    | None            -- ^ None
    | Percent         -- ^ Percent
    | Seconds         -- ^ Seconds
    | Terabits        -- ^ Terabits
    | TerabitsSecond  -- ^ Terabits/Second
    | Terabytes       -- ^ Terabytes
    | TerabytesSecond -- ^ Terabytes/Second
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StandardUnit

instance FromText StandardUnit where
    parser = takeLowerText >>= \case
        "bits"             -> pure Bits
        "bits/second"      -> pure BitsSecond
        "bytes"            -> pure Bytes
        "bytes/second"     -> pure BytesSecond
        "count"            -> pure Count
        "count/second"     -> pure CountSecond
        "gigabits"         -> pure Gigabits
        "gigabits/second"  -> pure GigabitsSecond
        "gigabytes"        -> pure Gigabytes
        "gigabytes/second" -> pure GigabytesSecond
        "kilobits"         -> pure Kilobits
        "kilobits/second"  -> pure KilobitsSecond
        "kilobytes"        -> pure Kilobytes
        "kilobytes/second" -> pure KilobytesSecond
        "megabits"         -> pure Megabits
        "megabits/second"  -> pure MegabitsSecond
        "megabytes"        -> pure Megabytes
        "megabytes/second" -> pure MegabytesSecond
        "microseconds"     -> pure Microseconds
        "milliseconds"     -> pure Milliseconds
        "none"             -> pure None
        "percent"          -> pure Percent
        "seconds"          -> pure Seconds
        "terabits"         -> pure Terabits
        "terabits/second"  -> pure TerabitsSecond
        "terabytes"        -> pure Terabytes
        "terabytes/second" -> pure TerabytesSecond
        e                  -> fail $
            "Failure parsing StandardUnit from " ++ show e

instance ToText StandardUnit where
    toText = \case
        Bits            -> "Bits"
        BitsSecond      -> "Bits/Second"
        Bytes           -> "Bytes"
        BytesSecond     -> "Bytes/Second"
        Count           -> "Count"
        CountSecond     -> "Count/Second"
        Gigabits        -> "Gigabits"
        GigabitsSecond  -> "Gigabits/Second"
        Gigabytes       -> "Gigabytes"
        GigabytesSecond -> "Gigabytes/Second"
        Kilobits        -> "Kilobits"
        KilobitsSecond  -> "Kilobits/Second"
        Kilobytes       -> "Kilobytes"
        KilobytesSecond -> "Kilobytes/Second"
        Megabits        -> "Megabits"
        MegabitsSecond  -> "Megabits/Second"
        Megabytes       -> "Megabytes"
        MegabytesSecond -> "Megabytes/Second"
        Microseconds    -> "Microseconds"
        Milliseconds    -> "Milliseconds"
        None            -> "None"
        Percent         -> "Percent"
        Seconds         -> "Seconds"
        Terabits        -> "Terabits"
        TerabitsSecond  -> "Terabits/Second"
        Terabytes       -> "Terabytes"
        TerabytesSecond -> "Terabytes/Second"

instance ToByteString StandardUnit
instance ToHeader     StandardUnit
instance ToQuery      StandardUnit

instance FromXML StandardUnit where
    parseXML = parseXMLText "StandardUnit"

data Dimension = Dimension
    { _dName  :: Text
    , _dValue :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Dimension' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dName' @::@ 'Text'
--
-- * 'dValue' @::@ 'Text'
--
dimension :: Text -- ^ 'dName'
          -> Text -- ^ 'dValue'
          -> Dimension
dimension p1 p2 = Dimension
    { _dName  = p1
    , _dValue = p2
    }

-- | The name of the dimension.
dName :: Lens' Dimension Text
dName = lens _dName (\s a -> s { _dName = a })

-- | The value representing the dimension measurement
dValue :: Lens' Dimension Text
dValue = lens _dValue (\s a -> s { _dValue = a })

instance FromXML Dimension where
    parseXML x = Dimension
        <$> x .@  "Name"
        <*> x .@  "Value"

instance ToQuery Dimension where
    toQuery Dimension{..} = mconcat
        [ "Name"  =? _dName
        , "Value" =? _dValue
        ]

data ComparisonOperator
    = GreaterThanOrEqualToThreshold -- ^ GreaterThanOrEqualToThreshold
    | GreaterThanThreshold          -- ^ GreaterThanThreshold
    | LessThanOrEqualToThreshold    -- ^ LessThanOrEqualToThreshold
    | LessThanThreshold             -- ^ LessThanThreshold
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ComparisonOperator

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "greaterthanorequaltothreshold" -> pure GreaterThanOrEqualToThreshold
        "greaterthanthreshold"          -> pure GreaterThanThreshold
        "lessthanorequaltothreshold"    -> pure LessThanOrEqualToThreshold
        "lessthanthreshold"             -> pure LessThanThreshold
        e                               -> fail $
            "Failure parsing ComparisonOperator from " ++ show e

instance ToText ComparisonOperator where
    toText = \case
        GreaterThanOrEqualToThreshold -> "GreaterThanOrEqualToThreshold"
        GreaterThanThreshold          -> "GreaterThanThreshold"
        LessThanOrEqualToThreshold    -> "LessThanOrEqualToThreshold"
        LessThanThreshold             -> "LessThanThreshold"

instance ToByteString ComparisonOperator
instance ToHeader     ComparisonOperator
instance ToQuery      ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"

data AlarmHistoryItem = AlarmHistoryItem
    { _ahiAlarmName       :: Maybe Text
    , _ahiHistoryData     :: Maybe Text
    , _ahiHistoryItemType :: Maybe HistoryItemType
    , _ahiHistorySummary  :: Maybe Text
    , _ahiTimestamp       :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'AlarmHistoryItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ahiAlarmName' @::@ 'Maybe' 'Text'
--
-- * 'ahiHistoryData' @::@ 'Maybe' 'Text'
--
-- * 'ahiHistoryItemType' @::@ 'Maybe' 'HistoryItemType'
--
-- * 'ahiHistorySummary' @::@ 'Maybe' 'Text'
--
-- * 'ahiTimestamp' @::@ 'Maybe' 'UTCTime'
--
alarmHistoryItem :: AlarmHistoryItem
alarmHistoryItem = AlarmHistoryItem
    { _ahiAlarmName       = Nothing
    , _ahiTimestamp       = Nothing
    , _ahiHistoryItemType = Nothing
    , _ahiHistorySummary  = Nothing
    , _ahiHistoryData     = Nothing
    }

-- | The descriptive name for the alarm.
ahiAlarmName :: Lens' AlarmHistoryItem (Maybe Text)
ahiAlarmName = lens _ahiAlarmName (\s a -> s { _ahiAlarmName = a })

-- | Machine-readable data about the alarm in JSON format.
ahiHistoryData :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistoryData = lens _ahiHistoryData (\s a -> s { _ahiHistoryData = a })

-- | The type of alarm history item.
ahiHistoryItemType :: Lens' AlarmHistoryItem (Maybe HistoryItemType)
ahiHistoryItemType =
    lens _ahiHistoryItemType (\s a -> s { _ahiHistoryItemType = a })

-- | A human-readable summary of the alarm history.
ahiHistorySummary :: Lens' AlarmHistoryItem (Maybe Text)
ahiHistorySummary =
    lens _ahiHistorySummary (\s a -> s { _ahiHistorySummary = a })

-- | The time stamp for the alarm history item. Amazon CloudWatch uses
-- Coordinated Universal Time (UTC) when returning time stamps, which do not
-- accommodate seasonal adjustments such as daylight savings time. For more
-- information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps> in the /Amazon CloudWatch Developer Guide/.
ahiTimestamp :: Lens' AlarmHistoryItem (Maybe UTCTime)
ahiTimestamp = lens _ahiTimestamp (\s a -> s { _ahiTimestamp = a }) . mapping _Time

instance FromXML AlarmHistoryItem where
    parseXML x = AlarmHistoryItem
        <$> x .@? "AlarmName"
        <*> x .@? "HistoryData"
        <*> x .@? "HistoryItemType"
        <*> x .@? "HistorySummary"
        <*> x .@? "Timestamp"

instance ToQuery AlarmHistoryItem where
    toQuery AlarmHistoryItem{..} = mconcat
        [ "AlarmName"       =? _ahiAlarmName
        , "HistoryData"     =? _ahiHistoryData
        , "HistoryItemType" =? _ahiHistoryItemType
        , "HistorySummary"  =? _ahiHistorySummary
        , "Timestamp"       =? _ahiTimestamp
        ]

data Metric = Metric
    { _mDimensions :: List "member" Dimension
    , _mMetricName :: Maybe Text
    , _mNamespace  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Metric' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mDimensions' @::@ ['Dimension']
--
-- * 'mMetricName' @::@ 'Maybe' 'Text'
--
-- * 'mNamespace' @::@ 'Maybe' 'Text'
--
metric :: Metric
metric = Metric
    { _mNamespace  = Nothing
    , _mMetricName = Nothing
    , _mDimensions = mempty
    }

-- | A list of dimensions associated with the metric.
mDimensions :: Lens' Metric [Dimension]
mDimensions = lens _mDimensions (\s a -> s { _mDimensions = a }) . _List

-- | The name of the metric.
mMetricName :: Lens' Metric (Maybe Text)
mMetricName = lens _mMetricName (\s a -> s { _mMetricName = a })

-- | The namespace of the metric.
mNamespace :: Lens' Metric (Maybe Text)
mNamespace = lens _mNamespace (\s a -> s { _mNamespace = a })

instance FromXML Metric where
    parseXML x = Metric
        <$> x .@? "Dimensions" .!@ mempty
        <*> x .@? "MetricName"
        <*> x .@? "Namespace"

instance ToQuery Metric where
    toQuery Metric{..} = mconcat
        [ "Dimensions" =? _mDimensions
        , "MetricName" =? _mMetricName
        , "Namespace"  =? _mNamespace
        ]

data StateValue
    = Alarm            -- ^ ALARM
    | InsufficientData -- ^ INSUFFICIENT_DATA
    | Ok               -- ^ OK
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable StateValue

instance FromText StateValue where
    parser = takeLowerText >>= \case
        "alarm"             -> pure Alarm
        "insufficient_data" -> pure InsufficientData
        "ok"                -> pure Ok
        e                   -> fail $
            "Failure parsing StateValue from " ++ show e

instance ToText StateValue where
    toText = \case
        Alarm            -> "ALARM"
        InsufficientData -> "INSUFFICIENT_DATA"
        Ok               -> "OK"

instance ToByteString StateValue
instance ToHeader     StateValue
instance ToQuery      StateValue

instance FromXML StateValue where
    parseXML = parseXMLText "StateValue"

data Datapoint = Datapoint
    { _dAverage     :: Maybe Double
    , _dMaximum     :: Maybe Double
    , _dMinimum     :: Maybe Double
    , _dSampleCount :: Maybe Double
    , _dSum         :: Maybe Double
    , _dTimestamp   :: Maybe ISO8601
    , _dUnit        :: Maybe StandardUnit
    } deriving (Eq, Read, Show)

-- | 'Datapoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dAverage' @::@ 'Maybe' 'Double'
--
-- * 'dMaximum' @::@ 'Maybe' 'Double'
--
-- * 'dMinimum' @::@ 'Maybe' 'Double'
--
-- * 'dSampleCount' @::@ 'Maybe' 'Double'
--
-- * 'dSum' @::@ 'Maybe' 'Double'
--
-- * 'dTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'dUnit' @::@ 'Maybe' 'StandardUnit'
--
datapoint :: Datapoint
datapoint = Datapoint
    { _dTimestamp   = Nothing
    , _dSampleCount = Nothing
    , _dAverage     = Nothing
    , _dSum         = Nothing
    , _dMinimum     = Nothing
    , _dMaximum     = Nothing
    , _dUnit        = Nothing
    }

-- | The average of metric values that correspond to the datapoint.
dAverage :: Lens' Datapoint (Maybe Double)
dAverage = lens _dAverage (\s a -> s { _dAverage = a })

-- | The maximum of the metric value used for the datapoint.
dMaximum :: Lens' Datapoint (Maybe Double)
dMaximum = lens _dMaximum (\s a -> s { _dMaximum = a })

-- | The minimum metric value used for the datapoint.
dMinimum :: Lens' Datapoint (Maybe Double)
dMinimum = lens _dMinimum (\s a -> s { _dMinimum = a })

-- | The number of metric values that contributed to the aggregate value of this
-- datapoint.
dSampleCount :: Lens' Datapoint (Maybe Double)
dSampleCount = lens _dSampleCount (\s a -> s { _dSampleCount = a })

-- | The sum of metric values used for the datapoint.
dSum :: Lens' Datapoint (Maybe Double)
dSum = lens _dSum (\s a -> s { _dSum = a })

-- | The time stamp used for the datapoint. Amazon CloudWatch uses Coordinated
-- Universal Time (UTC) when returning time stamps, which do not accommodate
-- seasonal adjustments such as daylight savings time. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/cloudwatch_concepts.html#about_timestamp Time stamps> in the /Amazon CloudWatch Developer Guide/.
dTimestamp :: Lens' Datapoint (Maybe UTCTime)
dTimestamp = lens _dTimestamp (\s a -> s { _dTimestamp = a }) . mapping _Time

-- | The standard unit used for the datapoint.
dUnit :: Lens' Datapoint (Maybe StandardUnit)
dUnit = lens _dUnit (\s a -> s { _dUnit = a })

instance FromXML Datapoint where
    parseXML x = Datapoint
        <$> x .@? "Average"
        <*> x .@? "Maximum"
        <*> x .@? "Minimum"
        <*> x .@? "SampleCount"
        <*> x .@? "Sum"
        <*> x .@? "Timestamp"
        <*> x .@? "Unit"

instance ToQuery Datapoint where
    toQuery Datapoint{..} = mconcat
        [ "Average"     =? _dAverage
        , "Maximum"     =? _dMaximum
        , "Minimum"     =? _dMinimum
        , "SampleCount" =? _dSampleCount
        , "Sum"         =? _dSum
        , "Timestamp"   =? _dTimestamp
        , "Unit"        =? _dUnit
        ]

data DimensionFilter = DimensionFilter
    { _dfName  :: Text
    , _dfValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DimensionFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfName' @::@ 'Text'
--
-- * 'dfValue' @::@ 'Maybe' 'Text'
--
dimensionFilter :: Text -- ^ 'dfName'
                -> DimensionFilter
dimensionFilter p1 = DimensionFilter
    { _dfName  = p1
    , _dfValue = Nothing
    }

-- | The dimension name to be matched.
dfName :: Lens' DimensionFilter Text
dfName = lens _dfName (\s a -> s { _dfName = a })

-- | The value of the dimension to be matched.
dfValue :: Lens' DimensionFilter (Maybe Text)
dfValue = lens _dfValue (\s a -> s { _dfValue = a })

instance FromXML DimensionFilter where
    parseXML x = DimensionFilter
        <$> x .@  "Name"
        <*> x .@? "Value"

instance ToQuery DimensionFilter where
    toQuery DimensionFilter{..} = mconcat
        [ "Name"  =? _dfName
        , "Value" =? _dfValue
        ]

data Statistic
    = Average     -- ^ Average
    | Maximum     -- ^ Maximum
    | Minimum     -- ^ Minimum
    | SampleCount -- ^ SampleCount
    | Sum         -- ^ Sum
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Statistic

instance FromText Statistic where
    parser = takeLowerText >>= \case
        "average"     -> pure Average
        "maximum"     -> pure Maximum
        "minimum"     -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum"         -> pure Sum
        e             -> fail $
            "Failure parsing Statistic from " ++ show e

instance ToText Statistic where
    toText = \case
        Average     -> "Average"
        Maximum     -> "Maximum"
        Minimum     -> "Minimum"
        SampleCount -> "SampleCount"
        Sum         -> "Sum"

instance ToByteString Statistic
instance ToHeader     Statistic
instance ToQuery      Statistic

instance FromXML Statistic where
    parseXML = parseXMLText "Statistic"
