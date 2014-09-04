{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.PutMetricAlarm
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm. When this
-- operation creates an alarm, the alarm state is immediately set to
-- INSUFFICIENT_DATA. The alarm is evaluated and its StateValue is set
-- appropriately. Any actions associated with the StateValue is then executed.
-- When updating an existing alarm, its StateValue is left unchanged.
module Network.AWS.CloudWatch.V2010_08_01.PutMetricAlarm
    (
    -- * Request
      PutMetricAlarm
    -- ** Request constructor
    , putMetricAlarm
    -- ** Request lenses
    , pmaiAlarmName
    , pmaiComparisonOperator
    , pmaiEvaluationPeriods
    , pmaiMetricName
    , pmaiNamespace
    , pmaiPeriod
    , pmaiStatistic
    , pmaiThreshold
    , pmaiActionsEnabled
    , pmaiAlarmDescription
    , pmaiDimensions
    , pmaiOKActions
    , pmaiAlarmActions
    , pmaiInsufficientDataActions
    , pmaiUnit

    -- * Response
    , PutMetricAlarmResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutMetricAlarm' request.
putMetricAlarm :: Text -- ^ 'pmaiAlarmName'
               -> ComparisonOperator -- ^ 'pmaiComparisonOperator'
               -> Integer -- ^ 'pmaiEvaluationPeriods'
               -> Text -- ^ 'pmaiMetricName'
               -> Text -- ^ 'pmaiNamespace'
               -> Integer -- ^ 'pmaiPeriod'
               -> Statistic -- ^ 'pmaiStatistic'
               -> Double -- ^ 'pmaiThreshold'
               -> PutMetricAlarm
putMetricAlarm p1 p2 p3 p4 p5 p6 p7 p8 = PutMetricAlarm
    { _pmaiAlarmName = p1
    , _pmaiComparisonOperator = p2
    , _pmaiEvaluationPeriods = p3
    , _pmaiMetricName = p4
    , _pmaiNamespace = p5
    , _pmaiPeriod = p6
    , _pmaiStatistic = p7
    , _pmaiThreshold = p8
    , _pmaiActionsEnabled = Nothing
    , _pmaiAlarmDescription = Nothing
    , _pmaiDimensions = mempty
    , _pmaiOKActions = mempty
    , _pmaiAlarmActions = mempty
    , _pmaiInsufficientDataActions = mempty
    , _pmaiUnit = Nothing
    }
{-# INLINE putMetricAlarm #-}

data PutMetricAlarm = PutMetricAlarm
    { _pmaiAlarmName :: Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account.
    , _pmaiComparisonOperator :: ComparisonOperator
      -- ^ The arithmetic operation to use when comparing the specified
      -- Statistic and Threshold. The specified Statistic value is used as
      -- the first operand.
    , _pmaiEvaluationPeriods :: Integer
      -- ^ The number of periods over which data is compared to the
      -- specified threshold.
    , _pmaiMetricName :: Text
      -- ^ The name for the alarm's associated metric.
    , _pmaiNamespace :: Text
      -- ^ The namespace for the alarm's associated metric.
    , _pmaiPeriod :: Integer
      -- ^ The period in seconds over which the specified statistic is
      -- applied.
    , _pmaiStatistic :: Statistic
      -- ^ The statistic to apply to the alarm's associated metric.
    , _pmaiThreshold :: Double
      -- ^ The value against which the specified statistic is compared.
    , _pmaiActionsEnabled :: Maybe Bool
      -- ^ Indicates whether or not actions should be executed during any
      -- changes to the alarm's state.
    , _pmaiAlarmDescription :: Maybe Text
      -- ^ The description for the alarm.
    , _pmaiDimensions :: [Dimension]
      -- ^ The dimensions for the alarm's associated metric.
    , _pmaiOKActions :: [Text]
      -- ^ The list of actions to execute when this alarm transitions into
      -- an OK state from any other state. Each action is specified as an
      -- Amazon Resource Number (ARN). Currently the only action supported
      -- is publishing to an Amazon SNS topic or an Amazon Auto Scaling
      -- policy.
    , _pmaiAlarmActions :: [Text]
      -- ^ The list of actions to execute when this alarm transitions into
      -- an ALARM state from any other state. Each action is specified as
      -- an Amazon Resource Number (ARN). Currently the only action
      -- supported is publishing to an Amazon SNS topic or an Amazon Auto
      -- Scaling policy.
    , _pmaiInsufficientDataActions :: [Text]
      -- ^ The list of actions to execute when this alarm transitions into
      -- an INSUFFICIENT_DATA state from any other state. Each action is
      -- specified as an Amazon Resource Number (ARN). Currently the only
      -- action supported is publishing to an Amazon SNS topic or an
      -- Amazon Auto Scaling policy.
    , _pmaiUnit :: Maybe StandardUnit
      -- ^ The unit for the alarm's associated metric.
    } deriving (Show, Generic)

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account.
pmaiAlarmName :: Lens' PutMetricAlarm (Text)
pmaiAlarmName f x =
    f (_pmaiAlarmName x)
        <&> \y -> x { _pmaiAlarmName = y }
{-# INLINE pmaiAlarmName #-}

-- | The arithmetic operation to use when comparing the specified Statistic and
-- Threshold. The specified Statistic value is used as the first operand.
pmaiComparisonOperator :: Lens' PutMetricAlarm (ComparisonOperator)
pmaiComparisonOperator f x =
    f (_pmaiComparisonOperator x)
        <&> \y -> x { _pmaiComparisonOperator = y }
{-# INLINE pmaiComparisonOperator #-}

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaiEvaluationPeriods :: Lens' PutMetricAlarm (Integer)
pmaiEvaluationPeriods f x =
    f (_pmaiEvaluationPeriods x)
        <&> \y -> x { _pmaiEvaluationPeriods = y }
{-# INLINE pmaiEvaluationPeriods #-}

-- | The name for the alarm's associated metric.
pmaiMetricName :: Lens' PutMetricAlarm (Text)
pmaiMetricName f x =
    f (_pmaiMetricName x)
        <&> \y -> x { _pmaiMetricName = y }
{-# INLINE pmaiMetricName #-}

-- | The namespace for the alarm's associated metric.
pmaiNamespace :: Lens' PutMetricAlarm (Text)
pmaiNamespace f x =
    f (_pmaiNamespace x)
        <&> \y -> x { _pmaiNamespace = y }
{-# INLINE pmaiNamespace #-}

-- | The period in seconds over which the specified statistic is applied.
pmaiPeriod :: Lens' PutMetricAlarm (Integer)
pmaiPeriod f x =
    f (_pmaiPeriod x)
        <&> \y -> x { _pmaiPeriod = y }
{-# INLINE pmaiPeriod #-}

-- | The statistic to apply to the alarm's associated metric.
pmaiStatistic :: Lens' PutMetricAlarm (Statistic)
pmaiStatistic f x =
    f (_pmaiStatistic x)
        <&> \y -> x { _pmaiStatistic = y }
{-# INLINE pmaiStatistic #-}

-- | The value against which the specified statistic is compared.
pmaiThreshold :: Lens' PutMetricAlarm (Double)
pmaiThreshold f x =
    f (_pmaiThreshold x)
        <&> \y -> x { _pmaiThreshold = y }
{-# INLINE pmaiThreshold #-}

-- | Indicates whether or not actions should be executed during any changes to
-- the alarm's state.
pmaiActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaiActionsEnabled f x =
    f (_pmaiActionsEnabled x)
        <&> \y -> x { _pmaiActionsEnabled = y }
{-# INLINE pmaiActionsEnabled #-}

-- | The description for the alarm.
pmaiAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaiAlarmDescription f x =
    f (_pmaiAlarmDescription x)
        <&> \y -> x { _pmaiAlarmDescription = y }
{-# INLINE pmaiAlarmDescription #-}

-- | The dimensions for the alarm's associated metric.
pmaiDimensions :: Lens' PutMetricAlarm ([Dimension])
pmaiDimensions f x =
    f (_pmaiDimensions x)
        <&> \y -> x { _pmaiDimensions = y }
{-# INLINE pmaiDimensions #-}

-- | The list of actions to execute when this alarm transitions into an OK state
-- from any other state. Each action is specified as an Amazon Resource Number
-- (ARN). Currently the only action supported is publishing to an Amazon SNS
-- topic or an Amazon Auto Scaling policy.
pmaiOKActions :: Lens' PutMetricAlarm ([Text])
pmaiOKActions f x =
    f (_pmaiOKActions x)
        <&> \y -> x { _pmaiOKActions = y }
{-# INLINE pmaiOKActions #-}

-- | The list of actions to execute when this alarm transitions into an ALARM
-- state from any other state. Each action is specified as an Amazon Resource
-- Number (ARN). Currently the only action supported is publishing to an
-- Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiAlarmActions :: Lens' PutMetricAlarm ([Text])
pmaiAlarmActions f x =
    f (_pmaiAlarmActions x)
        <&> \y -> x { _pmaiAlarmActions = y }
{-# INLINE pmaiAlarmActions #-}

-- | The list of actions to execute when this alarm transitions into an
-- INSUFFICIENT_DATA state from any other state. Each action is specified as
-- an Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiInsufficientDataActions :: Lens' PutMetricAlarm ([Text])
pmaiInsufficientDataActions f x =
    f (_pmaiInsufficientDataActions x)
        <&> \y -> x { _pmaiInsufficientDataActions = y }
{-# INLINE pmaiInsufficientDataActions #-}

-- | The unit for the alarm's associated metric.
pmaiUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaiUnit f x =
    f (_pmaiUnit x)
        <&> \y -> x { _pmaiUnit = y }
{-# INLINE pmaiUnit #-}

instance ToQuery PutMetricAlarm where
    toQuery = genericQuery def

data PutMetricAlarmResponse = PutMetricAlarmResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutMetricAlarm where
    type Sv PutMetricAlarm = CloudWatch
    type Rs PutMetricAlarm = PutMetricAlarmResponse

    request = post "PutMetricAlarm"
    response _ = nullaryResponse PutMetricAlarmResponse
