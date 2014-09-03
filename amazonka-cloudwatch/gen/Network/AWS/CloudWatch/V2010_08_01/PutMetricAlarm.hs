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
pmaiAlarmName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiAlarmName f x =
    (\y -> x { _pmaiAlarmName = y })
       <$> f (_pmaiAlarmName x)
{-# INLINE pmaiAlarmName #-}

-- | The arithmetic operation to use when comparing the specified Statistic and
-- Threshold. The specified Statistic value is used as the first operand.
pmaiComparisonOperator
    :: Functor f
    => (ComparisonOperator
    -> f (ComparisonOperator))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiComparisonOperator f x =
    (\y -> x { _pmaiComparisonOperator = y })
       <$> f (_pmaiComparisonOperator x)
{-# INLINE pmaiComparisonOperator #-}

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaiEvaluationPeriods
    :: Functor f
    => (Integer
    -> f (Integer))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiEvaluationPeriods f x =
    (\y -> x { _pmaiEvaluationPeriods = y })
       <$> f (_pmaiEvaluationPeriods x)
{-# INLINE pmaiEvaluationPeriods #-}

-- | The name for the alarm's associated metric.
pmaiMetricName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiMetricName f x =
    (\y -> x { _pmaiMetricName = y })
       <$> f (_pmaiMetricName x)
{-# INLINE pmaiMetricName #-}

-- | The namespace for the alarm's associated metric.
pmaiNamespace
    :: Functor f
    => (Text
    -> f (Text))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiNamespace f x =
    (\y -> x { _pmaiNamespace = y })
       <$> f (_pmaiNamespace x)
{-# INLINE pmaiNamespace #-}

-- | The period in seconds over which the specified statistic is applied.
pmaiPeriod
    :: Functor f
    => (Integer
    -> f (Integer))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiPeriod f x =
    (\y -> x { _pmaiPeriod = y })
       <$> f (_pmaiPeriod x)
{-# INLINE pmaiPeriod #-}

-- | The statistic to apply to the alarm's associated metric.
pmaiStatistic
    :: Functor f
    => (Statistic
    -> f (Statistic))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiStatistic f x =
    (\y -> x { _pmaiStatistic = y })
       <$> f (_pmaiStatistic x)
{-# INLINE pmaiStatistic #-}

-- | The value against which the specified statistic is compared.
pmaiThreshold
    :: Functor f
    => (Double
    -> f (Double))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiThreshold f x =
    (\y -> x { _pmaiThreshold = y })
       <$> f (_pmaiThreshold x)
{-# INLINE pmaiThreshold #-}

-- | Indicates whether or not actions should be executed during any changes to
-- the alarm's state.
pmaiActionsEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiActionsEnabled f x =
    (\y -> x { _pmaiActionsEnabled = y })
       <$> f (_pmaiActionsEnabled x)
{-# INLINE pmaiActionsEnabled #-}

-- | The description for the alarm.
pmaiAlarmDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiAlarmDescription f x =
    (\y -> x { _pmaiAlarmDescription = y })
       <$> f (_pmaiAlarmDescription x)
{-# INLINE pmaiAlarmDescription #-}

-- | The dimensions for the alarm's associated metric.
pmaiDimensions
    :: Functor f
    => ([Dimension]
    -> f ([Dimension]))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiDimensions f x =
    (\y -> x { _pmaiDimensions = y })
       <$> f (_pmaiDimensions x)
{-# INLINE pmaiDimensions #-}

-- | The list of actions to execute when this alarm transitions into an OK state
-- from any other state. Each action is specified as an Amazon Resource Number
-- (ARN). Currently the only action supported is publishing to an Amazon SNS
-- topic or an Amazon Auto Scaling policy.
pmaiOKActions
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiOKActions f x =
    (\y -> x { _pmaiOKActions = y })
       <$> f (_pmaiOKActions x)
{-# INLINE pmaiOKActions #-}

-- | The list of actions to execute when this alarm transitions into an ALARM
-- state from any other state. Each action is specified as an Amazon Resource
-- Number (ARN). Currently the only action supported is publishing to an
-- Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiAlarmActions
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiAlarmActions f x =
    (\y -> x { _pmaiAlarmActions = y })
       <$> f (_pmaiAlarmActions x)
{-# INLINE pmaiAlarmActions #-}

-- | The list of actions to execute when this alarm transitions into an
-- INSUFFICIENT_DATA state from any other state. Each action is specified as
-- an Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiInsufficientDataActions
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiInsufficientDataActions f x =
    (\y -> x { _pmaiInsufficientDataActions = y })
       <$> f (_pmaiInsufficientDataActions x)
{-# INLINE pmaiInsufficientDataActions #-}

-- | The unit for the alarm's associated metric.
pmaiUnit
    :: Functor f
    => (Maybe StandardUnit
    -> f (Maybe StandardUnit))
    -> PutMetricAlarm
    -> f PutMetricAlarm
pmaiUnit f x =
    (\y -> x { _pmaiUnit = y })
       <$> f (_pmaiUnit x)
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
