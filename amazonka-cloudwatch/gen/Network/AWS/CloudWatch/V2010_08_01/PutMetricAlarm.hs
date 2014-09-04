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
    , mkPutMetricAlarmInput
    -- ** Request lenses
    , pmaiAlarmName
    , pmaiAlarmDescription
    , pmaiActionsEnabled
    , pmaiOKActions
    , pmaiAlarmActions
    , pmaiInsufficientDataActions
    , pmaiMetricName
    , pmaiNamespace
    , pmaiStatistic
    , pmaiDimensions
    , pmaiPeriod
    , pmaiUnit
    , pmaiEvaluationPeriods
    , pmaiThreshold
    , pmaiComparisonOperator

    -- * Response
    , PutMetricAlarmResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricAlarm' request.
mkPutMetricAlarmInput :: Text -- ^ 'pmaiAlarmName'
                      -> Text -- ^ 'pmaiMetricName'
                      -> Text -- ^ 'pmaiNamespace'
                      -> Statistic -- ^ 'pmaiStatistic'
                      -> Integer -- ^ 'pmaiPeriod'
                      -> Integer -- ^ 'pmaiEvaluationPeriods'
                      -> Double -- ^ 'pmaiThreshold'
                      -> ComparisonOperator -- ^ 'pmaiComparisonOperator'
                      -> PutMetricAlarm
mkPutMetricAlarmInput p1 p2 p3 p4 p5 p6 p7 p8 = PutMetricAlarm
    { _pmaiAlarmName = p1
    , _pmaiAlarmDescription = Nothing
    , _pmaiActionsEnabled = Nothing
    , _pmaiOKActions = mempty
    , _pmaiAlarmActions = mempty
    , _pmaiInsufficientDataActions = mempty
    , _pmaiMetricName = p7
    , _pmaiNamespace = p8
    , _pmaiStatistic = p9
    , _pmaiDimensions = mempty
    , _pmaiPeriod = p11
    , _pmaiUnit = Nothing
    , _pmaiEvaluationPeriods = p13
    , _pmaiThreshold = p14
    , _pmaiComparisonOperator = p15
    }
{-# INLINE mkPutMetricAlarmInput #-}

data PutMetricAlarm = PutMetricAlarm
    { _pmaiAlarmName :: Text
      -- ^ The descriptive name for the alarm. This name must be unique
      -- within the user's AWS account.
    , _pmaiAlarmDescription :: Maybe Text
      -- ^ The description for the alarm.
    , _pmaiActionsEnabled :: Maybe Bool
      -- ^ Indicates whether or not actions should be executed during any
      -- changes to the alarm's state.
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
    , _pmaiMetricName :: Text
      -- ^ The name for the alarm's associated metric.
    , _pmaiNamespace :: Text
      -- ^ The namespace for the alarm's associated metric.
    , _pmaiStatistic :: Statistic
      -- ^ The statistic to apply to the alarm's associated metric.
    , _pmaiDimensions :: [Dimension]
      -- ^ The dimensions for the alarm's associated metric.
    , _pmaiPeriod :: Integer
      -- ^ The period in seconds over which the specified statistic is
      -- applied.
    , _pmaiUnit :: Maybe StandardUnit
      -- ^ The unit for the alarm's associated metric.
    , _pmaiEvaluationPeriods :: Integer
      -- ^ The number of periods over which data is compared to the
      -- specified threshold.
    , _pmaiThreshold :: Double
      -- ^ The value against which the specified statistic is compared.
    , _pmaiComparisonOperator :: ComparisonOperator
      -- ^ The arithmetic operation to use when comparing the specified
      -- Statistic and Threshold. The specified Statistic value is used as
      -- the first operand.
    } deriving (Show, Generic)

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account.
pmaiAlarmName :: Lens' PutMetricAlarm (Text)
pmaiAlarmName = lens _pmaiAlarmName (\s a -> s { _pmaiAlarmName = a })
{-# INLINE pmaiAlarmName #-}

-- | The description for the alarm.
pmaiAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaiAlarmDescription = lens _pmaiAlarmDescription (\s a -> s { _pmaiAlarmDescription = a })
{-# INLINE pmaiAlarmDescription #-}

-- | Indicates whether or not actions should be executed during any changes to
-- the alarm's state.
pmaiActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaiActionsEnabled = lens _pmaiActionsEnabled (\s a -> s { _pmaiActionsEnabled = a })
{-# INLINE pmaiActionsEnabled #-}

-- | The list of actions to execute when this alarm transitions into an OK state
-- from any other state. Each action is specified as an Amazon Resource Number
-- (ARN). Currently the only action supported is publishing to an Amazon SNS
-- topic or an Amazon Auto Scaling policy.
pmaiOKActions :: Lens' PutMetricAlarm ([Text])
pmaiOKActions = lens _pmaiOKActions (\s a -> s { _pmaiOKActions = a })
{-# INLINE pmaiOKActions #-}

-- | The list of actions to execute when this alarm transitions into an ALARM
-- state from any other state. Each action is specified as an Amazon Resource
-- Number (ARN). Currently the only action supported is publishing to an
-- Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiAlarmActions :: Lens' PutMetricAlarm ([Text])
pmaiAlarmActions = lens _pmaiAlarmActions (\s a -> s { _pmaiAlarmActions = a })
{-# INLINE pmaiAlarmActions #-}

-- | The list of actions to execute when this alarm transitions into an
-- INSUFFICIENT_DATA state from any other state. Each action is specified as
-- an Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiInsufficientDataActions :: Lens' PutMetricAlarm ([Text])
pmaiInsufficientDataActions = lens _pmaiInsufficientDataActions (\s a -> s { _pmaiInsufficientDataActions = a })
{-# INLINE pmaiInsufficientDataActions #-}

-- | The name for the alarm's associated metric.
pmaiMetricName :: Lens' PutMetricAlarm (Text)
pmaiMetricName = lens _pmaiMetricName (\s a -> s { _pmaiMetricName = a })
{-# INLINE pmaiMetricName #-}

-- | The namespace for the alarm's associated metric.
pmaiNamespace :: Lens' PutMetricAlarm (Text)
pmaiNamespace = lens _pmaiNamespace (\s a -> s { _pmaiNamespace = a })
{-# INLINE pmaiNamespace #-}

-- | The statistic to apply to the alarm's associated metric.
pmaiStatistic :: Lens' PutMetricAlarm (Statistic)
pmaiStatistic = lens _pmaiStatistic (\s a -> s { _pmaiStatistic = a })
{-# INLINE pmaiStatistic #-}

-- | The dimensions for the alarm's associated metric.
pmaiDimensions :: Lens' PutMetricAlarm ([Dimension])
pmaiDimensions = lens _pmaiDimensions (\s a -> s { _pmaiDimensions = a })
{-# INLINE pmaiDimensions #-}

-- | The period in seconds over which the specified statistic is applied.
pmaiPeriod :: Lens' PutMetricAlarm (Integer)
pmaiPeriod = lens _pmaiPeriod (\s a -> s { _pmaiPeriod = a })
{-# INLINE pmaiPeriod #-}

-- | The unit for the alarm's associated metric.
pmaiUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaiUnit = lens _pmaiUnit (\s a -> s { _pmaiUnit = a })
{-# INLINE pmaiUnit #-}

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaiEvaluationPeriods :: Lens' PutMetricAlarm (Integer)
pmaiEvaluationPeriods = lens _pmaiEvaluationPeriods (\s a -> s { _pmaiEvaluationPeriods = a })
{-# INLINE pmaiEvaluationPeriods #-}

-- | The value against which the specified statistic is compared.
pmaiThreshold :: Lens' PutMetricAlarm (Double)
pmaiThreshold = lens _pmaiThreshold (\s a -> s { _pmaiThreshold = a })
{-# INLINE pmaiThreshold #-}

-- | The arithmetic operation to use when comparing the specified Statistic and
-- Threshold. The specified Statistic value is used as the first operand.
pmaiComparisonOperator :: Lens' PutMetricAlarm (ComparisonOperator)
pmaiComparisonOperator = lens _pmaiComparisonOperator (\s a -> s { _pmaiComparisonOperator = a })
{-# INLINE pmaiComparisonOperator #-}

instance ToQuery PutMetricAlarm where
    toQuery = genericQuery def

data PutMetricAlarmResponse = PutMetricAlarmResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutMetricAlarm where
    type Sv PutMetricAlarm = CloudWatch
    type Rs PutMetricAlarm = PutMetricAlarmResponse

    request = post "PutMetricAlarm"
    response _ = nullaryResponse PutMetricAlarmResponse
