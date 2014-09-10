{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch
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
module Network.AWS.CloudWatch
    (
    -- * Request
      PutMetricAlarm
    -- ** Request constructor
    , mkPutMetricAlarm
    -- ** Request lenses
    , pmaAlarmName
    , pmaAlarmDescription
    , pmaActionsEnabled
    , pmaOKActions
    , pmaAlarmActions
    , pmaInsufficientDataActions
    , pmaMetricName
    , pmaNamespace
    , pmaStatistic
    , pmaDimensions
    , pmaPeriod
    , pmaUnit
    , pmaEvaluationPeriods
    , pmaThreshold
    , pmaComparisonOperator

    -- * Response
    , PutMetricAlarmResponse
    -- ** Response constructor
    , mkPutMetricAlarmResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude

data PutMetricAlarm = PutMetricAlarm
    { _pmaAlarmName :: !Text
    , _pmaAlarmDescription :: !(Maybe Text)
    , _pmaActionsEnabled :: !(Maybe Bool)
    , _pmaOKActions :: [Text]
    , _pmaAlarmActions :: [Text]
    , _pmaInsufficientDataActions :: [Text]
    , _pmaMetricName :: !Text
    , _pmaNamespace :: !Text
    , _pmaStatistic :: Statistic
    , _pmaDimensions :: [Dimension]
    , _pmaPeriod :: !Integer
    , _pmaUnit :: Maybe StandardUnit
    , _pmaEvaluationPeriods :: !Integer
    , _pmaThreshold :: !Double
    , _pmaComparisonOperator :: ComparisonOperator
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricAlarm' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AlarmName ::@ @Text@
--
-- * @AlarmDescription ::@ @Maybe Text@
--
-- * @ActionsEnabled ::@ @Maybe Bool@
--
-- * @OKActions ::@ @[Text]@
--
-- * @AlarmActions ::@ @[Text]@
--
-- * @InsufficientDataActions ::@ @[Text]@
--
-- * @MetricName ::@ @Text@
--
-- * @Namespace ::@ @Text@
--
-- * @Statistic ::@ @Statistic@
--
-- * @Dimensions ::@ @[Dimension]@
--
-- * @Period ::@ @Integer@
--
-- * @Unit ::@ @Maybe StandardUnit@
--
-- * @EvaluationPeriods ::@ @Integer@
--
-- * @Threshold ::@ @Double@
--
-- * @ComparisonOperator ::@ @ComparisonOperator@
--
mkPutMetricAlarm :: Text -- ^ 'pmaAlarmName'
                 -> Integer -- ^ 'pmaPeriod'
                 -> Integer -- ^ 'pmaEvaluationPeriods'
                 -> Double -- ^ 'pmaThreshold'
                 -> ComparisonOperator -- ^ 'pmaComparisonOperator'
                 -> Text -- ^ 'pmaMetricName'
                 -> Text -- ^ 'pmaNamespace'
                 -> Statistic -- ^ 'pmaStatistic'
                 -> PutMetricAlarm
mkPutMetricAlarm p1 p11 p13 p14 p15 p7 p8 p9 = PutMetricAlarm
    { _pmaAlarmName = p1
    , _pmaAlarmDescription = Nothing
    , _pmaActionsEnabled = Nothing
    , _pmaOKActions = mempty
    , _pmaAlarmActions = mempty
    , _pmaInsufficientDataActions = mempty
    , _pmaMetricName = p7
    , _pmaNamespace = p8
    , _pmaStatistic = p9
    , _pmaDimensions = mempty
    , _pmaPeriod = p11
    , _pmaUnit = Nothing
    , _pmaEvaluationPeriods = p13
    , _pmaThreshold = p14
    , _pmaComparisonOperator = p15
    }

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account.
pmaAlarmName :: Lens' PutMetricAlarm Text
pmaAlarmName = lens _pmaAlarmName (\s a -> s { _pmaAlarmName = a })

-- | The description for the alarm.
pmaAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaAlarmDescription =
    lens _pmaAlarmDescription (\s a -> s { _pmaAlarmDescription = a })

-- | Indicates whether or not actions should be executed during any changes to
-- the alarm's state.
pmaActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaActionsEnabled =
    lens _pmaActionsEnabled (\s a -> s { _pmaActionsEnabled = a })

-- | The list of actions to execute when this alarm transitions into an OK state
-- from any other state. Each action is specified as an Amazon Resource Number
-- (ARN). Currently the only action supported is publishing to an Amazon SNS
-- topic or an Amazon Auto Scaling policy.
pmaOKActions :: Lens' PutMetricAlarm [Text]
pmaOKActions = lens _pmaOKActions (\s a -> s { _pmaOKActions = a })

-- | The list of actions to execute when this alarm transitions into an ALARM
-- state from any other state. Each action is specified as an Amazon Resource
-- Number (ARN). Currently the only action supported is publishing to an
-- Amazon SNS topic or an Amazon Auto Scaling policy.
pmaAlarmActions :: Lens' PutMetricAlarm [Text]
pmaAlarmActions = lens _pmaAlarmActions (\s a -> s { _pmaAlarmActions = a })

-- | The list of actions to execute when this alarm transitions into an
-- INSUFFICIENT_DATA state from any other state. Each action is specified as
-- an Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmaInsufficientDataActions =
    lens _pmaInsufficientDataActions
         (\s a -> s { _pmaInsufficientDataActions = a })

-- | The name for the alarm's associated metric.
pmaMetricName :: Lens' PutMetricAlarm Text
pmaMetricName = lens _pmaMetricName (\s a -> s { _pmaMetricName = a })

-- | The namespace for the alarm's associated metric.
pmaNamespace :: Lens' PutMetricAlarm Text
pmaNamespace = lens _pmaNamespace (\s a -> s { _pmaNamespace = a })

-- | The statistic to apply to the alarm's associated metric.
pmaStatistic :: Lens' PutMetricAlarm Statistic
pmaStatistic = lens _pmaStatistic (\s a -> s { _pmaStatistic = a })

-- | The dimensions for the alarm's associated metric.
pmaDimensions :: Lens' PutMetricAlarm [Dimension]
pmaDimensions = lens _pmaDimensions (\s a -> s { _pmaDimensions = a })

-- | The period in seconds over which the specified statistic is applied.
pmaPeriod :: Lens' PutMetricAlarm Integer
pmaPeriod = lens _pmaPeriod (\s a -> s { _pmaPeriod = a })

-- | The unit for the alarm's associated metric.
pmaUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaUnit = lens _pmaUnit (\s a -> s { _pmaUnit = a })

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaEvaluationPeriods :: Lens' PutMetricAlarm Integer
pmaEvaluationPeriods =
    lens _pmaEvaluationPeriods (\s a -> s { _pmaEvaluationPeriods = a })

-- | The value against which the specified statistic is compared.
pmaThreshold :: Lens' PutMetricAlarm Double
pmaThreshold = lens _pmaThreshold (\s a -> s { _pmaThreshold = a })

-- | The arithmetic operation to use when comparing the specified Statistic and
-- Threshold. The specified Statistic value is used as the first operand.
pmaComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator =
    lens _pmaComparisonOperator (\s a -> s { _pmaComparisonOperator = a })

instance ToQuery PutMetricAlarm where
    toQuery = genericQuery def

data PutMetricAlarmResponse = PutMetricAlarmResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutMetricAlarmResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutMetricAlarmResponse :: PutMetricAlarmResponse
mkPutMetricAlarmResponse = PutMetricAlarmResponse

instance AWSRequest PutMetricAlarm where
    type Sv PutMetricAlarm = CloudWatch
    type Rs PutMetricAlarm = PutMetricAlarmResponse

    request = post "PutMetricAlarm"
    response _ = nullaryResponse PutMetricAlarmResponse
