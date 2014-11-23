{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.PutMetricAlarm
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
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricAlarm.html>
module Network.AWS.CloudWatch.PutMetricAlarm
    (
    -- * Request
      PutMetricAlarm
    -- ** Request constructor
    , putMetricAlarm
    -- ** Request lenses
    , pmaActionsEnabled
    , pmaAlarmActions
    , pmaAlarmDescription
    , pmaAlarmName
    , pmaComparisonOperator
    , pmaDimensions
    , pmaEvaluationPeriods
    , pmaInsufficientDataActions
    , pmaMetricName
    , pmaNamespace
    , pmaOKActions
    , pmaPeriod
    , pmaStatistic
    , pmaThreshold
    , pmaUnit

    -- * Response
    , PutMetricAlarmResponse
    -- ** Response constructor
    , putMetricAlarmResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

data PutMetricAlarm = PutMetricAlarm
    { _pmaActionsEnabled          :: Maybe Bool
    , _pmaAlarmActions            :: List "OKActions" Text
    , _pmaAlarmDescription        :: Maybe Text
    , _pmaAlarmName               :: Text
    , _pmaComparisonOperator      :: ComparisonOperator
    , _pmaDimensions              :: List "Dimensions" Dimension
    , _pmaEvaluationPeriods       :: Nat
    , _pmaInsufficientDataActions :: List "OKActions" Text
    , _pmaMetricName              :: Text
    , _pmaNamespace               :: Text
    , _pmaOKActions               :: List "OKActions" Text
    , _pmaPeriod                  :: Nat
    , _pmaStatistic               :: Statistic
    , _pmaThreshold               :: Double
    , _pmaUnit                    :: Maybe StandardUnit
    } deriving (Eq, Show)

-- | 'PutMetricAlarm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmaActionsEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'pmaAlarmActions' @::@ ['Text']
--
-- * 'pmaAlarmDescription' @::@ 'Maybe' 'Text'
--
-- * 'pmaAlarmName' @::@ 'Text'
--
-- * 'pmaComparisonOperator' @::@ 'ComparisonOperator'
--
-- * 'pmaDimensions' @::@ ['Dimension']
--
-- * 'pmaEvaluationPeriods' @::@ 'Natural'
--
-- * 'pmaInsufficientDataActions' @::@ ['Text']
--
-- * 'pmaMetricName' @::@ 'Text'
--
-- * 'pmaNamespace' @::@ 'Text'
--
-- * 'pmaOKActions' @::@ ['Text']
--
-- * 'pmaPeriod' @::@ 'Natural'
--
-- * 'pmaStatistic' @::@ 'Statistic'
--
-- * 'pmaThreshold' @::@ 'Double'
--
-- * 'pmaUnit' @::@ 'Maybe' 'StandardUnit'
--
putMetricAlarm :: Text -- ^ 'pmaAlarmName'
               -> Text -- ^ 'pmaMetricName'
               -> Text -- ^ 'pmaNamespace'
               -> Statistic -- ^ 'pmaStatistic'
               -> Natural -- ^ 'pmaPeriod'
               -> Natural -- ^ 'pmaEvaluationPeriods'
               -> Double -- ^ 'pmaThreshold'
               -> ComparisonOperator -- ^ 'pmaComparisonOperator'
               -> PutMetricAlarm
putMetricAlarm p1 p2 p3 p4 p5 p6 p7 p8 = PutMetricAlarm
    { _pmaAlarmName               = p1
    , _pmaMetricName              = p2
    , _pmaNamespace               = p3
    , _pmaStatistic               = p4
    , _pmaPeriod                  = withIso _Nat (const id) p5
    , _pmaEvaluationPeriods       = withIso _Nat (const id) p6
    , _pmaThreshold               = p7
    , _pmaComparisonOperator      = p8
    , _pmaAlarmDescription        = Nothing
    , _pmaActionsEnabled          = Nothing
    , _pmaOKActions               = mempty
    , _pmaAlarmActions            = mempty
    , _pmaInsufficientDataActions = mempty
    , _pmaDimensions              = mempty
    , _pmaUnit                    = Nothing
    }

-- | Indicates whether or not actions should be executed during any changes to
-- the alarm's state.
pmaActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaActionsEnabled =
    lens _pmaActionsEnabled (\s a -> s { _pmaActionsEnabled = a })

-- | The list of actions to execute when this alarm transitions into an ALARM
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaAlarmActions :: Lens' PutMetricAlarm [Text]
pmaAlarmActions = lens _pmaAlarmActions (\s a -> s { _pmaAlarmActions = a }) . _List

-- | The description for the alarm.
pmaAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaAlarmDescription =
    lens _pmaAlarmDescription (\s a -> s { _pmaAlarmDescription = a })

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account.
pmaAlarmName :: Lens' PutMetricAlarm Text
pmaAlarmName = lens _pmaAlarmName (\s a -> s { _pmaAlarmName = a })

-- | The arithmetic operation to use when comparing the specified Statistic
-- and Threshold. The specified Statistic value is used as the first
-- operand.
pmaComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator =
    lens _pmaComparisonOperator (\s a -> s { _pmaComparisonOperator = a })

-- | The dimensions for the alarm's associated metric.
pmaDimensions :: Lens' PutMetricAlarm [Dimension]
pmaDimensions = lens _pmaDimensions (\s a -> s { _pmaDimensions = a }) . _List

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaEvaluationPeriods :: Lens' PutMetricAlarm Natural
pmaEvaluationPeriods =
    lens _pmaEvaluationPeriods (\s a -> s { _pmaEvaluationPeriods = a })
        . _Nat

-- | The list of actions to execute when this alarm transitions into an
-- INSUFFICIENT_DATA state from any other state. Each action is specified as
-- an Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmaInsufficientDataActions =
    lens _pmaInsufficientDataActions
        (\s a -> s { _pmaInsufficientDataActions = a })
            . _List

-- | The name for the alarm's associated metric.
pmaMetricName :: Lens' PutMetricAlarm Text
pmaMetricName = lens _pmaMetricName (\s a -> s { _pmaMetricName = a })

-- | The namespace for the alarm's associated metric.
pmaNamespace :: Lens' PutMetricAlarm Text
pmaNamespace = lens _pmaNamespace (\s a -> s { _pmaNamespace = a })

-- | The list of actions to execute when this alarm transitions into an OK
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaOKActions :: Lens' PutMetricAlarm [Text]
pmaOKActions = lens _pmaOKActions (\s a -> s { _pmaOKActions = a }) . _List

-- | The period in seconds over which the specified statistic is applied.
pmaPeriod :: Lens' PutMetricAlarm Natural
pmaPeriod = lens _pmaPeriod (\s a -> s { _pmaPeriod = a }) . _Nat

-- | The statistic to apply to the alarm's associated metric.
pmaStatistic :: Lens' PutMetricAlarm Statistic
pmaStatistic = lens _pmaStatistic (\s a -> s { _pmaStatistic = a })

-- | The value against which the specified statistic is compared.
pmaThreshold :: Lens' PutMetricAlarm Double
pmaThreshold = lens _pmaThreshold (\s a -> s { _pmaThreshold = a })

-- | The unit for the alarm's associated metric.
pmaUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaUnit = lens _pmaUnit (\s a -> s { _pmaUnit = a })

data PutMetricAlarmResponse = PutMetricAlarmResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutMetricAlarmResponse' constructor.
putMetricAlarmResponse :: PutMetricAlarmResponse
putMetricAlarmResponse = PutMetricAlarmResponse

instance ToPath PutMetricAlarm where
    toPath = const "/"

instance ToQuery PutMetricAlarm where
    toQuery PutMetricAlarm{..} = mconcat
        [ "ActionsEnabled"          =? _pmaActionsEnabled
        , "AlarmActions"            =? _pmaAlarmActions
        , "AlarmDescription"        =? _pmaAlarmDescription
        , "AlarmName"               =? _pmaAlarmName
        , "ComparisonOperator"      =? _pmaComparisonOperator
        , "Dimensions"              =? _pmaDimensions
        , "EvaluationPeriods"       =? _pmaEvaluationPeriods
        , "InsufficientDataActions" =? _pmaInsufficientDataActions
        , "MetricName"              =? _pmaMetricName
        , "Namespace"               =? _pmaNamespace
        , "OKActions"               =? _pmaOKActions
        , "Period"                  =? _pmaPeriod
        , "Statistic"               =? _pmaStatistic
        , "Threshold"               =? _pmaThreshold
        , "Unit"                    =? _pmaUnit
        ]

instance ToHeaders PutMetricAlarm

instance AWSRequest PutMetricAlarm where
    type Sv PutMetricAlarm = CloudWatch
    type Rs PutMetricAlarm = PutMetricAlarmResponse

    request  = post "PutMetricAlarm"
    response = nullResponse PutMetricAlarmResponse
