{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.CloudWatch.PutMetricAlarm
    (
    -- * Request
      PutMetricAlarmInput
    -- ** Request constructor
    , putMetricAlarm
    -- ** Request lenses
    , pmaiActionsEnabled
    , pmaiAlarmActions
    , pmaiAlarmDescription
    , pmaiAlarmName
    , pmaiComparisonOperator
    , pmaiDimensions
    , pmaiEvaluationPeriods
    , pmaiInsufficientDataActions
    , pmaiMetricName
    , pmaiNamespace
    , pmaiOKActions
    , pmaiPeriod
    , pmaiStatistic
    , pmaiThreshold
    , pmaiUnit

    -- * Response
    , PutMetricAlarmResponse
    -- ** Response constructor
    , putMetricAlarmResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data PutMetricAlarmInput = PutMetricAlarmInput
    { _pmaiActionsEnabled          :: Maybe Bool
    , _pmaiAlarmActions            :: [Text]
    , _pmaiAlarmDescription        :: Maybe Text
    , _pmaiAlarmName               :: Text
    , _pmaiComparisonOperator      :: Text
    , _pmaiDimensions              :: [Dimension]
    , _pmaiEvaluationPeriods       :: Natural
    , _pmaiInsufficientDataActions :: [Text]
    , _pmaiMetricName              :: Text
    , _pmaiNamespace               :: Text
    , _pmaiOKActions               :: [Text]
    , _pmaiPeriod                  :: Natural
    , _pmaiStatistic               :: Text
    , _pmaiThreshold               :: Double
    , _pmaiUnit                    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PutMetricAlarmInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmaiActionsEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'pmaiAlarmActions' @::@ ['Text']
--
-- * 'pmaiAlarmDescription' @::@ 'Maybe' 'Text'
--
-- * 'pmaiAlarmName' @::@ 'Text'
--
-- * 'pmaiComparisonOperator' @::@ 'Text'
--
-- * 'pmaiDimensions' @::@ ['Dimension']
--
-- * 'pmaiEvaluationPeriods' @::@ 'Natural'
--
-- * 'pmaiInsufficientDataActions' @::@ ['Text']
--
-- * 'pmaiMetricName' @::@ 'Text'
--
-- * 'pmaiNamespace' @::@ 'Text'
--
-- * 'pmaiOKActions' @::@ ['Text']
--
-- * 'pmaiPeriod' @::@ 'Natural'
--
-- * 'pmaiStatistic' @::@ 'Text'
--
-- * 'pmaiThreshold' @::@ 'Double'
--
-- * 'pmaiUnit' @::@ 'Maybe' 'Text'
--
putMetricAlarm :: Text -- ^ 'pmaiAlarmName'
               -> Text -- ^ 'pmaiMetricName'
               -> Text -- ^ 'pmaiNamespace'
               -> Text -- ^ 'pmaiStatistic'
               -> Natural -- ^ 'pmaiPeriod'
               -> Natural -- ^ 'pmaiEvaluationPeriods'
               -> Double -- ^ 'pmaiThreshold'
               -> Text -- ^ 'pmaiComparisonOperator'
               -> PutMetricAlarmInput
putMetricAlarm p1 p2 p3 p4 p5 p6 p7 p8 = PutMetricAlarmInput
    { _pmaiAlarmName               = p1
    , _pmaiMetricName              = p2
    , _pmaiNamespace               = p3
    , _pmaiStatistic               = p4
    , _pmaiPeriod                  = p5
    , _pmaiEvaluationPeriods       = p6
    , _pmaiThreshold               = p7
    , _pmaiComparisonOperator      = p8
    , _pmaiAlarmDescription        = Nothing
    , _pmaiActionsEnabled          = Nothing
    , _pmaiOKActions               = mempty
    , _pmaiAlarmActions            = mempty
    , _pmaiInsufficientDataActions = mempty
    , _pmaiDimensions              = mempty
    , _pmaiUnit                    = Nothing
    }

-- | Indicates whether or not actions should be executed during any changes to
-- the alarm's state.
pmaiActionsEnabled :: Lens' PutMetricAlarmInput (Maybe Bool)
pmaiActionsEnabled =
    lens _pmaiActionsEnabled (\s a -> s { _pmaiActionsEnabled = a })

-- | The list of actions to execute when this alarm transitions into an ALARM
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiAlarmActions :: Lens' PutMetricAlarmInput [Text]
pmaiAlarmActions = lens _pmaiAlarmActions (\s a -> s { _pmaiAlarmActions = a })

-- | The description for the alarm.
pmaiAlarmDescription :: Lens' PutMetricAlarmInput (Maybe Text)
pmaiAlarmDescription =
    lens _pmaiAlarmDescription (\s a -> s { _pmaiAlarmDescription = a })

-- | The descriptive name for the alarm. This name must be unique within the
-- user's AWS account.
pmaiAlarmName :: Lens' PutMetricAlarmInput Text
pmaiAlarmName = lens _pmaiAlarmName (\s a -> s { _pmaiAlarmName = a })

-- | The arithmetic operation to use when comparing the specified Statistic
-- and Threshold. The specified Statistic value is used as the first
-- operand.
pmaiComparisonOperator :: Lens' PutMetricAlarmInput Text
pmaiComparisonOperator =
    lens _pmaiComparisonOperator (\s a -> s { _pmaiComparisonOperator = a })

-- | The dimensions for the alarm's associated metric.
pmaiDimensions :: Lens' PutMetricAlarmInput [Dimension]
pmaiDimensions = lens _pmaiDimensions (\s a -> s { _pmaiDimensions = a })

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaiEvaluationPeriods :: Lens' PutMetricAlarmInput Natural
pmaiEvaluationPeriods =
    lens _pmaiEvaluationPeriods (\s a -> s { _pmaiEvaluationPeriods = a })

-- | The list of actions to execute when this alarm transitions into an
-- INSUFFICIENT_DATA state from any other state. Each action is specified as
-- an Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiInsufficientDataActions :: Lens' PutMetricAlarmInput [Text]
pmaiInsufficientDataActions =
    lens _pmaiInsufficientDataActions
        (\s a -> s { _pmaiInsufficientDataActions = a })

-- | The name for the alarm's associated metric.
pmaiMetricName :: Lens' PutMetricAlarmInput Text
pmaiMetricName = lens _pmaiMetricName (\s a -> s { _pmaiMetricName = a })

-- | The namespace for the alarm's associated metric.
pmaiNamespace :: Lens' PutMetricAlarmInput Text
pmaiNamespace = lens _pmaiNamespace (\s a -> s { _pmaiNamespace = a })

-- | The list of actions to execute when this alarm transitions into an OK
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaiOKActions :: Lens' PutMetricAlarmInput [Text]
pmaiOKActions = lens _pmaiOKActions (\s a -> s { _pmaiOKActions = a })

-- | The period in seconds over which the specified statistic is applied.
pmaiPeriod :: Lens' PutMetricAlarmInput Natural
pmaiPeriod = lens _pmaiPeriod (\s a -> s { _pmaiPeriod = a })

-- | The statistic to apply to the alarm's associated metric.
pmaiStatistic :: Lens' PutMetricAlarmInput Text
pmaiStatistic = lens _pmaiStatistic (\s a -> s { _pmaiStatistic = a })

-- | The value against which the specified statistic is compared.
pmaiThreshold :: Lens' PutMetricAlarmInput Double
pmaiThreshold = lens _pmaiThreshold (\s a -> s { _pmaiThreshold = a })

-- | The unit for the alarm's associated metric.
pmaiUnit :: Lens' PutMetricAlarmInput (Maybe Text)
pmaiUnit = lens _pmaiUnit (\s a -> s { _pmaiUnit = a })

instance ToQuery PutMetricAlarmInput

instance ToPath PutMetricAlarmInput where
    toPath = const "/"

data PutMetricAlarmResponse = PutMetricAlarmResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutMetricAlarmResponse' constructor.
putMetricAlarmResponse :: PutMetricAlarmResponse
putMetricAlarmResponse = PutMetricAlarmResponse

instance FromXML PutMetricAlarmResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutMetricAlarmResponse"

instance AWSRequest PutMetricAlarmInput where
    type Sv PutMetricAlarmInput = CloudWatch
    type Rs PutMetricAlarmInput = PutMetricAlarmResponse

    request  = post "PutMetricAlarm"
    response = nullaryResponse PutMetricAlarmResponse
