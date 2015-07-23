{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricAlarm
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm.
--
-- When this operation creates an alarm, the alarm state is immediately set
-- to @INSUFFICIENT_DATA@. The alarm is evaluated and its @StateValue@ is
-- set appropriately. Any actions associated with the @StateValue@ is then
-- executed.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricAlarm.html>
module Network.AWS.CloudWatch.PutMetricAlarm
    (
    -- * Request
      PutMetricAlarm
    -- ** Request constructor
    , putMetricAlarm
    -- ** Request lenses
    , pmarqAlarmDescription
    , pmarqOKActions
    , pmarqActionsEnabled
    , pmarqInsufficientDataActions
    , pmarqDimensions
    , pmarqAlarmActions
    , pmarqUnit
    , pmarqAlarmName
    , pmarqMetricName
    , pmarqNamespace
    , pmarqStatistic
    , pmarqPeriod
    , pmarqEvaluationPeriods
    , pmarqThreshold
    , pmarqComparisonOperator

    -- * Response
    , PutMetricAlarmResponse
    -- ** Response constructor
    , putMetricAlarmResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putMetricAlarm' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmarqAlarmDescription'
--
-- * 'pmarqOKActions'
--
-- * 'pmarqActionsEnabled'
--
-- * 'pmarqInsufficientDataActions'
--
-- * 'pmarqDimensions'
--
-- * 'pmarqAlarmActions'
--
-- * 'pmarqUnit'
--
-- * 'pmarqAlarmName'
--
-- * 'pmarqMetricName'
--
-- * 'pmarqNamespace'
--
-- * 'pmarqStatistic'
--
-- * 'pmarqPeriod'
--
-- * 'pmarqEvaluationPeriods'
--
-- * 'pmarqThreshold'
--
-- * 'pmarqComparisonOperator'
data PutMetricAlarm = PutMetricAlarm'
    { _pmarqAlarmDescription        :: !(Maybe Text)
    , _pmarqOKActions               :: !(Maybe [Text])
    , _pmarqActionsEnabled          :: !(Maybe Bool)
    , _pmarqInsufficientDataActions :: !(Maybe [Text])
    , _pmarqDimensions              :: !(Maybe [Dimension])
    , _pmarqAlarmActions            :: !(Maybe [Text])
    , _pmarqUnit                    :: !(Maybe StandardUnit)
    , _pmarqAlarmName               :: !Text
    , _pmarqMetricName              :: !Text
    , _pmarqNamespace               :: !Text
    , _pmarqStatistic               :: !Statistic
    , _pmarqPeriod                  :: !Nat
    , _pmarqEvaluationPeriods       :: !Nat
    , _pmarqThreshold               :: !Double
    , _pmarqComparisonOperator      :: !ComparisonOperator
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutMetricAlarm' smart constructor.
putMetricAlarm :: Text -> Text -> Text -> Statistic -> Natural -> Natural -> Double -> ComparisonOperator -> PutMetricAlarm
putMetricAlarm pAlarmName_ pMetricName_ pNamespace_ pStatistic_ pPeriod_ pEvaluationPeriods_ pThreshold_ pComparisonOperator_ =
    PutMetricAlarm'
    { _pmarqAlarmDescription = Nothing
    , _pmarqOKActions = Nothing
    , _pmarqActionsEnabled = Nothing
    , _pmarqInsufficientDataActions = Nothing
    , _pmarqDimensions = Nothing
    , _pmarqAlarmActions = Nothing
    , _pmarqUnit = Nothing
    , _pmarqAlarmName = pAlarmName_
    , _pmarqMetricName = pMetricName_
    , _pmarqNamespace = pNamespace_
    , _pmarqStatistic = pStatistic_
    , _pmarqPeriod = _Nat # pPeriod_
    , _pmarqEvaluationPeriods = _Nat # pEvaluationPeriods_
    , _pmarqThreshold = pThreshold_
    , _pmarqComparisonOperator = pComparisonOperator_
    }

-- | The description for the alarm.
pmarqAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmarqAlarmDescription = lens _pmarqAlarmDescription (\ s a -> s{_pmarqAlarmDescription = a});

-- | The list of actions to execute when this alarm transitions into an @OK@
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmarqOKActions :: Lens' PutMetricAlarm [Text]
pmarqOKActions = lens _pmarqOKActions (\ s a -> s{_pmarqOKActions = a}) . _Default;

-- | Indicates whether or not actions should be executed during any changes
-- to the alarm\'s state.
pmarqActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmarqActionsEnabled = lens _pmarqActionsEnabled (\ s a -> s{_pmarqActionsEnabled = a});

-- | The list of actions to execute when this alarm transitions into an
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Number (ARN). Currently the only action supported
-- is publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmarqInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmarqInsufficientDataActions = lens _pmarqInsufficientDataActions (\ s a -> s{_pmarqInsufficientDataActions = a}) . _Default;

-- | The dimensions for the alarm\'s associated metric.
pmarqDimensions :: Lens' PutMetricAlarm [Dimension]
pmarqDimensions = lens _pmarqDimensions (\ s a -> s{_pmarqDimensions = a}) . _Default;

-- | The list of actions to execute when this alarm transitions into an
-- @ALARM@ state from any other state. Each action is specified as an
-- Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmarqAlarmActions :: Lens' PutMetricAlarm [Text]
pmarqAlarmActions = lens _pmarqAlarmActions (\ s a -> s{_pmarqAlarmActions = a}) . _Default;

-- | The unit for the alarm\'s associated metric.
pmarqUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmarqUnit = lens _pmarqUnit (\ s a -> s{_pmarqUnit = a});

-- | The descriptive name for the alarm. This name must be unique within the
-- user\'s AWS account
pmarqAlarmName :: Lens' PutMetricAlarm Text
pmarqAlarmName = lens _pmarqAlarmName (\ s a -> s{_pmarqAlarmName = a});

-- | The name for the alarm\'s associated metric.
pmarqMetricName :: Lens' PutMetricAlarm Text
pmarqMetricName = lens _pmarqMetricName (\ s a -> s{_pmarqMetricName = a});

-- | The namespace for the alarm\'s associated metric.
pmarqNamespace :: Lens' PutMetricAlarm Text
pmarqNamespace = lens _pmarqNamespace (\ s a -> s{_pmarqNamespace = a});

-- | The statistic to apply to the alarm\'s associated metric.
pmarqStatistic :: Lens' PutMetricAlarm Statistic
pmarqStatistic = lens _pmarqStatistic (\ s a -> s{_pmarqStatistic = a});

-- | The period in seconds over which the specified statistic is applied.
pmarqPeriod :: Lens' PutMetricAlarm Natural
pmarqPeriod = lens _pmarqPeriod (\ s a -> s{_pmarqPeriod = a}) . _Nat;

-- | The number of periods over which data is compared to the specified
-- threshold.
pmarqEvaluationPeriods :: Lens' PutMetricAlarm Natural
pmarqEvaluationPeriods = lens _pmarqEvaluationPeriods (\ s a -> s{_pmarqEvaluationPeriods = a}) . _Nat;

-- | The value against which the specified statistic is compared.
pmarqThreshold :: Lens' PutMetricAlarm Double
pmarqThreshold = lens _pmarqThreshold (\ s a -> s{_pmarqThreshold = a});

-- | The arithmetic operation to use when comparing the specified @Statistic@
-- and @Threshold@. The specified @Statistic@ value is used as the first
-- operand.
pmarqComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmarqComparisonOperator = lens _pmarqComparisonOperator (\ s a -> s{_pmarqComparisonOperator = a});

instance AWSRequest PutMetricAlarm where
        type Sv PutMetricAlarm = CloudWatch
        type Rs PutMetricAlarm = PutMetricAlarmResponse
        request = post
        response = receiveNull PutMetricAlarmResponse'

instance ToHeaders PutMetricAlarm where
        toHeaders = const mempty

instance ToPath PutMetricAlarm where
        toPath = const "/"

instance ToQuery PutMetricAlarm where
        toQuery PutMetricAlarm'{..}
          = mconcat
              ["Action" =: ("PutMetricAlarm" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmDescription" =: _pmarqAlarmDescription,
               "OKActions" =:
                 toQuery (toQueryList "member" <$> _pmarqOKActions),
               "ActionsEnabled" =: _pmarqActionsEnabled,
               "InsufficientDataActions" =:
                 toQuery
                   (toQueryList "member" <$>
                      _pmarqInsufficientDataActions),
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _pmarqDimensions),
               "AlarmActions" =:
                 toQuery
                   (toQueryList "member" <$> _pmarqAlarmActions),
               "Unit" =: _pmarqUnit, "AlarmName" =: _pmarqAlarmName,
               "MetricName" =: _pmarqMetricName,
               "Namespace" =: _pmarqNamespace,
               "Statistic" =: _pmarqStatistic,
               "Period" =: _pmarqPeriod,
               "EvaluationPeriods" =: _pmarqEvaluationPeriods,
               "Threshold" =: _pmarqThreshold,
               "ComparisonOperator" =: _pmarqComparisonOperator]

-- | /See:/ 'putMetricAlarmResponse' smart constructor.
data PutMetricAlarmResponse =
    PutMetricAlarmResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutMetricAlarmResponse' smart constructor.
putMetricAlarmResponse :: PutMetricAlarmResponse
putMetricAlarmResponse = PutMetricAlarmResponse'
