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
    , pmaAlarmDescription
    , pmaOKActions
    , pmaActionsEnabled
    , pmaInsufficientDataActions
    , pmaDimensions
    , pmaAlarmActions
    , pmaUnit
    , pmaAlarmName
    , pmaMetricName
    , pmaNamespace
    , pmaStatistic
    , pmaPeriod
    , pmaEvaluationPeriods
    , pmaThreshold
    , pmaComparisonOperator

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
-- * 'pmaAlarmDescription'
--
-- * 'pmaOKActions'
--
-- * 'pmaActionsEnabled'
--
-- * 'pmaInsufficientDataActions'
--
-- * 'pmaDimensions'
--
-- * 'pmaAlarmActions'
--
-- * 'pmaUnit'
--
-- * 'pmaAlarmName'
--
-- * 'pmaMetricName'
--
-- * 'pmaNamespace'
--
-- * 'pmaStatistic'
--
-- * 'pmaPeriod'
--
-- * 'pmaEvaluationPeriods'
--
-- * 'pmaThreshold'
--
-- * 'pmaComparisonOperator'
data PutMetricAlarm = PutMetricAlarm'
    { _pmaAlarmDescription        :: !(Maybe Text)
    , _pmaOKActions               :: !(Maybe [Text])
    , _pmaActionsEnabled          :: !(Maybe Bool)
    , _pmaInsufficientDataActions :: !(Maybe [Text])
    , _pmaDimensions              :: !(Maybe [Dimension])
    , _pmaAlarmActions            :: !(Maybe [Text])
    , _pmaUnit                    :: !(Maybe StandardUnit)
    , _pmaAlarmName               :: !Text
    , _pmaMetricName              :: !Text
    , _pmaNamespace               :: !Text
    , _pmaStatistic               :: !Statistic
    , _pmaPeriod                  :: !Nat
    , _pmaEvaluationPeriods       :: !Nat
    , _pmaThreshold               :: !Double
    , _pmaComparisonOperator      :: !ComparisonOperator
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutMetricAlarm' smart constructor.
putMetricAlarm :: Text -> Text -> Text -> Statistic -> Natural -> Natural -> Double -> ComparisonOperator -> PutMetricAlarm
putMetricAlarm pAlarmName pMetricName pNamespace pStatistic pPeriod pEvaluationPeriods pThreshold pComparisonOperator =
    PutMetricAlarm'
    { _pmaAlarmDescription = Nothing
    , _pmaOKActions = Nothing
    , _pmaActionsEnabled = Nothing
    , _pmaInsufficientDataActions = Nothing
    , _pmaDimensions = Nothing
    , _pmaAlarmActions = Nothing
    , _pmaUnit = Nothing
    , _pmaAlarmName = pAlarmName
    , _pmaMetricName = pMetricName
    , _pmaNamespace = pNamespace
    , _pmaStatistic = pStatistic
    , _pmaPeriod = _Nat # pPeriod
    , _pmaEvaluationPeriods = _Nat # pEvaluationPeriods
    , _pmaThreshold = pThreshold
    , _pmaComparisonOperator = pComparisonOperator
    }

-- | The description for the alarm.
pmaAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaAlarmDescription = lens _pmaAlarmDescription (\ s a -> s{_pmaAlarmDescription = a});

-- | The list of actions to execute when this alarm transitions into an @OK@
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaOKActions :: Lens' PutMetricAlarm [Text]
pmaOKActions = lens _pmaOKActions (\ s a -> s{_pmaOKActions = a}) . _Default;

-- | Indicates whether or not actions should be executed during any changes
-- to the alarm\'s state.
pmaActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaActionsEnabled = lens _pmaActionsEnabled (\ s a -> s{_pmaActionsEnabled = a});

-- | The list of actions to execute when this alarm transitions into an
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Number (ARN). Currently the only action supported
-- is publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmaInsufficientDataActions = lens _pmaInsufficientDataActions (\ s a -> s{_pmaInsufficientDataActions = a}) . _Default;

-- | The dimensions for the alarm\'s associated metric.
pmaDimensions :: Lens' PutMetricAlarm [Dimension]
pmaDimensions = lens _pmaDimensions (\ s a -> s{_pmaDimensions = a}) . _Default;

-- | The list of actions to execute when this alarm transitions into an
-- @ALARM@ state from any other state. Each action is specified as an
-- Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaAlarmActions :: Lens' PutMetricAlarm [Text]
pmaAlarmActions = lens _pmaAlarmActions (\ s a -> s{_pmaAlarmActions = a}) . _Default;

-- | The unit for the alarm\'s associated metric.
pmaUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaUnit = lens _pmaUnit (\ s a -> s{_pmaUnit = a});

-- | The descriptive name for the alarm. This name must be unique within the
-- user\'s AWS account
pmaAlarmName :: Lens' PutMetricAlarm Text
pmaAlarmName = lens _pmaAlarmName (\ s a -> s{_pmaAlarmName = a});

-- | The name for the alarm\'s associated metric.
pmaMetricName :: Lens' PutMetricAlarm Text
pmaMetricName = lens _pmaMetricName (\ s a -> s{_pmaMetricName = a});

-- | The namespace for the alarm\'s associated metric.
pmaNamespace :: Lens' PutMetricAlarm Text
pmaNamespace = lens _pmaNamespace (\ s a -> s{_pmaNamespace = a});

-- | The statistic to apply to the alarm\'s associated metric.
pmaStatistic :: Lens' PutMetricAlarm Statistic
pmaStatistic = lens _pmaStatistic (\ s a -> s{_pmaStatistic = a});

-- | The period in seconds over which the specified statistic is applied.
pmaPeriod :: Lens' PutMetricAlarm Natural
pmaPeriod = lens _pmaPeriod (\ s a -> s{_pmaPeriod = a}) . _Nat;

-- | The number of periods over which data is compared to the specified
-- threshold.
pmaEvaluationPeriods :: Lens' PutMetricAlarm Natural
pmaEvaluationPeriods = lens _pmaEvaluationPeriods (\ s a -> s{_pmaEvaluationPeriods = a}) . _Nat;

-- | The value against which the specified statistic is compared.
pmaThreshold :: Lens' PutMetricAlarm Double
pmaThreshold = lens _pmaThreshold (\ s a -> s{_pmaThreshold = a});

-- | The arithmetic operation to use when comparing the specified @Statistic@
-- and @Threshold@. The specified @Statistic@ value is used as the first
-- operand.
pmaComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator = lens _pmaComparisonOperator (\ s a -> s{_pmaComparisonOperator = a});

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
               "AlarmDescription" =: _pmaAlarmDescription,
               "OKActions" =:
                 toQuery (toQueryList "member" <$> _pmaOKActions),
               "ActionsEnabled" =: _pmaActionsEnabled,
               "InsufficientDataActions" =:
                 toQuery
                   (toQueryList "member" <$>
                      _pmaInsufficientDataActions),
               "Dimensions" =:
                 toQuery (toQueryList "member" <$> _pmaDimensions),
               "AlarmActions" =:
                 toQuery (toQueryList "member" <$> _pmaAlarmActions),
               "Unit" =: _pmaUnit, "AlarmName" =: _pmaAlarmName,
               "MetricName" =: _pmaMetricName,
               "Namespace" =: _pmaNamespace,
               "Statistic" =: _pmaStatistic, "Period" =: _pmaPeriod,
               "EvaluationPeriods" =: _pmaEvaluationPeriods,
               "Threshold" =: _pmaThreshold,
               "ComparisonOperator" =: _pmaComparisonOperator]

-- | /See:/ 'putMetricAlarmResponse' smart constructor.
data PutMetricAlarmResponse =
    PutMetricAlarmResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutMetricAlarmResponse' smart constructor.
putMetricAlarmResponse :: PutMetricAlarmResponse
putMetricAlarmResponse = PutMetricAlarmResponse'
