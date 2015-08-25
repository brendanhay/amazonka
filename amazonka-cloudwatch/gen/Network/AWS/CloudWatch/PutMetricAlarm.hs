{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricAlarm
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm.
--
-- When this operation creates an alarm, the alarm state is immediately set
-- to 'INSUFFICIENT_DATA'. The alarm is evaluated and its 'StateValue' is
-- set appropriately. Any actions associated with the 'StateValue' is then
-- executed.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricAlarm.html AWS API Reference> for PutMetricAlarm.
module Network.AWS.CloudWatch.PutMetricAlarm
    (
    -- * Creating a Request
      putMetricAlarm
    , PutMetricAlarm
    -- * Request Lenses
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

    -- * Destructuring the Response
    , putMetricAlarmResponse
    , PutMetricAlarmResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.CloudWatch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'putMetricAlarm' smart constructor.
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

-- | Creates a value of 'PutMetricAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
putMetricAlarm
    :: Text -- ^ 'pmaAlarmName'
    -> Text -- ^ 'pmaMetricName'
    -> Text -- ^ 'pmaNamespace'
    -> Statistic -- ^ 'pmaStatistic'
    -> Natural -- ^ 'pmaPeriod'
    -> Natural -- ^ 'pmaEvaluationPeriods'
    -> Double -- ^ 'pmaThreshold'
    -> ComparisonOperator -- ^ 'pmaComparisonOperator'
    -> PutMetricAlarm
putMetricAlarm pAlarmName_ pMetricName_ pNamespace_ pStatistic_ pPeriod_ pEvaluationPeriods_ pThreshold_ pComparisonOperator_ =
    PutMetricAlarm'
    { _pmaAlarmDescription = Nothing
    , _pmaOKActions = Nothing
    , _pmaActionsEnabled = Nothing
    , _pmaInsufficientDataActions = Nothing
    , _pmaDimensions = Nothing
    , _pmaAlarmActions = Nothing
    , _pmaUnit = Nothing
    , _pmaAlarmName = pAlarmName_
    , _pmaMetricName = pMetricName_
    , _pmaNamespace = pNamespace_
    , _pmaStatistic = pStatistic_
    , _pmaPeriod = _Nat # pPeriod_
    , _pmaEvaluationPeriods = _Nat # pEvaluationPeriods_
    , _pmaThreshold = pThreshold_
    , _pmaComparisonOperator = pComparisonOperator_
    }

-- | The description for the alarm.
pmaAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaAlarmDescription = lens _pmaAlarmDescription (\ s a -> s{_pmaAlarmDescription = a});

-- | The list of actions to execute when this alarm transitions into an 'OK'
-- state from any other state. Each action is specified as an Amazon
-- Resource Number (ARN). Currently the only action supported is publishing
-- to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaOKActions :: Lens' PutMetricAlarm [Text]
pmaOKActions = lens _pmaOKActions (\ s a -> s{_pmaOKActions = a}) . _Default . _Coerce;

-- | Indicates whether or not actions should be executed during any changes
-- to the alarm\'s state.
pmaActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaActionsEnabled = lens _pmaActionsEnabled (\ s a -> s{_pmaActionsEnabled = a});

-- | The list of actions to execute when this alarm transitions into an
-- 'INSUFFICIENT_DATA' state from any other state. Each action is specified
-- as an Amazon Resource Number (ARN). Currently the only action supported
-- is publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmaInsufficientDataActions = lens _pmaInsufficientDataActions (\ s a -> s{_pmaInsufficientDataActions = a}) . _Default . _Coerce;

-- | The dimensions for the alarm\'s associated metric.
pmaDimensions :: Lens' PutMetricAlarm [Dimension]
pmaDimensions = lens _pmaDimensions (\ s a -> s{_pmaDimensions = a}) . _Default . _Coerce;

-- | The list of actions to execute when this alarm transitions into an
-- 'ALARM' state from any other state. Each action is specified as an
-- Amazon Resource Number (ARN). Currently the only action supported is
-- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
pmaAlarmActions :: Lens' PutMetricAlarm [Text]
pmaAlarmActions = lens _pmaAlarmActions (\ s a -> s{_pmaAlarmActions = a}) . _Default . _Coerce;

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

-- | The arithmetic operation to use when comparing the specified 'Statistic'
-- and 'Threshold'. The specified 'Statistic' value is used as the first
-- operand.
pmaComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator = lens _pmaComparisonOperator (\ s a -> s{_pmaComparisonOperator = a});

instance AWSRequest PutMetricAlarm where
        type Rs PutMetricAlarm = PutMetricAlarmResponse
        request = postQuery cloudWatch
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

-- | Creates a value of 'PutMetricAlarmResponse' with the minimum fields required to make a request.
--
putMetricAlarmResponse
    :: PutMetricAlarmResponse
putMetricAlarmResponse = PutMetricAlarmResponse'
