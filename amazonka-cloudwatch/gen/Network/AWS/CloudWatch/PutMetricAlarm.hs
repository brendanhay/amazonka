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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm and associates it with the specified metric. Optionally, this operation can associate one or more Amazon SNS resources with the alarm.
--
--
-- When this operation creates an alarm, the alarm state is immediately set to @INSUFFICIENT_DATA@ . The alarm is evaluated and its state is set appropriately. Any actions associated with the state are then executed.
--
-- When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm.
--
-- If you are an AWS Identity and Access Management (IAM) user, you must have Amazon EC2 permissions for some operations:
--
--     * @ec2:DescribeInstanceStatus@ and @ec2:DescribeInstances@ for all alarms on EC2 instance status metrics
--
--     * @ec2:StopInstances@ for alarms with stop actions
--
--     * @ec2:TerminateInstances@ for alarms with terminate actions
--
--     * @ec2:DescribeInstanceRecoveryAttribute@ and @ec2:RecoverInstances@ for alarms with recover actions
--
--
--
-- If you have read/write permissions for Amazon CloudWatch but not for Amazon EC2, you can still create an alarm, but the stop or terminate actions won't be performed. However, if you are later granted the required permissions, the alarm actions that you created earlier will be performed.
--
-- If you are using an IAM role (for example, an Amazon EC2 instance profile), you cannot stop or terminate the instance using alarm actions. However, you can still see the alarm state and perform any other actions such as Amazon SNS notifications or Auto Scaling policies.
--
-- If you are using temporary security credentials granted using the AWS Security Token Service (AWS STS), you cannot stop or terminate an Amazon EC2 instance using alarm actions.
--
-- Note that you must create at least one stop, terminate, or reboot alarm using the Amazon EC2 or CloudWatch console to create the __EC2ActionsAccess__ IAM role. After this IAM role is created, you can create stop, terminate, or reboot alarms using a command-line interface or an API.
--
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
    , pmaStatistic
    , pmaExtendedStatistic
    , pmaAlarmName
    , pmaMetricName
    , pmaNamespace
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
import           Network.AWS.Lens
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
    , _pmaStatistic               :: !(Maybe Statistic)
    , _pmaExtendedStatistic       :: !(Maybe Text)
    , _pmaAlarmName               :: !Text
    , _pmaMetricName              :: !Text
    , _pmaNamespace               :: !Text
    , _pmaPeriod                  :: !Nat
    , _pmaEvaluationPeriods       :: !Nat
    , _pmaThreshold               :: !Double
    , _pmaComparisonOperator      :: !ComparisonOperator
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutMetricAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAlarmDescription' - The description for the alarm.
--
-- * 'pmaOKActions' - The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover Valid Values (for use with IAM roles): arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
--
-- * 'pmaActionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state.
--
-- * 'pmaInsufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover Valid Values (for use with IAM roles): arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
--
-- * 'pmaDimensions' - The dimensions for the metric associated with the alarm.
--
-- * 'pmaAlarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover Valid Values (for use with IAM roles): arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
--
-- * 'pmaUnit' - The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately. If you specify a unit, you must use a unit that is appropriate for the metric. Otherwise, the Amazon CloudWatch alarm can get stuck in the @INSUFFICIENT DATA@ state.
--
-- * 'pmaStatistic' - The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
--
-- * 'pmaExtendedStatistic' - The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
--
-- * 'pmaAlarmName' - The name for the alarm. This name must be unique within the AWS account.
--
-- * 'pmaMetricName' - The name for the metric associated with the alarm.
--
-- * 'pmaNamespace' - The namespace for the metric associated with the alarm.
--
-- * 'pmaPeriod' - The period, in seconds, over which the specified statistic is applied.
--
-- * 'pmaEvaluationPeriods' - The number of periods over which data is compared to the specified threshold.
--
-- * 'pmaThreshold' - The value against which the specified statistic is compared.
--
-- * 'pmaComparisonOperator' - The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
putMetricAlarm
    :: Text -- ^ 'pmaAlarmName'
    -> Text -- ^ 'pmaMetricName'
    -> Text -- ^ 'pmaNamespace'
    -> Natural -- ^ 'pmaPeriod'
    -> Natural -- ^ 'pmaEvaluationPeriods'
    -> Double -- ^ 'pmaThreshold'
    -> ComparisonOperator -- ^ 'pmaComparisonOperator'
    -> PutMetricAlarm
putMetricAlarm pAlarmName_ pMetricName_ pNamespace_ pPeriod_ pEvaluationPeriods_ pThreshold_ pComparisonOperator_ =
    PutMetricAlarm'
    { _pmaAlarmDescription = Nothing
    , _pmaOKActions = Nothing
    , _pmaActionsEnabled = Nothing
    , _pmaInsufficientDataActions = Nothing
    , _pmaDimensions = Nothing
    , _pmaAlarmActions = Nothing
    , _pmaUnit = Nothing
    , _pmaStatistic = Nothing
    , _pmaExtendedStatistic = Nothing
    , _pmaAlarmName = pAlarmName_
    , _pmaMetricName = pMetricName_
    , _pmaNamespace = pNamespace_
    , _pmaPeriod = _Nat # pPeriod_
    , _pmaEvaluationPeriods = _Nat # pEvaluationPeriods_
    , _pmaThreshold = pThreshold_
    , _pmaComparisonOperator = pComparisonOperator_
    }

-- | The description for the alarm.
pmaAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaAlarmDescription = lens _pmaAlarmDescription (\ s a -> s{_pmaAlarmDescription = a});

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover Valid Values (for use with IAM roles): arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
pmaOKActions :: Lens' PutMetricAlarm [Text]
pmaOKActions = lens _pmaOKActions (\ s a -> s{_pmaOKActions = a}) . _Default . _Coerce;

-- | Indicates whether actions should be executed during any changes to the alarm state.
pmaActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaActionsEnabled = lens _pmaActionsEnabled (\ s a -> s{_pmaActionsEnabled = a});

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover Valid Values (for use with IAM roles): arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
pmaInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmaInsufficientDataActions = lens _pmaInsufficientDataActions (\ s a -> s{_pmaInsufficientDataActions = a}) . _Default . _Coerce;

-- | The dimensions for the metric associated with the alarm.
pmaDimensions :: Lens' PutMetricAlarm [Dimension]
pmaDimensions = lens _pmaDimensions (\ s a -> s{_pmaDimensions = a}) . _Default . _Coerce;

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover Valid Values (for use with IAM roles): arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:us-east-1:{/customer-account/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
pmaAlarmActions :: Lens' PutMetricAlarm [Text]
pmaAlarmActions = lens _pmaAlarmActions (\ s a -> s{_pmaAlarmActions = a}) . _Default . _Coerce;

-- | The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately. If you specify a unit, you must use a unit that is appropriate for the metric. Otherwise, the Amazon CloudWatch alarm can get stuck in the @INSUFFICIENT DATA@ state.
pmaUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaUnit = lens _pmaUnit (\ s a -> s{_pmaUnit = a});

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ .
pmaStatistic :: Lens' PutMetricAlarm (Maybe Statistic)
pmaStatistic = lens _pmaStatistic (\ s a -> s{_pmaStatistic = a});

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100.
pmaExtendedStatistic :: Lens' PutMetricAlarm (Maybe Text)
pmaExtendedStatistic = lens _pmaExtendedStatistic (\ s a -> s{_pmaExtendedStatistic = a});

-- | The name for the alarm. This name must be unique within the AWS account.
pmaAlarmName :: Lens' PutMetricAlarm Text
pmaAlarmName = lens _pmaAlarmName (\ s a -> s{_pmaAlarmName = a});

-- | The name for the metric associated with the alarm.
pmaMetricName :: Lens' PutMetricAlarm Text
pmaMetricName = lens _pmaMetricName (\ s a -> s{_pmaMetricName = a});

-- | The namespace for the metric associated with the alarm.
pmaNamespace :: Lens' PutMetricAlarm Text
pmaNamespace = lens _pmaNamespace (\ s a -> s{_pmaNamespace = a});

-- | The period, in seconds, over which the specified statistic is applied.
pmaPeriod :: Lens' PutMetricAlarm Natural
pmaPeriod = lens _pmaPeriod (\ s a -> s{_pmaPeriod = a}) . _Nat;

-- | The number of periods over which data is compared to the specified threshold.
pmaEvaluationPeriods :: Lens' PutMetricAlarm Natural
pmaEvaluationPeriods = lens _pmaEvaluationPeriods (\ s a -> s{_pmaEvaluationPeriods = a}) . _Nat;

-- | The value against which the specified statistic is compared.
pmaThreshold :: Lens' PutMetricAlarm Double
pmaThreshold = lens _pmaThreshold (\ s a -> s{_pmaThreshold = a});

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
pmaComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator = lens _pmaComparisonOperator (\ s a -> s{_pmaComparisonOperator = a});

instance AWSRequest PutMetricAlarm where
        type Rs PutMetricAlarm = PutMetricAlarmResponse
        request = postQuery cloudWatch
        response = receiveNull PutMetricAlarmResponse'

instance Hashable PutMetricAlarm

instance NFData PutMetricAlarm

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
               "Unit" =: _pmaUnit, "Statistic" =: _pmaStatistic,
               "ExtendedStatistic" =: _pmaExtendedStatistic,
               "AlarmName" =: _pmaAlarmName,
               "MetricName" =: _pmaMetricName,
               "Namespace" =: _pmaNamespace, "Period" =: _pmaPeriod,
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

instance NFData PutMetricAlarmResponse
