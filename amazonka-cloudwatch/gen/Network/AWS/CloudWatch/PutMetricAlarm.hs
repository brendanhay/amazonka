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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- If you are an IAM user, you must have Amazon EC2 permissions for some operations:
--
--     * @iam:CreateServiceLinkedRole@ for all alarms with EC2 actions
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
-- If you have read/write permissions for Amazon CloudWatch but not for Amazon EC2, you can still create an alarm, but the stop or terminate actions are not performed. However, if you are later granted the required permissions, the alarm actions that you created earlier are performed.
--
-- If you are using an IAM role (for example, an EC2 instance profile), you cannot stop or terminate the instance using alarm actions. However, you can still see the alarm state and perform any other actions such as Amazon SNS notifications or Auto Scaling policies.
--
-- If you are using temporary security credentials granted using AWS STS, you cannot stop or terminate an EC2 instance using alarm actions.
--
-- You must create at least one stop, terminate, or reboot alarm using either the Amazon EC2 or CloudWatch consoles to create the __EC2ActionsAccess__ IAM role. After this IAM role is created, you can create stop, terminate, or reboot alarms using a command-line interface or API.
--
module Network.AWS.CloudWatch.PutMetricAlarm
    (
    -- * Creating a Request
      putMetricAlarm
    , PutMetricAlarm
    -- * Request Lenses
    , pmaTreatMissingData
    , pmaAlarmDescription
    , pmaOKActions
    , pmaEvaluateLowSampleCountPercentile
    , pmaDatapointsToAlarm
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

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putMetricAlarm' smart constructor.
data PutMetricAlarm = PutMetricAlarm'
  { _pmaTreatMissingData                 :: !(Maybe Text)
  , _pmaAlarmDescription                 :: !(Maybe Text)
  , _pmaOKActions                        :: !(Maybe [Text])
  , _pmaEvaluateLowSampleCountPercentile :: !(Maybe Text)
  , _pmaDatapointsToAlarm                :: !(Maybe Nat)
  , _pmaActionsEnabled                   :: !(Maybe Bool)
  , _pmaInsufficientDataActions          :: !(Maybe [Text])
  , _pmaDimensions                       :: !(Maybe [Dimension])
  , _pmaAlarmActions                     :: !(Maybe [Text])
  , _pmaUnit                             :: !(Maybe StandardUnit)
  , _pmaStatistic                        :: !(Maybe Statistic)
  , _pmaExtendedStatistic                :: !(Maybe Text)
  , _pmaAlarmName                        :: !Text
  , _pmaMetricName                       :: !Text
  , _pmaNamespace                        :: !Text
  , _pmaPeriod                           :: !Nat
  , _pmaEvaluationPeriods                :: !Nat
  , _pmaThreshold                        :: !Double
  , _pmaComparisonOperator               :: !ComparisonOperator
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMetricAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaTreatMissingData' - Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> . Valid Values: @breaching | notBreaching | ignore | missing@
--
-- * 'pmaAlarmDescription' - The description for the alarm.
--
-- * 'pmaOKActions' - The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover | arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ | arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/  Valid Values (for use with IAM roles): arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
--
-- * 'pmaEvaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> . Valid Values: @evaluate | ignore@
--
-- * 'pmaDatapointsToAlarm' - The number of datapoints that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
--
-- * 'pmaActionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state.
--
-- * 'pmaInsufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover | arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ | arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/  Valid Values (for use with IAM roles): arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
--
-- * 'pmaDimensions' - The dimensions for the metric associated with the alarm.
--
-- * 'pmaAlarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover | arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ | arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/  Valid Values (for use with IAM roles): arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
--
-- * 'pmaUnit' - The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately. If you specify a unit, you must use a unit that is appropriate for the metric. Otherwise, the CloudWatch alarm can get stuck in the @INSUFFICIENT DATA@ state.
--
-- * 'pmaStatistic' - The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- * 'pmaExtendedStatistic' - The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- * 'pmaAlarmName' - The name for the alarm. This name must be unique within the AWS account.
--
-- * 'pmaMetricName' - The name for the metric associated with the alarm.
--
-- * 'pmaNamespace' - The namespace for the metric associated with the alarm.
--
-- * 'pmaPeriod' - The period, in seconds, over which the specified statistic is applied. Valid values are 10, 30, and any multiple of 60. Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm may often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> . An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
--
-- * 'pmaEvaluationPeriods' - The number of periods over which data is compared to the specified threshold. If you are setting an alarm which requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N. An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
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
    { _pmaTreatMissingData = Nothing
    , _pmaAlarmDescription = Nothing
    , _pmaOKActions = Nothing
    , _pmaEvaluateLowSampleCountPercentile = Nothing
    , _pmaDatapointsToAlarm = Nothing
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


-- | Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> . Valid Values: @breaching | notBreaching | ignore | missing@
pmaTreatMissingData :: Lens' PutMetricAlarm (Maybe Text)
pmaTreatMissingData = lens _pmaTreatMissingData (\ s a -> s{_pmaTreatMissingData = a})

-- | The description for the alarm.
pmaAlarmDescription :: Lens' PutMetricAlarm (Maybe Text)
pmaAlarmDescription = lens _pmaAlarmDescription (\ s a -> s{_pmaAlarmDescription = a})

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover | arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ | arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/  Valid Values (for use with IAM roles): arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
pmaOKActions :: Lens' PutMetricAlarm [Text]
pmaOKActions = lens _pmaOKActions (\ s a -> s{_pmaOKActions = a}) . _Default . _Coerce

-- | Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> . Valid Values: @evaluate | ignore@
pmaEvaluateLowSampleCountPercentile :: Lens' PutMetricAlarm (Maybe Text)
pmaEvaluateLowSampleCountPercentile = lens _pmaEvaluateLowSampleCountPercentile (\ s a -> s{_pmaEvaluateLowSampleCountPercentile = a})

-- | The number of datapoints that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
pmaDatapointsToAlarm :: Lens' PutMetricAlarm (Maybe Natural)
pmaDatapointsToAlarm = lens _pmaDatapointsToAlarm (\ s a -> s{_pmaDatapointsToAlarm = a}) . mapping _Nat

-- | Indicates whether actions should be executed during any changes to the alarm state.
pmaActionsEnabled :: Lens' PutMetricAlarm (Maybe Bool)
pmaActionsEnabled = lens _pmaActionsEnabled (\ s a -> s{_pmaActionsEnabled = a})

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover | arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ | arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/  Valid Values (for use with IAM roles): arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
pmaInsufficientDataActions :: Lens' PutMetricAlarm [Text]
pmaInsufficientDataActions = lens _pmaInsufficientDataActions (\ s a -> s{_pmaInsufficientDataActions = a}) . _Default . _Coerce

-- | The dimensions for the metric associated with the alarm.
pmaDimensions :: Lens' PutMetricAlarm [Dimension]
pmaDimensions = lens _pmaDimensions (\ s a -> s{_pmaDimensions = a}) . _Default . _Coerce

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN). Valid Values: arn:aws:automate:/region/ :ec2:stop | arn:aws:automate:/region/ :ec2:terminate | arn:aws:automate:/region/ :ec2:recover | arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ | arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/  Valid Values (for use with IAM roles): arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Stop/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Terminate/1.0 | arn:aws:swf:/region/ :{/account-id/ }:action/actions/AWS_EC2.InstanceId.Reboot/1.0
pmaAlarmActions :: Lens' PutMetricAlarm [Text]
pmaAlarmActions = lens _pmaAlarmActions (\ s a -> s{_pmaAlarmActions = a}) . _Default . _Coerce

-- | The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately. If you specify a unit, you must use a unit that is appropriate for the metric. Otherwise, the CloudWatch alarm can get stuck in the @INSUFFICIENT DATA@ state.
pmaUnit :: Lens' PutMetricAlarm (Maybe StandardUnit)
pmaUnit = lens _pmaUnit (\ s a -> s{_pmaUnit = a})

-- | The statistic for the metric associated with the alarm, other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
pmaStatistic :: Lens' PutMetricAlarm (Maybe Statistic)
pmaStatistic = lens _pmaStatistic (\ s a -> s{_pmaStatistic = a})

-- | The percentile statistic for the metric associated with the alarm. Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
pmaExtendedStatistic :: Lens' PutMetricAlarm (Maybe Text)
pmaExtendedStatistic = lens _pmaExtendedStatistic (\ s a -> s{_pmaExtendedStatistic = a})

-- | The name for the alarm. This name must be unique within the AWS account.
pmaAlarmName :: Lens' PutMetricAlarm Text
pmaAlarmName = lens _pmaAlarmName (\ s a -> s{_pmaAlarmName = a})

-- | The name for the metric associated with the alarm.
pmaMetricName :: Lens' PutMetricAlarm Text
pmaMetricName = lens _pmaMetricName (\ s a -> s{_pmaMetricName = a})

-- | The namespace for the metric associated with the alarm.
pmaNamespace :: Lens' PutMetricAlarm Text
pmaNamespace = lens _pmaNamespace (\ s a -> s{_pmaNamespace = a})

-- | The period, in seconds, over which the specified statistic is applied. Valid values are 10, 30, and any multiple of 60. Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm may often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> . An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
pmaPeriod :: Lens' PutMetricAlarm Natural
pmaPeriod = lens _pmaPeriod (\ s a -> s{_pmaPeriod = a}) . _Nat

-- | The number of periods over which data is compared to the specified threshold. If you are setting an alarm which requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N. An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
pmaEvaluationPeriods :: Lens' PutMetricAlarm Natural
pmaEvaluationPeriods = lens _pmaEvaluationPeriods (\ s a -> s{_pmaEvaluationPeriods = a}) . _Nat

-- | The value against which the specified statistic is compared.
pmaThreshold :: Lens' PutMetricAlarm Double
pmaThreshold = lens _pmaThreshold (\ s a -> s{_pmaThreshold = a})

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
pmaComparisonOperator :: Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator = lens _pmaComparisonOperator (\ s a -> s{_pmaComparisonOperator = a})

instance AWSRequest PutMetricAlarm where
        type Rs PutMetricAlarm = PutMetricAlarmResponse
        request = postQuery cloudWatch
        response = receiveNull PutMetricAlarmResponse'

instance Hashable PutMetricAlarm where

instance NFData PutMetricAlarm where

instance ToHeaders PutMetricAlarm where
        toHeaders = const mempty

instance ToPath PutMetricAlarm where
        toPath = const "/"

instance ToQuery PutMetricAlarm where
        toQuery PutMetricAlarm'{..}
          = mconcat
              ["Action" =: ("PutMetricAlarm" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "TreatMissingData" =: _pmaTreatMissingData,
               "AlarmDescription" =: _pmaAlarmDescription,
               "OKActions" =:
                 toQuery (toQueryList "member" <$> _pmaOKActions),
               "EvaluateLowSampleCountPercentile" =:
                 _pmaEvaluateLowSampleCountPercentile,
               "DatapointsToAlarm" =: _pmaDatapointsToAlarm,
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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMetricAlarmResponse' with the minimum fields required to make a request.
--
putMetricAlarmResponse
    :: PutMetricAlarmResponse
putMetricAlarmResponse = PutMetricAlarmResponse'


instance NFData PutMetricAlarmResponse where
