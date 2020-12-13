{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm and associates it with the specified metric, metric math expression, or anomaly detection model.
--
-- Alarms based on anomaly detection models cannot have Auto Scaling actions.
-- When this operation creates an alarm, the alarm state is immediately set to @INSUFFICIENT_DATA@ . The alarm is then evaluated and its state is set appropriately. Any actions associated with the new state are then executed.
-- When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm.
-- If you are an IAM user, you must have Amazon EC2 permissions for some alarm operations:
--
--     * @iam:CreateServiceLinkedRole@ for all alarms with EC2 actions
--
--
--     * @ec2:DescribeInstanceStatus@ and @ec2:DescribeInstances@ for all alarms on EC2 instance status metrics
--
--
--     * @ec2:StopInstances@ for alarms with stop actions
--
--
--     * @ec2:TerminateInstances@ for alarms with terminate actions
--
--
--     * No specific permissions are needed for alarms with recover actions
--
--
-- If you have read/write permissions for Amazon CloudWatch but not for Amazon EC2, you can still create an alarm, but the stop or terminate actions are not performed. However, if you are later granted the required permissions, the alarm actions that you created earlier are performed.
-- If you are using an IAM role (for example, an EC2 instance profile), you cannot stop or terminate the instance using alarm actions. However, you can still see the alarm state and perform any other actions such as Amazon SNS notifications or Auto Scaling policies.
-- If you are using temporary security credentials granted using AWS STS, you cannot stop or terminate an EC2 instance using alarm actions.
-- The first time you create an alarm in the AWS Management Console, the CLI, or by using the PutMetricAlarm API, CloudWatch creates the necessary service-linked role for you. The service-linked role is called @AWSServiceRoleForCloudWatchEvents@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role AWS service-linked role> .
module Network.AWS.CloudWatch.PutMetricAlarm
  ( -- * Creating a request
    PutMetricAlarm (..),
    mkPutMetricAlarm,

    -- ** Request lenses
    pmaAlarmName,
    pmaMetrics,
    pmaTreatMissingData,
    pmaPeriod,
    pmaAlarmDescription,
    pmaEvaluationPeriods,
    pmaMetricName,
    pmaNamespace,
    pmaThresholdMetricId,
    pmaComparisonOperator,
    pmaOKActions,
    pmaEvaluateLowSampleCountPercentile,
    pmaDatapointsToAlarm,
    pmaThreshold,
    pmaActionsEnabled,
    pmaInsufficientDataActions,
    pmaDimensions,
    pmaAlarmActions,
    pmaUnit,
    pmaStatistic,
    pmaTags,
    pmaExtendedStatistic,

    -- * Destructuring the response
    PutMetricAlarmResponse (..),
    mkPutMetricAlarmResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutMetricAlarm' smart constructor.
data PutMetricAlarm = PutMetricAlarm'
  { -- | The name for the alarm. This name must be unique within the Region.
    alarmName :: Lude.Text,
    -- | An array of @MetricDataQuery@ structures that enable you to create an alarm based on the result of a metric math expression. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
    --
    -- Each item in the @Metrics@ array either retrieves a metric or performs a math expression.
    -- One item in the @Metrics@ array is the expression that the alarm watches. You designate this expression by setting @ReturnData@ to true for this object in the array. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery> .
    -- If you use the @Metrics@ parameter, you cannot include the @MetricName@ , @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters of @PutMetricAlarm@ in the same operation. Instead, you retrieve the metrics you are using in your math expression as part of the @Metrics@ array.
    metrics :: Lude.Maybe [MetricDataQuery],
    -- | Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> .
    --
    -- Valid Values: @breaching | notBreaching | ignore | missing@
    treatMissingData :: Lude.Maybe Lude.Text,
    -- | The length, in seconds, used each time the metric specified in @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of 60.
    --
    -- @Period@ is required for alarms based on static thresholds. If you are creating an alarm based on a metric math expression, you specify the period for each metric within the objects in the @Metrics@ array.
    -- Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
    -- An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
    period :: Lude.Maybe Lude.Natural,
    -- | The description for the alarm.
    alarmDescription :: Lude.Maybe Lude.Text,
    -- | The number of periods over which data is compared to the specified threshold. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N.
    --
    -- An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
    evaluationPeriods :: Lude.Natural,
    -- | The name for the metric associated with the alarm. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
    --
    -- If you are creating an alarm based on a math expression, you cannot specify this parameter, or any of the @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters. Instead, you specify all this information in the @Metrics@ array.
    metricName :: Lude.Maybe Lude.Text,
    -- | The namespace for the metric associated specified in @MetricName@ .
    namespace :: Lude.Maybe Lude.Text,
    -- | If this is an alarm based on an anomaly detection model, make this value match the ID of the @ANOMALY_DETECTION_BAND@ function.
    --
    -- For an example of how to use this parameter, see the __Anomaly Detection Model Alarm__ example on this page.
    -- If your alarm uses this parameter, it cannot have Auto Scaling actions.
    thresholdMetricId :: Lude.Maybe Lude.Text,
    -- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
    --
    -- The values @LessThanLowerOrGreaterThanUpperThreshold@ , @LessThanLowerThreshold@ , and @GreaterThanUpperThreshold@ are used only for alarms based on anomaly detection models.
    comparisonOperator :: ComparisonOperator,
    -- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
    -- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
    okActions :: Lude.Maybe [Lude.Text],
    -- | Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> .
    --
    -- Valid Values: @evaluate | ignore@
    evaluateLowSampleCountPercentile :: Lude.Maybe Lude.Text,
    -- | The number of data points that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
    datapointsToAlarm :: Lude.Maybe Lude.Natural,
    -- | The value against which the specified statistic is compared.
    --
    -- This parameter is required for alarms based on static thresholds, but should not be used for alarms based on anomaly detection models.
    threshold :: Lude.Maybe Lude.Double,
    -- | Indicates whether actions should be executed during any changes to the alarm state. The default is @TRUE@ .
    actionsEnabled :: Lude.Maybe Lude.Bool,
    -- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
    -- Valid Values (for use with IAM roles): @>arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
    insufficientDataActions :: Lude.Maybe [Lude.Text],
    -- | The dimensions for the metric specified in @MetricName@ .
    dimensions :: Lude.Maybe [Dimension],
    -- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
    -- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
    alarmActions :: Lude.Maybe [Lude.Text],
    -- | The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately.
    --
    -- If you don't specify @Unit@ , CloudWatch retrieves all unit types that have been published for the metric and attempts to evaluate the alarm. Usually, metrics are published with only one unit, so the alarm works as intended.
    -- However, if the metric is published with multiple types of units and you don't specify a unit, the alarm's behavior is not defined and it behaves predictably.
    -- We recommend omitting @Unit@ so that you don't inadvertently specify an incorrect unit that is not published for this metric. Doing so causes the alarm to be stuck in the @INSUFFICIENT DATA@ state.
    unit :: Lude.Maybe StandardUnit,
    -- | The statistic for the metric specified in @MetricName@ , other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
    statistic :: Lude.Maybe Statistic,
    -- | A list of key-value pairs to associate with the alarm. You can associate as many as 50 tags with an alarm.
    --
    -- Tags can help you organize and categorize your resources. You can also use them to scope user permissions by granting a user permission to access or change only resources with certain tag values.
    tags :: Lude.Maybe [Tag],
    -- | The percentile statistic for the metric specified in @MetricName@ . Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
    extendedStatistic :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricAlarm' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name for the alarm. This name must be unique within the Region.
-- * 'metrics' - An array of @MetricDataQuery@ structures that enable you to create an alarm based on the result of a metric math expression. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- Each item in the @Metrics@ array either retrieves a metric or performs a math expression.
-- One item in the @Metrics@ array is the expression that the alarm watches. You designate this expression by setting @ReturnData@ to true for this object in the array. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery> .
-- If you use the @Metrics@ parameter, you cannot include the @MetricName@ , @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters of @PutMetricAlarm@ in the same operation. Instead, you retrieve the metrics you are using in your math expression as part of the @Metrics@ array.
-- * 'treatMissingData' - Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> .
--
-- Valid Values: @breaching | notBreaching | ignore | missing@
-- * 'period' - The length, in seconds, used each time the metric specified in @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of 60.
--
-- @Period@ is required for alarms based on static thresholds. If you are creating an alarm based on a metric math expression, you specify the period for each metric within the objects in the @Metrics@ array.
-- Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
-- An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
-- * 'alarmDescription' - The description for the alarm.
-- * 'evaluationPeriods' - The number of periods over which data is compared to the specified threshold. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N.
--
-- An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
-- * 'metricName' - The name for the metric associated with the alarm. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot specify this parameter, or any of the @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters. Instead, you specify all this information in the @Metrics@ array.
-- * 'namespace' - The namespace for the metric associated specified in @MetricName@ .
-- * 'thresholdMetricId' - If this is an alarm based on an anomaly detection model, make this value match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection Model Alarm__ example on this page.
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
-- * 'comparisonOperator' - The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- The values @LessThanLowerOrGreaterThanUpperThreshold@ , @LessThanLowerThreshold@ , and @GreaterThanUpperThreshold@ are used only for alarms based on anomaly detection models.
-- * 'okActions' - The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
-- * 'evaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> .
--
-- Valid Values: @evaluate | ignore@
-- * 'datapointsToAlarm' - The number of data points that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
-- * 'threshold' - The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but should not be used for alarms based on anomaly detection models.
-- * 'actionsEnabled' - Indicates whether actions should be executed during any changes to the alarm state. The default is @TRUE@ .
-- * 'insufficientDataActions' - The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
-- Valid Values (for use with IAM roles): @>arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
-- * 'dimensions' - The dimensions for the metric specified in @MetricName@ .
-- * 'alarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
-- * 'unit' - The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately.
--
-- If you don't specify @Unit@ , CloudWatch retrieves all unit types that have been published for the metric and attempts to evaluate the alarm. Usually, metrics are published with only one unit, so the alarm works as intended.
-- However, if the metric is published with multiple types of units and you don't specify a unit, the alarm's behavior is not defined and it behaves predictably.
-- We recommend omitting @Unit@ so that you don't inadvertently specify an incorrect unit that is not published for this metric. Doing so causes the alarm to be stuck in the @INSUFFICIENT DATA@ state.
-- * 'statistic' - The statistic for the metric specified in @MetricName@ , other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
-- * 'tags' - A list of key-value pairs to associate with the alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions by granting a user permission to access or change only resources with certain tag values.
-- * 'extendedStatistic' - The percentile statistic for the metric specified in @MetricName@ . Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
mkPutMetricAlarm ::
  -- | 'alarmName'
  Lude.Text ->
  -- | 'evaluationPeriods'
  Lude.Natural ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  PutMetricAlarm
mkPutMetricAlarm
  pAlarmName_
  pEvaluationPeriods_
  pComparisonOperator_ =
    PutMetricAlarm'
      { alarmName = pAlarmName_,
        metrics = Lude.Nothing,
        treatMissingData = Lude.Nothing,
        period = Lude.Nothing,
        alarmDescription = Lude.Nothing,
        evaluationPeriods = pEvaluationPeriods_,
        metricName = Lude.Nothing,
        namespace = Lude.Nothing,
        thresholdMetricId = Lude.Nothing,
        comparisonOperator = pComparisonOperator_,
        okActions = Lude.Nothing,
        evaluateLowSampleCountPercentile = Lude.Nothing,
        datapointsToAlarm = Lude.Nothing,
        threshold = Lude.Nothing,
        actionsEnabled = Lude.Nothing,
        insufficientDataActions = Lude.Nothing,
        dimensions = Lude.Nothing,
        alarmActions = Lude.Nothing,
        unit = Lude.Nothing,
        statistic = Lude.Nothing,
        tags = Lude.Nothing,
        extendedStatistic = Lude.Nothing
      }

-- | The name for the alarm. This name must be unique within the Region.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAlarmName :: Lens.Lens' PutMetricAlarm Lude.Text
pmaAlarmName = Lens.lens (alarmName :: PutMetricAlarm -> Lude.Text) (\s a -> s {alarmName = a} :: PutMetricAlarm)
{-# DEPRECATED pmaAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | An array of @MetricDataQuery@ structures that enable you to create an alarm based on the result of a metric math expression. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- Each item in the @Metrics@ array either retrieves a metric or performs a math expression.
-- One item in the @Metrics@ array is the expression that the alarm watches. You designate this expression by setting @ReturnData@ to true for this object in the array. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery> .
-- If you use the @Metrics@ parameter, you cannot include the @MetricName@ , @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters of @PutMetricAlarm@ in the same operation. Instead, you retrieve the metrics you are using in your math expression as part of the @Metrics@ array.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaMetrics :: Lens.Lens' PutMetricAlarm (Lude.Maybe [MetricDataQuery])
pmaMetrics = Lens.lens (metrics :: PutMetricAlarm -> Lude.Maybe [MetricDataQuery]) (\s a -> s {metrics = a} :: PutMetricAlarm)
{-# DEPRECATED pmaMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> .
--
-- Valid Values: @breaching | notBreaching | ignore | missing@
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTreatMissingData :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaTreatMissingData = Lens.lens (treatMissingData :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {treatMissingData = a} :: PutMetricAlarm)
{-# DEPRECATED pmaTreatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead." #-}

-- | The length, in seconds, used each time the metric specified in @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of 60.
--
-- @Period@ is required for alarms based on static thresholds. If you are creating an alarm based on a metric math expression, you specify the period for each metric within the objects in the @Metrics@ array.
-- Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
-- An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaPeriod :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Natural)
pmaPeriod = Lens.lens (period :: PutMetricAlarm -> Lude.Maybe Lude.Natural) (\s a -> s {period = a} :: PutMetricAlarm)
{-# DEPRECATED pmaPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The description for the alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAlarmDescription :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaAlarmDescription = Lens.lens (alarmDescription :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {alarmDescription = a} :: PutMetricAlarm)
{-# DEPRECATED pmaAlarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead." #-}

-- | The number of periods over which data is compared to the specified threshold. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N.
--
-- An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaEvaluationPeriods :: Lens.Lens' PutMetricAlarm Lude.Natural
pmaEvaluationPeriods = Lens.lens (evaluationPeriods :: PutMetricAlarm -> Lude.Natural) (\s a -> s {evaluationPeriods = a} :: PutMetricAlarm)
{-# DEPRECATED pmaEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The name for the metric associated with the alarm. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot specify this parameter, or any of the @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters. Instead, you specify all this information in the @Metrics@ array.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaMetricName :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaMetricName = Lens.lens (metricName :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: PutMetricAlarm)
{-# DEPRECATED pmaMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The namespace for the metric associated specified in @MetricName@ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaNamespace :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaNamespace = Lens.lens (namespace :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: PutMetricAlarm)
{-# DEPRECATED pmaNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | If this is an alarm based on an anomaly detection model, make this value match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection Model Alarm__ example on this page.
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
--
-- /Note:/ Consider using 'thresholdMetricId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaThresholdMetricId :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaThresholdMetricId = Lens.lens (thresholdMetricId :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {thresholdMetricId = a} :: PutMetricAlarm)
{-# DEPRECATED pmaThresholdMetricId "Use generic-lens or generic-optics with 'thresholdMetricId' instead." #-}

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- The values @LessThanLowerOrGreaterThanUpperThreshold@ , @LessThanLowerThreshold@ , and @GreaterThanUpperThreshold@ are used only for alarms based on anomaly detection models.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaComparisonOperator :: Lens.Lens' PutMetricAlarm ComparisonOperator
pmaComparisonOperator = Lens.lens (comparisonOperator :: PutMetricAlarm -> ComparisonOperator) (\s a -> s {comparisonOperator = a} :: PutMetricAlarm)
{-# DEPRECATED pmaComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
--
-- /Note:/ Consider using 'okActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaOKActions :: Lens.Lens' PutMetricAlarm (Lude.Maybe [Lude.Text])
pmaOKActions = Lens.lens (okActions :: PutMetricAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {okActions = a} :: PutMetricAlarm)
{-# DEPRECATED pmaOKActions "Use generic-lens or generic-optics with 'okActions' instead." #-}

-- | Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> .
--
-- Valid Values: @evaluate | ignore@
--
-- /Note:/ Consider using 'evaluateLowSampleCountPercentile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaEvaluateLowSampleCountPercentile :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaEvaluateLowSampleCountPercentile = Lens.lens (evaluateLowSampleCountPercentile :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {evaluateLowSampleCountPercentile = a} :: PutMetricAlarm)
{-# DEPRECATED pmaEvaluateLowSampleCountPercentile "Use generic-lens or generic-optics with 'evaluateLowSampleCountPercentile' instead." #-}

-- | The number of data points that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDatapointsToAlarm :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Natural)
pmaDatapointsToAlarm = Lens.lens (datapointsToAlarm :: PutMetricAlarm -> Lude.Maybe Lude.Natural) (\s a -> s {datapointsToAlarm = a} :: PutMetricAlarm)
{-# DEPRECATED pmaDatapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead." #-}

-- | The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but should not be used for alarms based on anomaly detection models.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaThreshold :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Double)
pmaThreshold = Lens.lens (threshold :: PutMetricAlarm -> Lude.Maybe Lude.Double) (\s a -> s {threshold = a} :: PutMetricAlarm)
{-# DEPRECATED pmaThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | Indicates whether actions should be executed during any changes to the alarm state. The default is @TRUE@ .
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaActionsEnabled :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Bool)
pmaActionsEnabled = Lens.lens (actionsEnabled :: PutMetricAlarm -> Lude.Maybe Lude.Bool) (\s a -> s {actionsEnabled = a} :: PutMetricAlarm)
{-# DEPRECATED pmaActionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead." #-}

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
-- Valid Values (for use with IAM roles): @>arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaInsufficientDataActions :: Lens.Lens' PutMetricAlarm (Lude.Maybe [Lude.Text])
pmaInsufficientDataActions = Lens.lens (insufficientDataActions :: PutMetricAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {insufficientDataActions = a} :: PutMetricAlarm)
{-# DEPRECATED pmaInsufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead." #-}

-- | The dimensions for the metric specified in @MetricName@ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDimensions :: Lens.Lens' PutMetricAlarm (Lude.Maybe [Dimension])
pmaDimensions = Lens.lens (dimensions :: PutMetricAlarm -> Lude.Maybe [Dimension]) (\s a -> s {dimensions = a} :: PutMetricAlarm)
{-# DEPRECATED pmaDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAlarmActions :: Lens.Lens' PutMetricAlarm (Lude.Maybe [Lude.Text])
pmaAlarmActions = Lens.lens (alarmActions :: PutMetricAlarm -> Lude.Maybe [Lude.Text]) (\s a -> s {alarmActions = a} :: PutMetricAlarm)
{-# DEPRECATED pmaAlarmActions "Use generic-lens or generic-optics with 'alarmActions' instead." #-}

-- | The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately.
--
-- If you don't specify @Unit@ , CloudWatch retrieves all unit types that have been published for the metric and attempts to evaluate the alarm. Usually, metrics are published with only one unit, so the alarm works as intended.
-- However, if the metric is published with multiple types of units and you don't specify a unit, the alarm's behavior is not defined and it behaves predictably.
-- We recommend omitting @Unit@ so that you don't inadvertently specify an incorrect unit that is not published for this metric. Doing so causes the alarm to be stuck in the @INSUFFICIENT DATA@ state.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaUnit :: Lens.Lens' PutMetricAlarm (Lude.Maybe StandardUnit)
pmaUnit = Lens.lens (unit :: PutMetricAlarm -> Lude.Maybe StandardUnit) (\s a -> s {unit = a} :: PutMetricAlarm)
{-# DEPRECATED pmaUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The statistic for the metric specified in @MetricName@ , other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaStatistic :: Lens.Lens' PutMetricAlarm (Lude.Maybe Statistic)
pmaStatistic = Lens.lens (statistic :: PutMetricAlarm -> Lude.Maybe Statistic) (\s a -> s {statistic = a} :: PutMetricAlarm)
{-# DEPRECATED pmaStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | A list of key-value pairs to associate with the alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions by granting a user permission to access or change only resources with certain tag values.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTags :: Lens.Lens' PutMetricAlarm (Lude.Maybe [Tag])
pmaTags = Lens.lens (tags :: PutMetricAlarm -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutMetricAlarm)
{-# DEPRECATED pmaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The percentile statistic for the metric specified in @MetricName@ . Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaExtendedStatistic :: Lens.Lens' PutMetricAlarm (Lude.Maybe Lude.Text)
pmaExtendedStatistic = Lens.lens (extendedStatistic :: PutMetricAlarm -> Lude.Maybe Lude.Text) (\s a -> s {extendedStatistic = a} :: PutMetricAlarm)
{-# DEPRECATED pmaExtendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead." #-}

instance Lude.AWSRequest PutMetricAlarm where
  type Rs PutMetricAlarm = PutMetricAlarmResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull PutMetricAlarmResponse'

instance Lude.ToHeaders PutMetricAlarm where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutMetricAlarm where
  toPath = Lude.const "/"

instance Lude.ToQuery PutMetricAlarm where
  toQuery PutMetricAlarm' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutMetricAlarm" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmName" Lude.=: alarmName,
        "Metrics"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> metrics),
        "TreatMissingData" Lude.=: treatMissingData,
        "Period" Lude.=: period,
        "AlarmDescription" Lude.=: alarmDescription,
        "EvaluationPeriods" Lude.=: evaluationPeriods,
        "MetricName" Lude.=: metricName,
        "Namespace" Lude.=: namespace,
        "ThresholdMetricId" Lude.=: thresholdMetricId,
        "ComparisonOperator" Lude.=: comparisonOperator,
        "OKActions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> okActions),
        "EvaluateLowSampleCountPercentile"
          Lude.=: evaluateLowSampleCountPercentile,
        "DatapointsToAlarm" Lude.=: datapointsToAlarm,
        "Threshold" Lude.=: threshold,
        "ActionsEnabled" Lude.=: actionsEnabled,
        "InsufficientDataActions"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> insufficientDataActions),
        "Dimensions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> dimensions),
        "AlarmActions"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alarmActions),
        "Unit" Lude.=: unit,
        "Statistic" Lude.=: statistic,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "ExtendedStatistic" Lude.=: extendedStatistic
      ]

-- | /See:/ 'mkPutMetricAlarmResponse' smart constructor.
data PutMetricAlarmResponse = PutMetricAlarmResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricAlarmResponse' with the minimum fields required to make a request.
mkPutMetricAlarmResponse ::
  PutMetricAlarmResponse
mkPutMetricAlarmResponse = PutMetricAlarmResponse'
