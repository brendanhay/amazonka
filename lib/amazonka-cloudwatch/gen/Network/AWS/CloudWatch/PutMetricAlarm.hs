{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutMetricAlarm (..)
    , mkPutMetricAlarm
    -- ** Request lenses
    , pmaAlarmName
    , pmaEvaluationPeriods
    , pmaComparisonOperator
    , pmaActionsEnabled
    , pmaAlarmActions
    , pmaAlarmDescription
    , pmaDatapointsToAlarm
    , pmaDimensions
    , pmaEvaluateLowSampleCountPercentile
    , pmaExtendedStatistic
    , pmaInsufficientDataActions
    , pmaMetricName
    , pmaMetrics
    , pmaNamespace
    , pmaOKActions
    , pmaPeriod
    , pmaStatistic
    , pmaTags
    , pmaThreshold
    , pmaThresholdMetricId
    , pmaTreatMissingData
    , pmaUnit

    -- * Destructuring the response
    , PutMetricAlarmResponse (..)
    , mkPutMetricAlarmResponse
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutMetricAlarm' smart constructor.
data PutMetricAlarm = PutMetricAlarm'
  { alarmName :: Types.AlarmName
    -- ^ The name for the alarm. This name must be unique within the Region.
  , evaluationPeriods :: Core.Natural
    -- ^ The number of periods over which data is compared to the specified threshold. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N.
--
-- An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
  , comparisonOperator :: Types.ComparisonOperator
    -- ^ The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- The values @LessThanLowerOrGreaterThanUpperThreshold@ , @LessThanLowerThreshold@ , and @GreaterThanUpperThreshold@ are used only for alarms based on anomaly detection models.
  , actionsEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether actions should be executed during any changes to the alarm state. The default is @TRUE@ .
  , alarmActions :: Core.Maybe [Types.ResourceName]
    -- ^ The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @ 
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@ 
  , alarmDescription :: Core.Maybe Types.AlarmDescription
    -- ^ The description for the alarm.
  , datapointsToAlarm :: Core.Maybe Core.Natural
    -- ^ The number of data points that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
  , dimensions :: Core.Maybe [Types.Dimension]
    -- ^ The dimensions for the metric specified in @MetricName@ .
  , evaluateLowSampleCountPercentile :: Core.Maybe Types.EvaluateLowSampleCountPercentile
    -- ^ Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> .
--
-- Valid Values: @evaluate | ignore@ 
  , extendedStatistic :: Core.Maybe Types.ExtendedStatistic
    -- ^ The percentile statistic for the metric specified in @MetricName@ . Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
  , insufficientDataActions :: Core.Maybe [Types.ResourceName]
    -- ^ The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @ 
-- Valid Values (for use with IAM roles): @>arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@ 
  , metricName :: Core.Maybe Types.MetricName
    -- ^ The name for the metric associated with the alarm. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot specify this parameter, or any of the @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters. Instead, you specify all this information in the @Metrics@ array.
  , metrics :: Core.Maybe [Types.MetricDataQuery]
    -- ^ An array of @MetricDataQuery@ structures that enable you to create an alarm based on the result of a metric math expression. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- Each item in the @Metrics@ array either retrieves a metric or performs a math expression.
-- One item in the @Metrics@ array is the expression that the alarm watches. You designate this expression by setting @ReturnData@ to true for this object in the array. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery> .
-- If you use the @Metrics@ parameter, you cannot include the @MetricName@ , @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters of @PutMetricAlarm@ in the same operation. Instead, you retrieve the metrics you are using in your math expression as part of the @Metrics@ array.
  , namespace :: Core.Maybe Types.Namespace
    -- ^ The namespace for the metric associated specified in @MetricName@ .
  , oKActions :: Core.Maybe [Types.ResourceName]
    -- ^ The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @ 
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@ 
  , period :: Core.Maybe Core.Natural
    -- ^ The length, in seconds, used each time the metric specified in @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of 60.
--
-- @Period@ is required for alarms based on static thresholds. If you are creating an alarm based on a metric math expression, you specify the period for each metric within the objects in the @Metrics@ array.
-- Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
-- An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
  , statistic :: Core.Maybe Types.Statistic
    -- ^ The statistic for the metric specified in @MetricName@ , other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of key-value pairs to associate with the alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions by granting a user permission to access or change only resources with certain tag values.
  , threshold :: Core.Maybe Core.Double
    -- ^ The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but should not be used for alarms based on anomaly detection models.
  , thresholdMetricId :: Core.Maybe Types.MetricId
    -- ^ If this is an alarm based on an anomaly detection model, make this value match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection Model Alarm__ example on this page.
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
  , treatMissingData :: Core.Maybe Types.TreatMissingData
    -- ^ Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> .
--
-- Valid Values: @breaching | notBreaching | ignore | missing@ 
  , unit :: Core.Maybe Types.StandardUnit
    -- ^ The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately.
--
-- If you don't specify @Unit@ , CloudWatch retrieves all unit types that have been published for the metric and attempts to evaluate the alarm. Usually, metrics are published with only one unit, so the alarm works as intended.
-- However, if the metric is published with multiple types of units and you don't specify a unit, the alarm's behavior is not defined and it behaves predictably.
-- We recommend omitting @Unit@ so that you don't inadvertently specify an incorrect unit that is not published for this metric. Doing so causes the alarm to be stuck in the @INSUFFICIENT DATA@ state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricAlarm' value with any optional fields omitted.
mkPutMetricAlarm
    :: Types.AlarmName -- ^ 'alarmName'
    -> Core.Natural -- ^ 'evaluationPeriods'
    -> Types.ComparisonOperator -- ^ 'comparisonOperator'
    -> PutMetricAlarm
mkPutMetricAlarm alarmName evaluationPeriods comparisonOperator
  = PutMetricAlarm'{alarmName, evaluationPeriods, comparisonOperator,
                    actionsEnabled = Core.Nothing, alarmActions = Core.Nothing,
                    alarmDescription = Core.Nothing, datapointsToAlarm = Core.Nothing,
                    dimensions = Core.Nothing,
                    evaluateLowSampleCountPercentile = Core.Nothing,
                    extendedStatistic = Core.Nothing,
                    insufficientDataActions = Core.Nothing, metricName = Core.Nothing,
                    metrics = Core.Nothing, namespace = Core.Nothing,
                    oKActions = Core.Nothing, period = Core.Nothing,
                    statistic = Core.Nothing, tags = Core.Nothing,
                    threshold = Core.Nothing, thresholdMetricId = Core.Nothing,
                    treatMissingData = Core.Nothing, unit = Core.Nothing}

-- | The name for the alarm. This name must be unique within the Region.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAlarmName :: Lens.Lens' PutMetricAlarm Types.AlarmName
pmaAlarmName = Lens.field @"alarmName"
{-# INLINEABLE pmaAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | The number of periods over which data is compared to the specified threshold. If you are setting an alarm that requires that a number of consecutive data points be breaching to trigger the alarm, this value specifies that number. If you are setting an "M out of N" alarm, this value is the N.
--
-- An alarm's total current evaluation period can be no longer than one day, so this number multiplied by @Period@ cannot be more than 86,400 seconds.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaEvaluationPeriods :: Lens.Lens' PutMetricAlarm Core.Natural
pmaEvaluationPeriods = Lens.field @"evaluationPeriods"
{-# INLINEABLE pmaEvaluationPeriods #-}
{-# DEPRECATED evaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead"  #-}

-- | The arithmetic operation to use when comparing the specified statistic and threshold. The specified statistic value is used as the first operand.
--
-- The values @LessThanLowerOrGreaterThanUpperThreshold@ , @LessThanLowerThreshold@ , and @GreaterThanUpperThreshold@ are used only for alarms based on anomaly detection models.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaComparisonOperator :: Lens.Lens' PutMetricAlarm Types.ComparisonOperator
pmaComparisonOperator = Lens.field @"comparisonOperator"
{-# INLINEABLE pmaComparisonOperator #-}
{-# DEPRECATED comparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead"  #-}

-- | Indicates whether actions should be executed during any changes to the alarm state. The default is @TRUE@ .
--
-- /Note:/ Consider using 'actionsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaActionsEnabled :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Bool)
pmaActionsEnabled = Lens.field @"actionsEnabled"
{-# INLINEABLE pmaActionsEnabled #-}
{-# DEPRECATED actionsEnabled "Use generic-lens or generic-optics with 'actionsEnabled' instead"  #-}

-- | The actions to execute when this alarm transitions to the @ALARM@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @ 
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@ 
--
-- /Note:/ Consider using 'alarmActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAlarmActions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Types.ResourceName])
pmaAlarmActions = Lens.field @"alarmActions"
{-# INLINEABLE pmaAlarmActions #-}
{-# DEPRECATED alarmActions "Use generic-lens or generic-optics with 'alarmActions' instead"  #-}

-- | The description for the alarm.
--
-- /Note:/ Consider using 'alarmDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAlarmDescription :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.AlarmDescription)
pmaAlarmDescription = Lens.field @"alarmDescription"
{-# INLINEABLE pmaAlarmDescription #-}
{-# DEPRECATED alarmDescription "Use generic-lens or generic-optics with 'alarmDescription' instead"  #-}

-- | The number of data points that must be breaching to trigger the alarm. This is used only if you are setting an "M out of N" alarm. In that case, this value is the M. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDatapointsToAlarm :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Natural)
pmaDatapointsToAlarm = Lens.field @"datapointsToAlarm"
{-# INLINEABLE pmaDatapointsToAlarm #-}
{-# DEPRECATED datapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead"  #-}

-- | The dimensions for the metric specified in @MetricName@ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDimensions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Types.Dimension])
pmaDimensions = Lens.field @"dimensions"
{-# INLINEABLE pmaDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | Used only for alarms based on percentiles. If you specify @ignore@ , the alarm state does not change during periods with too few data points to be statistically significant. If you specify @evaluate@ or omit this parameter, the alarm is always evaluated and possibly changes state no matter how many data points are available. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples> .
--
-- Valid Values: @evaluate | ignore@ 
--
-- /Note:/ Consider using 'evaluateLowSampleCountPercentile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaEvaluateLowSampleCountPercentile :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.EvaluateLowSampleCountPercentile)
pmaEvaluateLowSampleCountPercentile = Lens.field @"evaluateLowSampleCountPercentile"
{-# INLINEABLE pmaEvaluateLowSampleCountPercentile #-}
{-# DEPRECATED evaluateLowSampleCountPercentile "Use generic-lens or generic-optics with 'evaluateLowSampleCountPercentile' instead"  #-}

-- | The percentile statistic for the metric specified in @MetricName@ . Specify a value between p0.0 and p100. When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- /Note:/ Consider using 'extendedStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaExtendedStatistic :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.ExtendedStatistic)
pmaExtendedStatistic = Lens.field @"extendedStatistic"
{-# INLINEABLE pmaExtendedStatistic #-}
{-# DEPRECATED extendedStatistic "Use generic-lens or generic-optics with 'extendedStatistic' instead"  #-}

-- | The actions to execute when this alarm transitions to the @INSUFFICIENT_DATA@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @ 
-- Valid Values (for use with IAM roles): @>arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@ 
--
-- /Note:/ Consider using 'insufficientDataActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaInsufficientDataActions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Types.ResourceName])
pmaInsufficientDataActions = Lens.field @"insufficientDataActions"
{-# INLINEABLE pmaInsufficientDataActions #-}
{-# DEPRECATED insufficientDataActions "Use generic-lens or generic-optics with 'insufficientDataActions' instead"  #-}

-- | The name for the metric associated with the alarm. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot specify this parameter, or any of the @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters. Instead, you specify all this information in the @Metrics@ array.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaMetricName :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.MetricName)
pmaMetricName = Lens.field @"metricName"
{-# INLINEABLE pmaMetricName #-}
{-# DEPRECATED metricName "Use generic-lens or generic-optics with 'metricName' instead"  #-}

-- | An array of @MetricDataQuery@ structures that enable you to create an alarm based on the result of a metric math expression. For each @PutMetricAlarm@ operation, you must specify either @MetricName@ or a @Metrics@ array.
--
-- Each item in the @Metrics@ array either retrieves a metric or performs a math expression.
-- One item in the @Metrics@ array is the expression that the alarm watches. You designate this expression by setting @ReturnData@ to true for this object in the array. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery> .
-- If you use the @Metrics@ parameter, you cannot include the @MetricName@ , @Dimensions@ , @Period@ , @Namespace@ , @Statistic@ , or @ExtendedStatistic@ parameters of @PutMetricAlarm@ in the same operation. Instead, you retrieve the metrics you are using in your math expression as part of the @Metrics@ array.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaMetrics :: Lens.Lens' PutMetricAlarm (Core.Maybe [Types.MetricDataQuery])
pmaMetrics = Lens.field @"metrics"
{-# INLINEABLE pmaMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The namespace for the metric associated specified in @MetricName@ .
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaNamespace :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.Namespace)
pmaNamespace = Lens.field @"namespace"
{-# INLINEABLE pmaNamespace #-}
{-# DEPRECATED namespace "Use generic-lens or generic-optics with 'namespace' instead"  #-}

-- | The actions to execute when this alarm transitions to an @OK@ state from any other state. Each action is specified as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:/region/ :ec2:stop@ | @arn:aws:automate:/region/ :ec2:terminate@ | @arn:aws:automate:/region/ :ec2:recover@ | @arn:aws:automate:/region/ :ec2:reboot@ | @arn:aws:sns:/region/ :/account-id/ :/sns-topic-name/ @ | @arn:aws:autoscaling:/region/ :/account-id/ :scalingPolicy:/policy-id/ :autoScalingGroupName//group-friendly-name/ :policyName//policy-friendly-name/ @ 
-- Valid Values (for use with IAM roles): @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Stop/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Terminate/1.0@ | @arn:aws:swf:/region/ :/account-id/ :action/actions/AWS_EC2.InstanceId.Reboot/1.0@ 
--
-- /Note:/ Consider using 'oKActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaOKActions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Types.ResourceName])
pmaOKActions = Lens.field @"oKActions"
{-# INLINEABLE pmaOKActions #-}
{-# DEPRECATED oKActions "Use generic-lens or generic-optics with 'oKActions' instead"  #-}

-- | The length, in seconds, used each time the metric specified in @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of 60.
--
-- @Period@ is required for alarms based on static thresholds. If you are creating an alarm based on a metric math expression, you specify the period for each metric within the objects in the @Metrics@ array.
-- Be sure to specify 10 or 30 only for metrics that are stored by a @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a period of 10 or 30 for a metric that does not have sub-minute resolution, the alarm still attempts to gather data at the period rate that you specify. In this case, it does not receive data for the attempts that do not correspond to a one-minute data resolution, and the alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or 30 also sets this alarm as a high-resolution alarm, which has a higher charge than other alarms. For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
-- An alarm's total current evaluation period can be no longer than one day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than 86,400 seconds.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaPeriod :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Natural)
pmaPeriod = Lens.field @"period"
{-# INLINEABLE pmaPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

-- | The statistic for the metric specified in @MetricName@ , other than percentile. For percentile statistics, use @ExtendedStatistic@ . When you call @PutMetricAlarm@ and specify a @MetricName@ , you must specify either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaStatistic :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.Statistic)
pmaStatistic = Lens.field @"statistic"
{-# INLINEABLE pmaStatistic #-}
{-# DEPRECATED statistic "Use generic-lens or generic-optics with 'statistic' instead"  #-}

-- | A list of key-value pairs to associate with the alarm. You can associate as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions by granting a user permission to access or change only resources with certain tag values.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTags :: Lens.Lens' PutMetricAlarm (Core.Maybe [Types.Tag])
pmaTags = Lens.field @"tags"
{-# INLINEABLE pmaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but should not be used for alarms based on anomaly detection models.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaThreshold :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Double)
pmaThreshold = Lens.field @"threshold"
{-# INLINEABLE pmaThreshold #-}
{-# DEPRECATED threshold "Use generic-lens or generic-optics with 'threshold' instead"  #-}

-- | If this is an alarm based on an anomaly detection model, make this value match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection Model Alarm__ example on this page.
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
--
-- /Note:/ Consider using 'thresholdMetricId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaThresholdMetricId :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.MetricId)
pmaThresholdMetricId = Lens.field @"thresholdMetricId"
{-# INLINEABLE pmaThresholdMetricId #-}
{-# DEPRECATED thresholdMetricId "Use generic-lens or generic-optics with 'thresholdMetricId' instead"  #-}

-- | Sets how this alarm is to handle missing data points. If @TreatMissingData@ is omitted, the default behavior of @missing@ is used. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data> .
--
-- Valid Values: @breaching | notBreaching | ignore | missing@ 
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaTreatMissingData :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.TreatMissingData)
pmaTreatMissingData = Lens.field @"treatMissingData"
{-# INLINEABLE pmaTreatMissingData #-}
{-# DEPRECATED treatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead"  #-}

-- | The unit of measure for the statistic. For example, the units for the Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the number of bytes that an instance receives on all network interfaces. You can also specify a unit when you create a custom metric. Units help provide conceptual meaning to your data. Metric data points that specify a unit of measure, such as Percent, are aggregated separately.
--
-- If you don't specify @Unit@ , CloudWatch retrieves all unit types that have been published for the metric and attempts to evaluate the alarm. Usually, metrics are published with only one unit, so the alarm works as intended.
-- However, if the metric is published with multiple types of units and you don't specify a unit, the alarm's behavior is not defined and it behaves predictably.
-- We recommend omitting @Unit@ so that you don't inadvertently specify an incorrect unit that is not published for this metric. Doing so causes the alarm to be stuck in the @INSUFFICIENT DATA@ state.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaUnit :: Lens.Lens' PutMetricAlarm (Core.Maybe Types.StandardUnit)
pmaUnit = Lens.field @"unit"
{-# INLINEABLE pmaUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.ToQuery PutMetricAlarm where
        toQuery PutMetricAlarm{..}
          = Core.toQueryPair "Action" ("PutMetricAlarm" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "AlarmName" alarmName
              Core.<> Core.toQueryPair "EvaluationPeriods" evaluationPeriods
              Core.<> Core.toQueryPair "ComparisonOperator" comparisonOperator
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ActionsEnabled")
                actionsEnabled
              Core.<>
              Core.toQueryPair "AlarmActions"
                (Core.maybe Core.mempty (Core.toQueryList "member") alarmActions)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AlarmDescription")
                alarmDescription
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DatapointsToAlarm")
                datapointsToAlarm
              Core.<>
              Core.toQueryPair "Dimensions"
                (Core.maybe Core.mempty (Core.toQueryList "member") dimensions)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EvaluateLowSampleCountPercentile")
                evaluateLowSampleCountPercentile
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ExtendedStatistic")
                extendedStatistic
              Core.<>
              Core.toQueryPair "InsufficientDataActions"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   insufficientDataActions)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetricName") metricName
              Core.<>
              Core.toQueryPair "Metrics"
                (Core.maybe Core.mempty (Core.toQueryList "member") metrics)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Namespace") namespace
              Core.<>
              Core.toQueryPair "OKActions"
                (Core.maybe Core.mempty (Core.toQueryList "member") oKActions)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Period") period
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Statistic") statistic
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Threshold") threshold
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ThresholdMetricId")
                thresholdMetricId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TreatMissingData")
                treatMissingData
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Unit") unit

instance Core.ToHeaders PutMetricAlarm where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutMetricAlarm where
        type Rs PutMetricAlarm = PutMetricAlarmResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutMetricAlarmResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutMetricAlarmResponse' smart constructor.
data PutMetricAlarmResponse = PutMetricAlarmResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricAlarmResponse' value with any optional fields omitted.
mkPutMetricAlarmResponse
    :: PutMetricAlarmResponse
mkPutMetricAlarmResponse = PutMetricAlarmResponse'
