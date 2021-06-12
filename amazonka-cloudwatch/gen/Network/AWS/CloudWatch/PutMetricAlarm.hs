{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricAlarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an alarm and associates it with the specified metric,
-- metric math expression, or anomaly detection model.
--
-- Alarms based on anomaly detection models cannot have Auto Scaling
-- actions.
--
-- When this operation creates an alarm, the alarm state is immediately set
-- to @INSUFFICIENT_DATA@. The alarm is then evaluated and its state is set
-- appropriately. Any actions associated with the new state are then
-- executed.
--
-- When you update an existing alarm, its state is left unchanged, but the
-- update completely overwrites the previous configuration of the alarm.
--
-- If you are an IAM user, you must have Amazon EC2 permissions for some
-- alarm operations:
--
-- -   The @iam:CreateServiceLinkedRole@ for all alarms with EC2 actions
--
-- -   The @iam:CreateServiceLinkedRole@ to create an alarm with Systems
--     Manager OpsItem actions.
--
-- The first time you create an alarm in the AWS Management Console, the
-- CLI, or by using the PutMetricAlarm API, CloudWatch creates the
-- necessary service-linked rolea for you. The service-linked roles are
-- called @AWSServiceRoleForCloudWatchEvents@ and
-- @AWSServiceRoleForCloudWatchAlarms_ActionSSM@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role AWS service-linked role>.
module Network.AWS.CloudWatch.PutMetricAlarm
  ( -- * Creating a Request
    PutMetricAlarm (..),
    newPutMetricAlarm,

    -- * Request Lenses
    putMetricAlarm_threshold,
    putMetricAlarm_datapointsToAlarm,
    putMetricAlarm_evaluateLowSampleCountPercentile,
    putMetricAlarm_extendedStatistic,
    putMetricAlarm_alarmActions,
    putMetricAlarm_unit,
    putMetricAlarm_thresholdMetricId,
    putMetricAlarm_metricName,
    putMetricAlarm_insufficientDataActions,
    putMetricAlarm_treatMissingData,
    putMetricAlarm_metrics,
    putMetricAlarm_tags,
    putMetricAlarm_oKActions,
    putMetricAlarm_statistic,
    putMetricAlarm_dimensions,
    putMetricAlarm_namespace,
    putMetricAlarm_actionsEnabled,
    putMetricAlarm_alarmDescription,
    putMetricAlarm_period,
    putMetricAlarm_alarmName,
    putMetricAlarm_evaluationPeriods,
    putMetricAlarm_comparisonOperator,

    -- * Destructuring the Response
    PutMetricAlarmResponse (..),
    newPutMetricAlarmResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutMetricAlarm' smart constructor.
data PutMetricAlarm = PutMetricAlarm'
  { -- | The value against which the specified statistic is compared.
    --
    -- This parameter is required for alarms based on static thresholds, but
    -- should not be used for alarms based on anomaly detection models.
    threshold :: Core.Maybe Core.Double,
    -- | The number of data points that must be breaching to trigger the alarm.
    -- This is used only if you are setting an \"M out of N\" alarm. In that
    -- case, this value is the M. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm>
    -- in the /Amazon CloudWatch User Guide/.
    datapointsToAlarm :: Core.Maybe Core.Natural,
    -- | Used only for alarms based on percentiles. If you specify @ignore@, the
    -- alarm state does not change during periods with too few data points to
    -- be statistically significant. If you specify @evaluate@ or omit this
    -- parameter, the alarm is always evaluated and possibly changes state no
    -- matter how many data points are available. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples>.
    --
    -- Valid Values: @evaluate | ignore@
    evaluateLowSampleCountPercentile :: Core.Maybe Core.Text,
    -- | The percentile statistic for the metric specified in @MetricName@.
    -- Specify a value between p0.0 and p100. When you call @PutMetricAlarm@
    -- and specify a @MetricName@, you must specify either @Statistic@ or
    -- @ExtendedStatistic,@ but not both.
    extendedStatistic :: Core.Maybe Core.Text,
    -- | The actions to execute when this alarm transitions to the @ALARM@ state
    -- from any other state. Each action is specified as an Amazon Resource
    -- Name (ARN).
    --
    -- Valid Values: @arn:aws:automate:region:ec2:stop@ |
    -- @arn:aws:automate:region:ec2:terminate@ |
    -- @arn:aws:automate:region:ec2:recover@ |
    -- @arn:aws:automate:region:ec2:reboot@ |
    -- @arn:aws:sns:region:account-id:sns-topic-name @ |
    -- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
    -- | @arn:aws:ssm:region:account-id:opsitem:severity @
    --
    -- Valid Values (for use with IAM roles):
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
    alarmActions :: Core.Maybe [Core.Text],
    -- | The unit of measure for the statistic. For example, the units for the
    -- Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the
    -- number of bytes that an instance receives on all network interfaces. You
    -- can also specify a unit when you create a custom metric. Units help
    -- provide conceptual meaning to your data. Metric data points that specify
    -- a unit of measure, such as Percent, are aggregated separately.
    --
    -- If you don\'t specify @Unit@, CloudWatch retrieves all unit types that
    -- have been published for the metric and attempts to evaluate the alarm.
    -- Usually, metrics are published with only one unit, so the alarm works as
    -- intended.
    --
    -- However, if the metric is published with multiple types of units and you
    -- don\'t specify a unit, the alarm\'s behavior is not defined and it
    -- behaves predictably.
    --
    -- We recommend omitting @Unit@ so that you don\'t inadvertently specify an
    -- incorrect unit that is not published for this metric. Doing so causes
    -- the alarm to be stuck in the @INSUFFICIENT DATA@ state.
    unit :: Core.Maybe StandardUnit,
    -- | If this is an alarm based on an anomaly detection model, make this value
    -- match the ID of the @ANOMALY_DETECTION_BAND@ function.
    --
    -- For an example of how to use this parameter, see the __Anomaly Detection
    -- Model Alarm__ example on this page.
    --
    -- If your alarm uses this parameter, it cannot have Auto Scaling actions.
    thresholdMetricId :: Core.Maybe Core.Text,
    -- | The name for the metric associated with the alarm. For each
    -- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
    -- @Metrics@ array.
    --
    -- If you are creating an alarm based on a math expression, you cannot
    -- specify this parameter, or any of the @Dimensions@, @Period@,
    -- @Namespace@, @Statistic@, or @ExtendedStatistic@ parameters. Instead,
    -- you specify all this information in the @Metrics@ array.
    metricName :: Core.Maybe Core.Text,
    -- | The actions to execute when this alarm transitions to the
    -- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
    -- as an Amazon Resource Name (ARN).
    --
    -- Valid Values: @arn:aws:automate:region:ec2:stop@ |
    -- @arn:aws:automate:region:ec2:terminate@ |
    -- @arn:aws:automate:region:ec2:recover@ |
    -- @arn:aws:automate:region:ec2:reboot@ |
    -- @arn:aws:sns:region:account-id:sns-topic-name @ |
    -- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
    --
    -- Valid Values (for use with IAM roles):
    -- @>arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
    insufficientDataActions :: Core.Maybe [Core.Text],
    -- | Sets how this alarm is to handle missing data points. If
    -- @TreatMissingData@ is omitted, the default behavior of @missing@ is
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data>.
    --
    -- Valid Values: @breaching | notBreaching | ignore | missing@
    treatMissingData :: Core.Maybe Core.Text,
    -- | An array of @MetricDataQuery@ structures that enable you to create an
    -- alarm based on the result of a metric math expression. For each
    -- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
    -- @Metrics@ array.
    --
    -- Each item in the @Metrics@ array either retrieves a metric or performs a
    -- math expression.
    --
    -- One item in the @Metrics@ array is the expression that the alarm
    -- watches. You designate this expression by setting @ReturnData@ to true
    -- for this object in the array. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery>.
    --
    -- If you use the @Metrics@ parameter, you cannot include the @MetricName@,
    -- @Dimensions@, @Period@, @Namespace@, @Statistic@, or @ExtendedStatistic@
    -- parameters of @PutMetricAlarm@ in the same operation. Instead, you
    -- retrieve the metrics you are using in your math expression as part of
    -- the @Metrics@ array.
    metrics :: Core.Maybe [MetricDataQuery],
    -- | A list of key-value pairs to associate with the alarm. You can associate
    -- as many as 50 tags with an alarm.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- If you are using this operation to update an existing alarm, any tags
    -- you specify in this parameter are ignored. To change the tags of an
    -- existing alarm, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_UntagResource.html UntagResource>.
    tags :: Core.Maybe [Tag],
    -- | The actions to execute when this alarm transitions to an @OK@ state from
    -- any other state. Each action is specified as an Amazon Resource Name
    -- (ARN).
    --
    -- Valid Values: @arn:aws:automate:region:ec2:stop@ |
    -- @arn:aws:automate:region:ec2:terminate@ |
    -- @arn:aws:automate:region:ec2:recover@ |
    -- @arn:aws:automate:region:ec2:reboot@ |
    -- @arn:aws:sns:region:account-id:sns-topic-name @ |
    -- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
    --
    -- Valid Values (for use with IAM roles):
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
    oKActions :: Core.Maybe [Core.Text],
    -- | The statistic for the metric specified in @MetricName@, other than
    -- percentile. For percentile statistics, use @ExtendedStatistic@. When you
    -- call @PutMetricAlarm@ and specify a @MetricName@, you must specify
    -- either @Statistic@ or @ExtendedStatistic,@ but not both.
    statistic :: Core.Maybe Statistic,
    -- | The dimensions for the metric specified in @MetricName@.
    dimensions :: Core.Maybe [Dimension],
    -- | The namespace for the metric associated specified in @MetricName@.
    namespace :: Core.Maybe Core.Text,
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state. The default is @TRUE@.
    actionsEnabled :: Core.Maybe Core.Bool,
    -- | The description for the alarm.
    alarmDescription :: Core.Maybe Core.Text,
    -- | The length, in seconds, used each time the metric specified in
    -- @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of
    -- 60.
    --
    -- @Period@ is required for alarms based on static thresholds. If you are
    -- creating an alarm based on a metric math expression, you specify the
    -- period for each metric within the objects in the @Metrics@ array.
    --
    -- Be sure to specify 10 or 30 only for metrics that are stored by a
    -- @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a
    -- period of 10 or 30 for a metric that does not have sub-minute
    -- resolution, the alarm still attempts to gather data at the period rate
    -- that you specify. In this case, it does not receive data for the
    -- attempts that do not correspond to a one-minute data resolution, and the
    -- alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or
    -- 30 also sets this alarm as a high-resolution alarm, which has a higher
    -- charge than other alarms. For more information about pricing, see
    -- <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
    --
    -- An alarm\'s total current evaluation period can be no longer than one
    -- day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than
    -- 86,400 seconds.
    period :: Core.Maybe Core.Natural,
    -- | The name for the alarm. This name must be unique within the Region.
    alarmName :: Core.Text,
    -- | The number of periods over which data is compared to the specified
    -- threshold. If you are setting an alarm that requires that a number of
    -- consecutive data points be breaching to trigger the alarm, this value
    -- specifies that number. If you are setting an \"M out of N\" alarm, this
    -- value is the N.
    --
    -- An alarm\'s total current evaluation period can be no longer than one
    -- day, so this number multiplied by @Period@ cannot be more than 86,400
    -- seconds.
    evaluationPeriods :: Core.Natural,
    -- | The arithmetic operation to use when comparing the specified statistic
    -- and threshold. The specified statistic value is used as the first
    -- operand.
    --
    -- The values @LessThanLowerOrGreaterThanUpperThreshold@,
    -- @LessThanLowerThreshold@, and @GreaterThanUpperThreshold@ are used only
    -- for alarms based on anomaly detection models.
    comparisonOperator :: ComparisonOperator
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutMetricAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threshold', 'putMetricAlarm_threshold' - The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but
-- should not be used for alarms based on anomaly detection models.
--
-- 'datapointsToAlarm', 'putMetricAlarm_datapointsToAlarm' - The number of data points that must be breaching to trigger the alarm.
-- This is used only if you are setting an \"M out of N\" alarm. In that
-- case, this value is the M. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm>
-- in the /Amazon CloudWatch User Guide/.
--
-- 'evaluateLowSampleCountPercentile', 'putMetricAlarm_evaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If you specify @ignore@, the
-- alarm state does not change during periods with too few data points to
-- be statistically significant. If you specify @evaluate@ or omit this
-- parameter, the alarm is always evaluated and possibly changes state no
-- matter how many data points are available. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples>.
--
-- Valid Values: @evaluate | ignore@
--
-- 'extendedStatistic', 'putMetricAlarm_extendedStatistic' - The percentile statistic for the metric specified in @MetricName@.
-- Specify a value between p0.0 and p100. When you call @PutMetricAlarm@
-- and specify a @MetricName@, you must specify either @Statistic@ or
-- @ExtendedStatistic,@ but not both.
--
-- 'alarmActions', 'putMetricAlarm_alarmActions' - The actions to execute when this alarm transitions to the @ALARM@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- Valid Values: @arn:aws:automate:region:ec2:stop@ |
-- @arn:aws:automate:region:ec2:terminate@ |
-- @arn:aws:automate:region:ec2:recover@ |
-- @arn:aws:automate:region:ec2:reboot@ |
-- @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
-- | @arn:aws:ssm:region:account-id:opsitem:severity @
--
-- Valid Values (for use with IAM roles):
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
--
-- 'unit', 'putMetricAlarm_unit' - The unit of measure for the statistic. For example, the units for the
-- Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the
-- number of bytes that an instance receives on all network interfaces. You
-- can also specify a unit when you create a custom metric. Units help
-- provide conceptual meaning to your data. Metric data points that specify
-- a unit of measure, such as Percent, are aggregated separately.
--
-- If you don\'t specify @Unit@, CloudWatch retrieves all unit types that
-- have been published for the metric and attempts to evaluate the alarm.
-- Usually, metrics are published with only one unit, so the alarm works as
-- intended.
--
-- However, if the metric is published with multiple types of units and you
-- don\'t specify a unit, the alarm\'s behavior is not defined and it
-- behaves predictably.
--
-- We recommend omitting @Unit@ so that you don\'t inadvertently specify an
-- incorrect unit that is not published for this metric. Doing so causes
-- the alarm to be stuck in the @INSUFFICIENT DATA@ state.
--
-- 'thresholdMetricId', 'putMetricAlarm_thresholdMetricId' - If this is an alarm based on an anomaly detection model, make this value
-- match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection
-- Model Alarm__ example on this page.
--
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
--
-- 'metricName', 'putMetricAlarm_metricName' - The name for the metric associated with the alarm. For each
-- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
-- @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot
-- specify this parameter, or any of the @Dimensions@, @Period@,
-- @Namespace@, @Statistic@, or @ExtendedStatistic@ parameters. Instead,
-- you specify all this information in the @Metrics@ array.
--
-- 'insufficientDataActions', 'putMetricAlarm_insufficientDataActions' - The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:region:ec2:stop@ |
-- @arn:aws:automate:region:ec2:terminate@ |
-- @arn:aws:automate:region:ec2:recover@ |
-- @arn:aws:automate:region:ec2:reboot@ |
-- @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
--
-- Valid Values (for use with IAM roles):
-- @>arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
--
-- 'treatMissingData', 'putMetricAlarm_treatMissingData' - Sets how this alarm is to handle missing data points. If
-- @TreatMissingData@ is omitted, the default behavior of @missing@ is
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data>.
--
-- Valid Values: @breaching | notBreaching | ignore | missing@
--
-- 'metrics', 'putMetricAlarm_metrics' - An array of @MetricDataQuery@ structures that enable you to create an
-- alarm based on the result of a metric math expression. For each
-- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
-- @Metrics@ array.
--
-- Each item in the @Metrics@ array either retrieves a metric or performs a
-- math expression.
--
-- One item in the @Metrics@ array is the expression that the alarm
-- watches. You designate this expression by setting @ReturnData@ to true
-- for this object in the array. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery>.
--
-- If you use the @Metrics@ parameter, you cannot include the @MetricName@,
-- @Dimensions@, @Period@, @Namespace@, @Statistic@, or @ExtendedStatistic@
-- parameters of @PutMetricAlarm@ in the same operation. Instead, you
-- retrieve the metrics you are using in your math expression as part of
-- the @Metrics@ array.
--
-- 'tags', 'putMetricAlarm_tags' - A list of key-value pairs to associate with the alarm. You can associate
-- as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- If you are using this operation to update an existing alarm, any tags
-- you specify in this parameter are ignored. To change the tags of an
-- existing alarm, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_UntagResource.html UntagResource>.
--
-- 'oKActions', 'putMetricAlarm_oKActions' - The actions to execute when this alarm transitions to an @OK@ state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
--
-- Valid Values: @arn:aws:automate:region:ec2:stop@ |
-- @arn:aws:automate:region:ec2:terminate@ |
-- @arn:aws:automate:region:ec2:recover@ |
-- @arn:aws:automate:region:ec2:reboot@ |
-- @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
--
-- Valid Values (for use with IAM roles):
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
--
-- 'statistic', 'putMetricAlarm_statistic' - The statistic for the metric specified in @MetricName@, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@. When you
-- call @PutMetricAlarm@ and specify a @MetricName@, you must specify
-- either @Statistic@ or @ExtendedStatistic,@ but not both.
--
-- 'dimensions', 'putMetricAlarm_dimensions' - The dimensions for the metric specified in @MetricName@.
--
-- 'namespace', 'putMetricAlarm_namespace' - The namespace for the metric associated specified in @MetricName@.
--
-- 'actionsEnabled', 'putMetricAlarm_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state. The default is @TRUE@.
--
-- 'alarmDescription', 'putMetricAlarm_alarmDescription' - The description for the alarm.
--
-- 'period', 'putMetricAlarm_period' - The length, in seconds, used each time the metric specified in
-- @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of
-- 60.
--
-- @Period@ is required for alarms based on static thresholds. If you are
-- creating an alarm based on a metric math expression, you specify the
-- period for each metric within the objects in the @Metrics@ array.
--
-- Be sure to specify 10 or 30 only for metrics that are stored by a
-- @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a
-- period of 10 or 30 for a metric that does not have sub-minute
-- resolution, the alarm still attempts to gather data at the period rate
-- that you specify. In this case, it does not receive data for the
-- attempts that do not correspond to a one-minute data resolution, and the
-- alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or
-- 30 also sets this alarm as a high-resolution alarm, which has a higher
-- charge than other alarms. For more information about pricing, see
-- <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
--
-- An alarm\'s total current evaluation period can be no longer than one
-- day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than
-- 86,400 seconds.
--
-- 'alarmName', 'putMetricAlarm_alarmName' - The name for the alarm. This name must be unique within the Region.
--
-- 'evaluationPeriods', 'putMetricAlarm_evaluationPeriods' - The number of periods over which data is compared to the specified
-- threshold. If you are setting an alarm that requires that a number of
-- consecutive data points be breaching to trigger the alarm, this value
-- specifies that number. If you are setting an \"M out of N\" alarm, this
-- value is the N.
--
-- An alarm\'s total current evaluation period can be no longer than one
-- day, so this number multiplied by @Period@ cannot be more than 86,400
-- seconds.
--
-- 'comparisonOperator', 'putMetricAlarm_comparisonOperator' - The arithmetic operation to use when comparing the specified statistic
-- and threshold. The specified statistic value is used as the first
-- operand.
--
-- The values @LessThanLowerOrGreaterThanUpperThreshold@,
-- @LessThanLowerThreshold@, and @GreaterThanUpperThreshold@ are used only
-- for alarms based on anomaly detection models.
newPutMetricAlarm ::
  -- | 'alarmName'
  Core.Text ->
  -- | 'evaluationPeriods'
  Core.Natural ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  PutMetricAlarm
newPutMetricAlarm
  pAlarmName_
  pEvaluationPeriods_
  pComparisonOperator_ =
    PutMetricAlarm'
      { threshold = Core.Nothing,
        datapointsToAlarm = Core.Nothing,
        evaluateLowSampleCountPercentile = Core.Nothing,
        extendedStatistic = Core.Nothing,
        alarmActions = Core.Nothing,
        unit = Core.Nothing,
        thresholdMetricId = Core.Nothing,
        metricName = Core.Nothing,
        insufficientDataActions = Core.Nothing,
        treatMissingData = Core.Nothing,
        metrics = Core.Nothing,
        tags = Core.Nothing,
        oKActions = Core.Nothing,
        statistic = Core.Nothing,
        dimensions = Core.Nothing,
        namespace = Core.Nothing,
        actionsEnabled = Core.Nothing,
        alarmDescription = Core.Nothing,
        period = Core.Nothing,
        alarmName = pAlarmName_,
        evaluationPeriods = pEvaluationPeriods_,
        comparisonOperator = pComparisonOperator_
      }

-- | The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but
-- should not be used for alarms based on anomaly detection models.
putMetricAlarm_threshold :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Double)
putMetricAlarm_threshold = Lens.lens (\PutMetricAlarm' {threshold} -> threshold) (\s@PutMetricAlarm' {} a -> s {threshold = a} :: PutMetricAlarm)

-- | The number of data points that must be breaching to trigger the alarm.
-- This is used only if you are setting an \"M out of N\" alarm. In that
-- case, this value is the M. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm>
-- in the /Amazon CloudWatch User Guide/.
putMetricAlarm_datapointsToAlarm :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Natural)
putMetricAlarm_datapointsToAlarm = Lens.lens (\PutMetricAlarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@PutMetricAlarm' {} a -> s {datapointsToAlarm = a} :: PutMetricAlarm)

-- | Used only for alarms based on percentiles. If you specify @ignore@, the
-- alarm state does not change during periods with too few data points to
-- be statistically significant. If you specify @evaluate@ or omit this
-- parameter, the alarm is always evaluated and possibly changes state no
-- matter how many data points are available. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples>.
--
-- Valid Values: @evaluate | ignore@
putMetricAlarm_evaluateLowSampleCountPercentile :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_evaluateLowSampleCountPercentile = Lens.lens (\PutMetricAlarm' {evaluateLowSampleCountPercentile} -> evaluateLowSampleCountPercentile) (\s@PutMetricAlarm' {} a -> s {evaluateLowSampleCountPercentile = a} :: PutMetricAlarm)

-- | The percentile statistic for the metric specified in @MetricName@.
-- Specify a value between p0.0 and p100. When you call @PutMetricAlarm@
-- and specify a @MetricName@, you must specify either @Statistic@ or
-- @ExtendedStatistic,@ but not both.
putMetricAlarm_extendedStatistic :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_extendedStatistic = Lens.lens (\PutMetricAlarm' {extendedStatistic} -> extendedStatistic) (\s@PutMetricAlarm' {} a -> s {extendedStatistic = a} :: PutMetricAlarm)

-- | The actions to execute when this alarm transitions to the @ALARM@ state
-- from any other state. Each action is specified as an Amazon Resource
-- Name (ARN).
--
-- Valid Values: @arn:aws:automate:region:ec2:stop@ |
-- @arn:aws:automate:region:ec2:terminate@ |
-- @arn:aws:automate:region:ec2:recover@ |
-- @arn:aws:automate:region:ec2:reboot@ |
-- @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
-- | @arn:aws:ssm:region:account-id:opsitem:severity @
--
-- Valid Values (for use with IAM roles):
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
putMetricAlarm_alarmActions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Core.Text])
putMetricAlarm_alarmActions = Lens.lens (\PutMetricAlarm' {alarmActions} -> alarmActions) (\s@PutMetricAlarm' {} a -> s {alarmActions = a} :: PutMetricAlarm) Core.. Lens.mapping Lens._Coerce

-- | The unit of measure for the statistic. For example, the units for the
-- Amazon EC2 NetworkIn metric are Bytes because NetworkIn tracks the
-- number of bytes that an instance receives on all network interfaces. You
-- can also specify a unit when you create a custom metric. Units help
-- provide conceptual meaning to your data. Metric data points that specify
-- a unit of measure, such as Percent, are aggregated separately.
--
-- If you don\'t specify @Unit@, CloudWatch retrieves all unit types that
-- have been published for the metric and attempts to evaluate the alarm.
-- Usually, metrics are published with only one unit, so the alarm works as
-- intended.
--
-- However, if the metric is published with multiple types of units and you
-- don\'t specify a unit, the alarm\'s behavior is not defined and it
-- behaves predictably.
--
-- We recommend omitting @Unit@ so that you don\'t inadvertently specify an
-- incorrect unit that is not published for this metric. Doing so causes
-- the alarm to be stuck in the @INSUFFICIENT DATA@ state.
putMetricAlarm_unit :: Lens.Lens' PutMetricAlarm (Core.Maybe StandardUnit)
putMetricAlarm_unit = Lens.lens (\PutMetricAlarm' {unit} -> unit) (\s@PutMetricAlarm' {} a -> s {unit = a} :: PutMetricAlarm)

-- | If this is an alarm based on an anomaly detection model, make this value
-- match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection
-- Model Alarm__ example on this page.
--
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
putMetricAlarm_thresholdMetricId :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_thresholdMetricId = Lens.lens (\PutMetricAlarm' {thresholdMetricId} -> thresholdMetricId) (\s@PutMetricAlarm' {} a -> s {thresholdMetricId = a} :: PutMetricAlarm)

-- | The name for the metric associated with the alarm. For each
-- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
-- @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot
-- specify this parameter, or any of the @Dimensions@, @Period@,
-- @Namespace@, @Statistic@, or @ExtendedStatistic@ parameters. Instead,
-- you specify all this information in the @Metrics@ array.
putMetricAlarm_metricName :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_metricName = Lens.lens (\PutMetricAlarm' {metricName} -> metricName) (\s@PutMetricAlarm' {} a -> s {metricName = a} :: PutMetricAlarm)

-- | The actions to execute when this alarm transitions to the
-- @INSUFFICIENT_DATA@ state from any other state. Each action is specified
-- as an Amazon Resource Name (ARN).
--
-- Valid Values: @arn:aws:automate:region:ec2:stop@ |
-- @arn:aws:automate:region:ec2:terminate@ |
-- @arn:aws:automate:region:ec2:recover@ |
-- @arn:aws:automate:region:ec2:reboot@ |
-- @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
--
-- Valid Values (for use with IAM roles):
-- @>arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
putMetricAlarm_insufficientDataActions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Core.Text])
putMetricAlarm_insufficientDataActions = Lens.lens (\PutMetricAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@PutMetricAlarm' {} a -> s {insufficientDataActions = a} :: PutMetricAlarm) Core.. Lens.mapping Lens._Coerce

-- | Sets how this alarm is to handle missing data points. If
-- @TreatMissingData@ is omitted, the default behavior of @missing@ is
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data>.
--
-- Valid Values: @breaching | notBreaching | ignore | missing@
putMetricAlarm_treatMissingData :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_treatMissingData = Lens.lens (\PutMetricAlarm' {treatMissingData} -> treatMissingData) (\s@PutMetricAlarm' {} a -> s {treatMissingData = a} :: PutMetricAlarm)

-- | An array of @MetricDataQuery@ structures that enable you to create an
-- alarm based on the result of a metric math expression. For each
-- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
-- @Metrics@ array.
--
-- Each item in the @Metrics@ array either retrieves a metric or performs a
-- math expression.
--
-- One item in the @Metrics@ array is the expression that the alarm
-- watches. You designate this expression by setting @ReturnData@ to true
-- for this object in the array. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDataQuery.html MetricDataQuery>.
--
-- If you use the @Metrics@ parameter, you cannot include the @MetricName@,
-- @Dimensions@, @Period@, @Namespace@, @Statistic@, or @ExtendedStatistic@
-- parameters of @PutMetricAlarm@ in the same operation. Instead, you
-- retrieve the metrics you are using in your math expression as part of
-- the @Metrics@ array.
putMetricAlarm_metrics :: Lens.Lens' PutMetricAlarm (Core.Maybe [MetricDataQuery])
putMetricAlarm_metrics = Lens.lens (\PutMetricAlarm' {metrics} -> metrics) (\s@PutMetricAlarm' {} a -> s {metrics = a} :: PutMetricAlarm) Core.. Lens.mapping Lens._Coerce

-- | A list of key-value pairs to associate with the alarm. You can associate
-- as many as 50 tags with an alarm.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- If you are using this operation to update an existing alarm, any tags
-- you specify in this parameter are ignored. To change the tags of an
-- existing alarm, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_UntagResource.html UntagResource>.
putMetricAlarm_tags :: Lens.Lens' PutMetricAlarm (Core.Maybe [Tag])
putMetricAlarm_tags = Lens.lens (\PutMetricAlarm' {tags} -> tags) (\s@PutMetricAlarm' {} a -> s {tags = a} :: PutMetricAlarm) Core.. Lens.mapping Lens._Coerce

-- | The actions to execute when this alarm transitions to an @OK@ state from
-- any other state. Each action is specified as an Amazon Resource Name
-- (ARN).
--
-- Valid Values: @arn:aws:automate:region:ec2:stop@ |
-- @arn:aws:automate:region:ec2:terminate@ |
-- @arn:aws:automate:region:ec2:recover@ |
-- @arn:aws:automate:region:ec2:reboot@ |
-- @arn:aws:sns:region:account-id:sns-topic-name @ |
-- @arn:aws:autoscaling:region:account-id:scalingPolicy:policy-id:autoScalingGroupName\/group-friendly-name:policyName\/policy-friendly-name @
--
-- Valid Values (for use with IAM roles):
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
putMetricAlarm_oKActions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Core.Text])
putMetricAlarm_oKActions = Lens.lens (\PutMetricAlarm' {oKActions} -> oKActions) (\s@PutMetricAlarm' {} a -> s {oKActions = a} :: PutMetricAlarm) Core.. Lens.mapping Lens._Coerce

-- | The statistic for the metric specified in @MetricName@, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@. When you
-- call @PutMetricAlarm@ and specify a @MetricName@, you must specify
-- either @Statistic@ or @ExtendedStatistic,@ but not both.
putMetricAlarm_statistic :: Lens.Lens' PutMetricAlarm (Core.Maybe Statistic)
putMetricAlarm_statistic = Lens.lens (\PutMetricAlarm' {statistic} -> statistic) (\s@PutMetricAlarm' {} a -> s {statistic = a} :: PutMetricAlarm)

-- | The dimensions for the metric specified in @MetricName@.
putMetricAlarm_dimensions :: Lens.Lens' PutMetricAlarm (Core.Maybe [Dimension])
putMetricAlarm_dimensions = Lens.lens (\PutMetricAlarm' {dimensions} -> dimensions) (\s@PutMetricAlarm' {} a -> s {dimensions = a} :: PutMetricAlarm) Core.. Lens.mapping Lens._Coerce

-- | The namespace for the metric associated specified in @MetricName@.
putMetricAlarm_namespace :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_namespace = Lens.lens (\PutMetricAlarm' {namespace} -> namespace) (\s@PutMetricAlarm' {} a -> s {namespace = a} :: PutMetricAlarm)

-- | Indicates whether actions should be executed during any changes to the
-- alarm state. The default is @TRUE@.
putMetricAlarm_actionsEnabled :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Bool)
putMetricAlarm_actionsEnabled = Lens.lens (\PutMetricAlarm' {actionsEnabled} -> actionsEnabled) (\s@PutMetricAlarm' {} a -> s {actionsEnabled = a} :: PutMetricAlarm)

-- | The description for the alarm.
putMetricAlarm_alarmDescription :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Text)
putMetricAlarm_alarmDescription = Lens.lens (\PutMetricAlarm' {alarmDescription} -> alarmDescription) (\s@PutMetricAlarm' {} a -> s {alarmDescription = a} :: PutMetricAlarm)

-- | The length, in seconds, used each time the metric specified in
-- @MetricName@ is evaluated. Valid values are 10, 30, and any multiple of
-- 60.
--
-- @Period@ is required for alarms based on static thresholds. If you are
-- creating an alarm based on a metric math expression, you specify the
-- period for each metric within the objects in the @Metrics@ array.
--
-- Be sure to specify 10 or 30 only for metrics that are stored by a
-- @PutMetricData@ call with a @StorageResolution@ of 1. If you specify a
-- period of 10 or 30 for a metric that does not have sub-minute
-- resolution, the alarm still attempts to gather data at the period rate
-- that you specify. In this case, it does not receive data for the
-- attempts that do not correspond to a one-minute data resolution, and the
-- alarm might often lapse into INSUFFICENT_DATA status. Specifying 10 or
-- 30 also sets this alarm as a high-resolution alarm, which has a higher
-- charge than other alarms. For more information about pricing, see
-- <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing>.
--
-- An alarm\'s total current evaluation period can be no longer than one
-- day, so @Period@ multiplied by @EvaluationPeriods@ cannot be more than
-- 86,400 seconds.
putMetricAlarm_period :: Lens.Lens' PutMetricAlarm (Core.Maybe Core.Natural)
putMetricAlarm_period = Lens.lens (\PutMetricAlarm' {period} -> period) (\s@PutMetricAlarm' {} a -> s {period = a} :: PutMetricAlarm)

-- | The name for the alarm. This name must be unique within the Region.
putMetricAlarm_alarmName :: Lens.Lens' PutMetricAlarm Core.Text
putMetricAlarm_alarmName = Lens.lens (\PutMetricAlarm' {alarmName} -> alarmName) (\s@PutMetricAlarm' {} a -> s {alarmName = a} :: PutMetricAlarm)

-- | The number of periods over which data is compared to the specified
-- threshold. If you are setting an alarm that requires that a number of
-- consecutive data points be breaching to trigger the alarm, this value
-- specifies that number. If you are setting an \"M out of N\" alarm, this
-- value is the N.
--
-- An alarm\'s total current evaluation period can be no longer than one
-- day, so this number multiplied by @Period@ cannot be more than 86,400
-- seconds.
putMetricAlarm_evaluationPeriods :: Lens.Lens' PutMetricAlarm Core.Natural
putMetricAlarm_evaluationPeriods = Lens.lens (\PutMetricAlarm' {evaluationPeriods} -> evaluationPeriods) (\s@PutMetricAlarm' {} a -> s {evaluationPeriods = a} :: PutMetricAlarm)

-- | The arithmetic operation to use when comparing the specified statistic
-- and threshold. The specified statistic value is used as the first
-- operand.
--
-- The values @LessThanLowerOrGreaterThanUpperThreshold@,
-- @LessThanLowerThreshold@, and @GreaterThanUpperThreshold@ are used only
-- for alarms based on anomaly detection models.
putMetricAlarm_comparisonOperator :: Lens.Lens' PutMetricAlarm ComparisonOperator
putMetricAlarm_comparisonOperator = Lens.lens (\PutMetricAlarm' {comparisonOperator} -> comparisonOperator) (\s@PutMetricAlarm' {} a -> s {comparisonOperator = a} :: PutMetricAlarm)

instance Core.AWSRequest PutMetricAlarm where
  type
    AWSResponse PutMetricAlarm =
      PutMetricAlarmResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull PutMetricAlarmResponse'

instance Core.Hashable PutMetricAlarm

instance Core.NFData PutMetricAlarm

instance Core.ToHeaders PutMetricAlarm where
  toHeaders = Core.const Core.mempty

instance Core.ToPath PutMetricAlarm where
  toPath = Core.const "/"

instance Core.ToQuery PutMetricAlarm where
  toQuery PutMetricAlarm' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("PutMetricAlarm" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "Threshold" Core.=: threshold,
        "DatapointsToAlarm" Core.=: datapointsToAlarm,
        "EvaluateLowSampleCountPercentile"
          Core.=: evaluateLowSampleCountPercentile,
        "ExtendedStatistic" Core.=: extendedStatistic,
        "AlarmActions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> alarmActions),
        "Unit" Core.=: unit,
        "ThresholdMetricId" Core.=: thresholdMetricId,
        "MetricName" Core.=: metricName,
        "InsufficientDataActions"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> insufficientDataActions
            ),
        "TreatMissingData" Core.=: treatMissingData,
        "Metrics"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> metrics),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tags),
        "OKActions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> oKActions),
        "Statistic" Core.=: statistic,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> dimensions),
        "Namespace" Core.=: namespace,
        "ActionsEnabled" Core.=: actionsEnabled,
        "AlarmDescription" Core.=: alarmDescription,
        "Period" Core.=: period,
        "AlarmName" Core.=: alarmName,
        "EvaluationPeriods" Core.=: evaluationPeriods,
        "ComparisonOperator" Core.=: comparisonOperator
      ]

-- | /See:/ 'newPutMetricAlarmResponse' smart constructor.
data PutMetricAlarmResponse = PutMetricAlarmResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutMetricAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetricAlarmResponse ::
  PutMetricAlarmResponse
newPutMetricAlarmResponse = PutMetricAlarmResponse'

instance Core.NFData PutMetricAlarmResponse
