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
-- Module      : Amazonka.CloudWatch.PutMetricAlarm
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- The first time you create an alarm in the Amazon Web Services Management
-- Console, the CLI, or by using the PutMetricAlarm API, CloudWatch creates
-- the necessary service-linked role for you. The service-linked roles are
-- called @AWSServiceRoleForCloudWatchEvents@ and
-- @AWSServiceRoleForCloudWatchAlarms_ActionSSM@. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role Amazon Web Services service-linked role>.
--
-- __Cross-account alarms__
--
-- You can set an alarm on metrics in the current account, or in another
-- account. To create a cross-account alarm that watches a metric in a
-- different account, you must have completed the following pre-requisites:
--
-- -   The account where the metrics are located (the /sharing account/)
--     must already have a sharing role named
--     __CloudWatch-CrossAccountSharingRole__. If it does not already have
--     this role, you must create it using the instructions in __Set up a
--     sharing account__ in
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Cross-Account-Cross-Region.html#enable-cross-account-cross-Region Cross-account cross-Region CloudWatch console>.
--     The policy for that role must grant access to the ID of the account
--     where you are creating the alarm.
--
-- -   The account where you are creating the alarm (the /monitoring
--     account/) must already have a service-linked role named
--     __AWSServiceRoleForCloudWatchCrossAccount__ to allow CloudWatch to
--     assume the sharing role in the sharing account. If it does not, you
--     must create it following the directions in __Set up a monitoring
--     account__ in
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/Cross-Account-Cross-Region.html#enable-cross-account-cross-Region Cross-account cross-Region CloudWatch console>.
module Amazonka.CloudWatch.PutMetricAlarm
  ( -- * Creating a Request
    PutMetricAlarm (..),
    newPutMetricAlarm,

    -- * Request Lenses
    putMetricAlarm_tags,
    putMetricAlarm_alarmActions,
    putMetricAlarm_alarmDescription,
    putMetricAlarm_extendedStatistic,
    putMetricAlarm_actionsEnabled,
    putMetricAlarm_period,
    putMetricAlarm_evaluateLowSampleCountPercentile,
    putMetricAlarm_dimensions,
    putMetricAlarm_thresholdMetricId,
    putMetricAlarm_treatMissingData,
    putMetricAlarm_metrics,
    putMetricAlarm_datapointsToAlarm,
    putMetricAlarm_insufficientDataActions,
    putMetricAlarm_metricName,
    putMetricAlarm_threshold,
    putMetricAlarm_oKActions,
    putMetricAlarm_namespace,
    putMetricAlarm_statistic,
    putMetricAlarm_unit,
    putMetricAlarm_alarmName,
    putMetricAlarm_evaluationPeriods,
    putMetricAlarm_comparisonOperator,

    -- * Destructuring the Response
    PutMetricAlarmResponse (..),
    newPutMetricAlarmResponse,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMetricAlarm' smart constructor.
data PutMetricAlarm = PutMetricAlarm'
  { -- | A list of key-value pairs to associate with the alarm. You can associate
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
    tags :: Prelude.Maybe [Tag],
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
    -- | @arn:aws:ssm:region:account-id:opsitem:severity @ |
    -- @arn:aws:ssm-incidents::account-id:response-plan:response-plan-name @
    --
    -- Valid Values (for use with IAM roles):
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Recover\/1.0@
    alarmActions :: Prelude.Maybe [Prelude.Text],
    -- | The description for the alarm.
    alarmDescription :: Prelude.Maybe Prelude.Text,
    -- | The percentile statistic for the metric specified in @MetricName@.
    -- Specify a value between p0.0 and p100. When you call @PutMetricAlarm@
    -- and specify a @MetricName@, you must specify either @Statistic@ or
    -- @ExtendedStatistic,@ but not both.
    extendedStatistic :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether actions should be executed during any changes to the
    -- alarm state. The default is @TRUE@.
    actionsEnabled :: Prelude.Maybe Prelude.Bool,
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
    period :: Prelude.Maybe Prelude.Natural,
    -- | Used only for alarms based on percentiles. If you specify @ignore@, the
    -- alarm state does not change during periods with too few data points to
    -- be statistically significant. If you specify @evaluate@ or omit this
    -- parameter, the alarm is always evaluated and possibly changes state no
    -- matter how many data points are available. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples>.
    --
    -- Valid Values: @evaluate | ignore@
    evaluateLowSampleCountPercentile :: Prelude.Maybe Prelude.Text,
    -- | The dimensions for the metric specified in @MetricName@.
    dimensions :: Prelude.Maybe [Dimension],
    -- | If this is an alarm based on an anomaly detection model, make this value
    -- match the ID of the @ANOMALY_DETECTION_BAND@ function.
    --
    -- For an example of how to use this parameter, see the __Anomaly Detection
    -- Model Alarm__ example on this page.
    --
    -- If your alarm uses this parameter, it cannot have Auto Scaling actions.
    thresholdMetricId :: Prelude.Maybe Prelude.Text,
    -- | Sets how this alarm is to handle missing data points. If
    -- @TreatMissingData@ is omitted, the default behavior of @missing@ is
    -- used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data>.
    --
    -- Valid Values: @breaching | notBreaching | ignore | missing@
    --
    -- Alarms that evaluate metrics in the @AWS\/DynamoDB@ namespace always
    -- @ignore@ missing data even if you choose a different option for
    -- @TreatMissingData@. When an @AWS\/DynamoDB@ metric has missing data,
    -- alarms that evaluate that metric remain in their current state.
    treatMissingData :: Prelude.Maybe Prelude.Text,
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
    metrics :: Prelude.Maybe [MetricDataQuery],
    -- | The number of data points that must be breaching to trigger the alarm.
    -- This is used only if you are setting an \"M out of N\" alarm. In that
    -- case, this value is the M. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm>
    -- in the /Amazon CloudWatch User Guide/.
    datapointsToAlarm :: Prelude.Maybe Prelude.Natural,
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
    insufficientDataActions :: Prelude.Maybe [Prelude.Text],
    -- | The name for the metric associated with the alarm. For each
    -- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
    -- @Metrics@ array.
    --
    -- If you are creating an alarm based on a math expression, you cannot
    -- specify this parameter, or any of the @Dimensions@, @Period@,
    -- @Namespace@, @Statistic@, or @ExtendedStatistic@ parameters. Instead,
    -- you specify all this information in the @Metrics@ array.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The value against which the specified statistic is compared.
    --
    -- This parameter is required for alarms based on static thresholds, but
    -- should not be used for alarms based on anomaly detection models.
    threshold :: Prelude.Maybe Prelude.Double,
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
    -- |
    -- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Recover\/1.0@
    oKActions :: Prelude.Maybe [Prelude.Text],
    -- | The namespace for the metric associated specified in @MetricName@.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The statistic for the metric specified in @MetricName@, other than
    -- percentile. For percentile statistics, use @ExtendedStatistic@. When you
    -- call @PutMetricAlarm@ and specify a @MetricName@, you must specify
    -- either @Statistic@ or @ExtendedStatistic,@ but not both.
    statistic :: Prelude.Maybe Statistic,
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
    -- behaves unpredictably.
    --
    -- We recommend omitting @Unit@ so that you don\'t inadvertently specify an
    -- incorrect unit that is not published for this metric. Doing so causes
    -- the alarm to be stuck in the @INSUFFICIENT DATA@ state.
    unit :: Prelude.Maybe StandardUnit,
    -- | The name for the alarm. This name must be unique within the Region.
    alarmName :: Prelude.Text,
    -- | The number of periods over which data is compared to the specified
    -- threshold. If you are setting an alarm that requires that a number of
    -- consecutive data points be breaching to trigger the alarm, this value
    -- specifies that number. If you are setting an \"M out of N\" alarm, this
    -- value is the N.
    --
    -- An alarm\'s total current evaluation period can be no longer than one
    -- day, so this number multiplied by @Period@ cannot be more than 86,400
    -- seconds.
    evaluationPeriods :: Prelude.Natural,
    -- | The arithmetic operation to use when comparing the specified statistic
    -- and threshold. The specified statistic value is used as the first
    -- operand.
    --
    -- The values @LessThanLowerOrGreaterThanUpperThreshold@,
    -- @LessThanLowerThreshold@, and @GreaterThanUpperThreshold@ are used only
    -- for alarms based on anomaly detection models.
    comparisonOperator :: ComparisonOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- | @arn:aws:ssm:region:account-id:opsitem:severity @ |
-- @arn:aws:ssm-incidents::account-id:response-plan:response-plan-name @
--
-- Valid Values (for use with IAM roles):
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Recover\/1.0@
--
-- 'alarmDescription', 'putMetricAlarm_alarmDescription' - The description for the alarm.
--
-- 'extendedStatistic', 'putMetricAlarm_extendedStatistic' - The percentile statistic for the metric specified in @MetricName@.
-- Specify a value between p0.0 and p100. When you call @PutMetricAlarm@
-- and specify a @MetricName@, you must specify either @Statistic@ or
-- @ExtendedStatistic,@ but not both.
--
-- 'actionsEnabled', 'putMetricAlarm_actionsEnabled' - Indicates whether actions should be executed during any changes to the
-- alarm state. The default is @TRUE@.
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
-- 'evaluateLowSampleCountPercentile', 'putMetricAlarm_evaluateLowSampleCountPercentile' - Used only for alarms based on percentiles. If you specify @ignore@, the
-- alarm state does not change during periods with too few data points to
-- be statistically significant. If you specify @evaluate@ or omit this
-- parameter, the alarm is always evaluated and possibly changes state no
-- matter how many data points are available. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples>.
--
-- Valid Values: @evaluate | ignore@
--
-- 'dimensions', 'putMetricAlarm_dimensions' - The dimensions for the metric specified in @MetricName@.
--
-- 'thresholdMetricId', 'putMetricAlarm_thresholdMetricId' - If this is an alarm based on an anomaly detection model, make this value
-- match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection
-- Model Alarm__ example on this page.
--
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
--
-- 'treatMissingData', 'putMetricAlarm_treatMissingData' - Sets how this alarm is to handle missing data points. If
-- @TreatMissingData@ is omitted, the default behavior of @missing@ is
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data>.
--
-- Valid Values: @breaching | notBreaching | ignore | missing@
--
-- Alarms that evaluate metrics in the @AWS\/DynamoDB@ namespace always
-- @ignore@ missing data even if you choose a different option for
-- @TreatMissingData@. When an @AWS\/DynamoDB@ metric has missing data,
-- alarms that evaluate that metric remain in their current state.
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
-- 'datapointsToAlarm', 'putMetricAlarm_datapointsToAlarm' - The number of data points that must be breaching to trigger the alarm.
-- This is used only if you are setting an \"M out of N\" alarm. In that
-- case, this value is the M. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm>
-- in the /Amazon CloudWatch User Guide/.
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
-- 'metricName', 'putMetricAlarm_metricName' - The name for the metric associated with the alarm. For each
-- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
-- @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot
-- specify this parameter, or any of the @Dimensions@, @Period@,
-- @Namespace@, @Statistic@, or @ExtendedStatistic@ parameters. Instead,
-- you specify all this information in the @Metrics@ array.
--
-- 'threshold', 'putMetricAlarm_threshold' - The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but
-- should not be used for alarms based on anomaly detection models.
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
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Recover\/1.0@
--
-- 'namespace', 'putMetricAlarm_namespace' - The namespace for the metric associated specified in @MetricName@.
--
-- 'statistic', 'putMetricAlarm_statistic' - The statistic for the metric specified in @MetricName@, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@. When you
-- call @PutMetricAlarm@ and specify a @MetricName@, you must specify
-- either @Statistic@ or @ExtendedStatistic,@ but not both.
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
-- behaves unpredictably.
--
-- We recommend omitting @Unit@ so that you don\'t inadvertently specify an
-- incorrect unit that is not published for this metric. Doing so causes
-- the alarm to be stuck in the @INSUFFICIENT DATA@ state.
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
  Prelude.Text ->
  -- | 'evaluationPeriods'
  Prelude.Natural ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  PutMetricAlarm
newPutMetricAlarm
  pAlarmName_
  pEvaluationPeriods_
  pComparisonOperator_ =
    PutMetricAlarm'
      { tags = Prelude.Nothing,
        alarmActions = Prelude.Nothing,
        alarmDescription = Prelude.Nothing,
        extendedStatistic = Prelude.Nothing,
        actionsEnabled = Prelude.Nothing,
        period = Prelude.Nothing,
        evaluateLowSampleCountPercentile = Prelude.Nothing,
        dimensions = Prelude.Nothing,
        thresholdMetricId = Prelude.Nothing,
        treatMissingData = Prelude.Nothing,
        metrics = Prelude.Nothing,
        datapointsToAlarm = Prelude.Nothing,
        insufficientDataActions = Prelude.Nothing,
        metricName = Prelude.Nothing,
        threshold = Prelude.Nothing,
        oKActions = Prelude.Nothing,
        namespace = Prelude.Nothing,
        statistic = Prelude.Nothing,
        unit = Prelude.Nothing,
        alarmName = pAlarmName_,
        evaluationPeriods = pEvaluationPeriods_,
        comparisonOperator = pComparisonOperator_
      }

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
putMetricAlarm_tags :: Lens.Lens' PutMetricAlarm (Prelude.Maybe [Tag])
putMetricAlarm_tags = Lens.lens (\PutMetricAlarm' {tags} -> tags) (\s@PutMetricAlarm' {} a -> s {tags = a} :: PutMetricAlarm) Prelude.. Lens.mapping Lens.coerced

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
-- | @arn:aws:ssm:region:account-id:opsitem:severity @ |
-- @arn:aws:ssm-incidents::account-id:response-plan:response-plan-name @
--
-- Valid Values (for use with IAM roles):
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Stop\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Terminate\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Reboot\/1.0@
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Recover\/1.0@
putMetricAlarm_alarmActions :: Lens.Lens' PutMetricAlarm (Prelude.Maybe [Prelude.Text])
putMetricAlarm_alarmActions = Lens.lens (\PutMetricAlarm' {alarmActions} -> alarmActions) (\s@PutMetricAlarm' {} a -> s {alarmActions = a} :: PutMetricAlarm) Prelude.. Lens.mapping Lens.coerced

-- | The description for the alarm.
putMetricAlarm_alarmDescription :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
putMetricAlarm_alarmDescription = Lens.lens (\PutMetricAlarm' {alarmDescription} -> alarmDescription) (\s@PutMetricAlarm' {} a -> s {alarmDescription = a} :: PutMetricAlarm)

-- | The percentile statistic for the metric specified in @MetricName@.
-- Specify a value between p0.0 and p100. When you call @PutMetricAlarm@
-- and specify a @MetricName@, you must specify either @Statistic@ or
-- @ExtendedStatistic,@ but not both.
putMetricAlarm_extendedStatistic :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
putMetricAlarm_extendedStatistic = Lens.lens (\PutMetricAlarm' {extendedStatistic} -> extendedStatistic) (\s@PutMetricAlarm' {} a -> s {extendedStatistic = a} :: PutMetricAlarm)

-- | Indicates whether actions should be executed during any changes to the
-- alarm state. The default is @TRUE@.
putMetricAlarm_actionsEnabled :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Bool)
putMetricAlarm_actionsEnabled = Lens.lens (\PutMetricAlarm' {actionsEnabled} -> actionsEnabled) (\s@PutMetricAlarm' {} a -> s {actionsEnabled = a} :: PutMetricAlarm)

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
putMetricAlarm_period :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Natural)
putMetricAlarm_period = Lens.lens (\PutMetricAlarm' {period} -> period) (\s@PutMetricAlarm' {} a -> s {period = a} :: PutMetricAlarm)

-- | Used only for alarms based on percentiles. If you specify @ignore@, the
-- alarm state does not change during periods with too few data points to
-- be statistically significant. If you specify @evaluate@ or omit this
-- parameter, the alarm is always evaluated and possibly changes state no
-- matter how many data points are available. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#percentiles-with-low-samples Percentile-Based CloudWatch Alarms and Low Data Samples>.
--
-- Valid Values: @evaluate | ignore@
putMetricAlarm_evaluateLowSampleCountPercentile :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
putMetricAlarm_evaluateLowSampleCountPercentile = Lens.lens (\PutMetricAlarm' {evaluateLowSampleCountPercentile} -> evaluateLowSampleCountPercentile) (\s@PutMetricAlarm' {} a -> s {evaluateLowSampleCountPercentile = a} :: PutMetricAlarm)

-- | The dimensions for the metric specified in @MetricName@.
putMetricAlarm_dimensions :: Lens.Lens' PutMetricAlarm (Prelude.Maybe [Dimension])
putMetricAlarm_dimensions = Lens.lens (\PutMetricAlarm' {dimensions} -> dimensions) (\s@PutMetricAlarm' {} a -> s {dimensions = a} :: PutMetricAlarm) Prelude.. Lens.mapping Lens.coerced

-- | If this is an alarm based on an anomaly detection model, make this value
-- match the ID of the @ANOMALY_DETECTION_BAND@ function.
--
-- For an example of how to use this parameter, see the __Anomaly Detection
-- Model Alarm__ example on this page.
--
-- If your alarm uses this parameter, it cannot have Auto Scaling actions.
putMetricAlarm_thresholdMetricId :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
putMetricAlarm_thresholdMetricId = Lens.lens (\PutMetricAlarm' {thresholdMetricId} -> thresholdMetricId) (\s@PutMetricAlarm' {} a -> s {thresholdMetricId = a} :: PutMetricAlarm)

-- | Sets how this alarm is to handle missing data points. If
-- @TreatMissingData@ is omitted, the default behavior of @missing@ is
-- used. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarms-and-missing-data Configuring How CloudWatch Alarms Treats Missing Data>.
--
-- Valid Values: @breaching | notBreaching | ignore | missing@
--
-- Alarms that evaluate metrics in the @AWS\/DynamoDB@ namespace always
-- @ignore@ missing data even if you choose a different option for
-- @TreatMissingData@. When an @AWS\/DynamoDB@ metric has missing data,
-- alarms that evaluate that metric remain in their current state.
putMetricAlarm_treatMissingData :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
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
putMetricAlarm_metrics :: Lens.Lens' PutMetricAlarm (Prelude.Maybe [MetricDataQuery])
putMetricAlarm_metrics = Lens.lens (\PutMetricAlarm' {metrics} -> metrics) (\s@PutMetricAlarm' {} a -> s {metrics = a} :: PutMetricAlarm) Prelude.. Lens.mapping Lens.coerced

-- | The number of data points that must be breaching to trigger the alarm.
-- This is used only if you are setting an \"M out of N\" alarm. In that
-- case, this value is the M. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html#alarm-evaluation Evaluating an Alarm>
-- in the /Amazon CloudWatch User Guide/.
putMetricAlarm_datapointsToAlarm :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Natural)
putMetricAlarm_datapointsToAlarm = Lens.lens (\PutMetricAlarm' {datapointsToAlarm} -> datapointsToAlarm) (\s@PutMetricAlarm' {} a -> s {datapointsToAlarm = a} :: PutMetricAlarm)

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
putMetricAlarm_insufficientDataActions :: Lens.Lens' PutMetricAlarm (Prelude.Maybe [Prelude.Text])
putMetricAlarm_insufficientDataActions = Lens.lens (\PutMetricAlarm' {insufficientDataActions} -> insufficientDataActions) (\s@PutMetricAlarm' {} a -> s {insufficientDataActions = a} :: PutMetricAlarm) Prelude.. Lens.mapping Lens.coerced

-- | The name for the metric associated with the alarm. For each
-- @PutMetricAlarm@ operation, you must specify either @MetricName@ or a
-- @Metrics@ array.
--
-- If you are creating an alarm based on a math expression, you cannot
-- specify this parameter, or any of the @Dimensions@, @Period@,
-- @Namespace@, @Statistic@, or @ExtendedStatistic@ parameters. Instead,
-- you specify all this information in the @Metrics@ array.
putMetricAlarm_metricName :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
putMetricAlarm_metricName = Lens.lens (\PutMetricAlarm' {metricName} -> metricName) (\s@PutMetricAlarm' {} a -> s {metricName = a} :: PutMetricAlarm)

-- | The value against which the specified statistic is compared.
--
-- This parameter is required for alarms based on static thresholds, but
-- should not be used for alarms based on anomaly detection models.
putMetricAlarm_threshold :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Double)
putMetricAlarm_threshold = Lens.lens (\PutMetricAlarm' {threshold} -> threshold) (\s@PutMetricAlarm' {} a -> s {threshold = a} :: PutMetricAlarm)

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
-- |
-- @arn:aws:swf:region:account-id:action\/actions\/AWS_EC2.InstanceId.Recover\/1.0@
putMetricAlarm_oKActions :: Lens.Lens' PutMetricAlarm (Prelude.Maybe [Prelude.Text])
putMetricAlarm_oKActions = Lens.lens (\PutMetricAlarm' {oKActions} -> oKActions) (\s@PutMetricAlarm' {} a -> s {oKActions = a} :: PutMetricAlarm) Prelude.. Lens.mapping Lens.coerced

-- | The namespace for the metric associated specified in @MetricName@.
putMetricAlarm_namespace :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Prelude.Text)
putMetricAlarm_namespace = Lens.lens (\PutMetricAlarm' {namespace} -> namespace) (\s@PutMetricAlarm' {} a -> s {namespace = a} :: PutMetricAlarm)

-- | The statistic for the metric specified in @MetricName@, other than
-- percentile. For percentile statistics, use @ExtendedStatistic@. When you
-- call @PutMetricAlarm@ and specify a @MetricName@, you must specify
-- either @Statistic@ or @ExtendedStatistic,@ but not both.
putMetricAlarm_statistic :: Lens.Lens' PutMetricAlarm (Prelude.Maybe Statistic)
putMetricAlarm_statistic = Lens.lens (\PutMetricAlarm' {statistic} -> statistic) (\s@PutMetricAlarm' {} a -> s {statistic = a} :: PutMetricAlarm)

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
-- behaves unpredictably.
--
-- We recommend omitting @Unit@ so that you don\'t inadvertently specify an
-- incorrect unit that is not published for this metric. Doing so causes
-- the alarm to be stuck in the @INSUFFICIENT DATA@ state.
putMetricAlarm_unit :: Lens.Lens' PutMetricAlarm (Prelude.Maybe StandardUnit)
putMetricAlarm_unit = Lens.lens (\PutMetricAlarm' {unit} -> unit) (\s@PutMetricAlarm' {} a -> s {unit = a} :: PutMetricAlarm)

-- | The name for the alarm. This name must be unique within the Region.
putMetricAlarm_alarmName :: Lens.Lens' PutMetricAlarm Prelude.Text
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
putMetricAlarm_evaluationPeriods :: Lens.Lens' PutMetricAlarm Prelude.Natural
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull PutMetricAlarmResponse'

instance Prelude.Hashable PutMetricAlarm where
  hashWithSalt _salt PutMetricAlarm' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` alarmActions
      `Prelude.hashWithSalt` alarmDescription
      `Prelude.hashWithSalt` extendedStatistic
      `Prelude.hashWithSalt` actionsEnabled
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` evaluateLowSampleCountPercentile
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` thresholdMetricId
      `Prelude.hashWithSalt` treatMissingData
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` datapointsToAlarm
      `Prelude.hashWithSalt` insufficientDataActions
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` oKActions
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` statistic
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` alarmName
      `Prelude.hashWithSalt` evaluationPeriods
      `Prelude.hashWithSalt` comparisonOperator

instance Prelude.NFData PutMetricAlarm where
  rnf PutMetricAlarm' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf alarmActions
      `Prelude.seq` Prelude.rnf alarmDescription
      `Prelude.seq` Prelude.rnf extendedStatistic
      `Prelude.seq` Prelude.rnf actionsEnabled
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf evaluateLowSampleCountPercentile
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf thresholdMetricId
      `Prelude.seq` Prelude.rnf treatMissingData
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf datapointsToAlarm
      `Prelude.seq` Prelude.rnf insufficientDataActions
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf oKActions
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf
        evaluationPeriods
      `Prelude.seq` Prelude.rnf
        comparisonOperator

instance Data.ToHeaders PutMetricAlarm where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutMetricAlarm where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMetricAlarm where
  toQuery PutMetricAlarm' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutMetricAlarm" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "AlarmActions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> alarmActions),
        "AlarmDescription" Data.=: alarmDescription,
        "ExtendedStatistic" Data.=: extendedStatistic,
        "ActionsEnabled" Data.=: actionsEnabled,
        "Period" Data.=: period,
        "EvaluateLowSampleCountPercentile"
          Data.=: evaluateLowSampleCountPercentile,
        "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "ThresholdMetricId" Data.=: thresholdMetricId,
        "TreatMissingData" Data.=: treatMissingData,
        "Metrics"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> metrics),
        "DatapointsToAlarm" Data.=: datapointsToAlarm,
        "InsufficientDataActions"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> insufficientDataActions
            ),
        "MetricName" Data.=: metricName,
        "Threshold" Data.=: threshold,
        "OKActions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> oKActions),
        "Namespace" Data.=: namespace,
        "Statistic" Data.=: statistic,
        "Unit" Data.=: unit,
        "AlarmName" Data.=: alarmName,
        "EvaluationPeriods" Data.=: evaluationPeriods,
        "ComparisonOperator" Data.=: comparisonOperator
      ]

-- | /See:/ 'newPutMetricAlarmResponse' smart constructor.
data PutMetricAlarmResponse = PutMetricAlarmResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetricAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetricAlarmResponse ::
  PutMetricAlarmResponse
newPutMetricAlarmResponse = PutMetricAlarmResponse'

instance Prelude.NFData PutMetricAlarmResponse where
  rnf _ = ()
