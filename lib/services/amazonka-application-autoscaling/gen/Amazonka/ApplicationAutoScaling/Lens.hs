{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationAutoScaling.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Lens
  ( -- * Operations

    -- ** DeleteScalingPolicy
    deleteScalingPolicy_policyName,
    deleteScalingPolicy_serviceNamespace,
    deleteScalingPolicy_resourceId,
    deleteScalingPolicy_scalableDimension,
    deleteScalingPolicyResponse_httpStatus,

    -- ** DeleteScheduledAction
    deleteScheduledAction_serviceNamespace,
    deleteScheduledAction_scheduledActionName,
    deleteScheduledAction_resourceId,
    deleteScheduledAction_scalableDimension,
    deleteScheduledActionResponse_httpStatus,

    -- ** DeregisterScalableTarget
    deregisterScalableTarget_serviceNamespace,
    deregisterScalableTarget_resourceId,
    deregisterScalableTarget_scalableDimension,
    deregisterScalableTargetResponse_httpStatus,

    -- ** DescribeScalableTargets
    describeScalableTargets_maxResults,
    describeScalableTargets_nextToken,
    describeScalableTargets_resourceIds,
    describeScalableTargets_scalableDimension,
    describeScalableTargets_serviceNamespace,
    describeScalableTargetsResponse_nextToken,
    describeScalableTargetsResponse_scalableTargets,
    describeScalableTargetsResponse_httpStatus,

    -- ** DescribeScalingActivities
    describeScalingActivities_includeNotScaledActivities,
    describeScalingActivities_maxResults,
    describeScalingActivities_nextToken,
    describeScalingActivities_resourceId,
    describeScalingActivities_scalableDimension,
    describeScalingActivities_serviceNamespace,
    describeScalingActivitiesResponse_nextToken,
    describeScalingActivitiesResponse_scalingActivities,
    describeScalingActivitiesResponse_httpStatus,

    -- ** DescribeScalingPolicies
    describeScalingPolicies_maxResults,
    describeScalingPolicies_nextToken,
    describeScalingPolicies_policyNames,
    describeScalingPolicies_resourceId,
    describeScalingPolicies_scalableDimension,
    describeScalingPolicies_serviceNamespace,
    describeScalingPoliciesResponse_nextToken,
    describeScalingPoliciesResponse_scalingPolicies,
    describeScalingPoliciesResponse_httpStatus,

    -- ** DescribeScheduledActions
    describeScheduledActions_maxResults,
    describeScheduledActions_nextToken,
    describeScheduledActions_resourceId,
    describeScheduledActions_scalableDimension,
    describeScheduledActions_scheduledActionNames,
    describeScheduledActions_serviceNamespace,
    describeScheduledActionsResponse_nextToken,
    describeScheduledActionsResponse_scheduledActions,
    describeScheduledActionsResponse_httpStatus,

    -- ** PutScalingPolicy
    putScalingPolicy_policyType,
    putScalingPolicy_stepScalingPolicyConfiguration,
    putScalingPolicy_targetTrackingScalingPolicyConfiguration,
    putScalingPolicy_policyName,
    putScalingPolicy_serviceNamespace,
    putScalingPolicy_resourceId,
    putScalingPolicy_scalableDimension,
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_httpStatus,
    putScalingPolicyResponse_policyARN,

    -- ** PutScheduledAction
    putScheduledAction_endTime,
    putScheduledAction_scalableTargetAction,
    putScheduledAction_schedule,
    putScheduledAction_startTime,
    putScheduledAction_timezone,
    putScheduledAction_serviceNamespace,
    putScheduledAction_scheduledActionName,
    putScheduledAction_resourceId,
    putScheduledAction_scalableDimension,
    putScheduledActionResponse_httpStatus,

    -- ** RegisterScalableTarget
    registerScalableTarget_maxCapacity,
    registerScalableTarget_minCapacity,
    registerScalableTarget_roleARN,
    registerScalableTarget_suspendedState,
    registerScalableTarget_serviceNamespace,
    registerScalableTarget_resourceId,
    registerScalableTarget_scalableDimension,
    registerScalableTargetResponse_httpStatus,

    -- * Types

    -- ** Alarm
    alarm_alarmName,
    alarm_alarmARN,

    -- ** CustomizedMetricSpecification
    customizedMetricSpecification_dimensions,
    customizedMetricSpecification_unit,
    customizedMetricSpecification_metricName,
    customizedMetricSpecification_namespace,
    customizedMetricSpecification_statistic,

    -- ** MetricDimension
    metricDimension_name,
    metricDimension_value,

    -- ** NotScaledReason
    notScaledReason_currentCapacity,
    notScaledReason_maxCapacity,
    notScaledReason_minCapacity,
    notScaledReason_code,

    -- ** PredefinedMetricSpecification
    predefinedMetricSpecification_resourceLabel,
    predefinedMetricSpecification_predefinedMetricType,

    -- ** ScalableTarget
    scalableTarget_suspendedState,
    scalableTarget_serviceNamespace,
    scalableTarget_resourceId,
    scalableTarget_scalableDimension,
    scalableTarget_minCapacity,
    scalableTarget_maxCapacity,
    scalableTarget_roleARN,
    scalableTarget_creationTime,

    -- ** ScalableTargetAction
    scalableTargetAction_maxCapacity,
    scalableTargetAction_minCapacity,

    -- ** ScalingActivity
    scalingActivity_details,
    scalingActivity_endTime,
    scalingActivity_notScaledReasons,
    scalingActivity_statusMessage,
    scalingActivity_activityId,
    scalingActivity_serviceNamespace,
    scalingActivity_resourceId,
    scalingActivity_scalableDimension,
    scalingActivity_description,
    scalingActivity_cause,
    scalingActivity_startTime,
    scalingActivity_statusCode,

    -- ** ScalingPolicy
    scalingPolicy_alarms,
    scalingPolicy_stepScalingPolicyConfiguration,
    scalingPolicy_targetTrackingScalingPolicyConfiguration,
    scalingPolicy_policyARN,
    scalingPolicy_policyName,
    scalingPolicy_serviceNamespace,
    scalingPolicy_resourceId,
    scalingPolicy_scalableDimension,
    scalingPolicy_policyType,
    scalingPolicy_creationTime,

    -- ** ScheduledAction
    scheduledAction_endTime,
    scheduledAction_scalableDimension,
    scheduledAction_scalableTargetAction,
    scheduledAction_startTime,
    scheduledAction_timezone,
    scheduledAction_scheduledActionName,
    scheduledAction_scheduledActionARN,
    scheduledAction_serviceNamespace,
    scheduledAction_schedule,
    scheduledAction_resourceId,
    scheduledAction_creationTime,

    -- ** StepAdjustment
    stepAdjustment_metricIntervalLowerBound,
    stepAdjustment_metricIntervalUpperBound,
    stepAdjustment_scalingAdjustment,

    -- ** StepScalingPolicyConfiguration
    stepScalingPolicyConfiguration_adjustmentType,
    stepScalingPolicyConfiguration_cooldown,
    stepScalingPolicyConfiguration_metricAggregationType,
    stepScalingPolicyConfiguration_minAdjustmentMagnitude,
    stepScalingPolicyConfiguration_stepAdjustments,

    -- ** SuspendedState
    suspendedState_dynamicScalingInSuspended,
    suspendedState_dynamicScalingOutSuspended,
    suspendedState_scheduledScalingSuspended,

    -- ** TargetTrackingScalingPolicyConfiguration
    targetTrackingScalingPolicyConfiguration_customizedMetricSpecification,
    targetTrackingScalingPolicyConfiguration_disableScaleIn,
    targetTrackingScalingPolicyConfiguration_predefinedMetricSpecification,
    targetTrackingScalingPolicyConfiguration_scaleInCooldown,
    targetTrackingScalingPolicyConfiguration_scaleOutCooldown,
    targetTrackingScalingPolicyConfiguration_targetValue,
  )
where

import Amazonka.ApplicationAutoScaling.DeleteScalingPolicy
import Amazonka.ApplicationAutoScaling.DeleteScheduledAction
import Amazonka.ApplicationAutoScaling.DeregisterScalableTarget
import Amazonka.ApplicationAutoScaling.DescribeScalableTargets
import Amazonka.ApplicationAutoScaling.DescribeScalingActivities
import Amazonka.ApplicationAutoScaling.DescribeScalingPolicies
import Amazonka.ApplicationAutoScaling.DescribeScheduledActions
import Amazonka.ApplicationAutoScaling.PutScalingPolicy
import Amazonka.ApplicationAutoScaling.PutScheduledAction
import Amazonka.ApplicationAutoScaling.RegisterScalableTarget
import Amazonka.ApplicationAutoScaling.Types.Alarm
import Amazonka.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Amazonka.ApplicationAutoScaling.Types.MetricDimension
import Amazonka.ApplicationAutoScaling.Types.NotScaledReason
import Amazonka.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import Amazonka.ApplicationAutoScaling.Types.ScalableTarget
import Amazonka.ApplicationAutoScaling.Types.ScalableTargetAction
import Amazonka.ApplicationAutoScaling.Types.ScalingActivity
import Amazonka.ApplicationAutoScaling.Types.ScalingPolicy
import Amazonka.ApplicationAutoScaling.Types.ScheduledAction
import Amazonka.ApplicationAutoScaling.Types.StepAdjustment
import Amazonka.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Amazonka.ApplicationAutoScaling.Types.SuspendedState
import Amazonka.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
