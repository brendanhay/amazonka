{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScalingPlans.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Lens
  ( -- * Operations

    -- ** DescribeScalingPlanResources
    describeScalingPlanResources_nextToken,
    describeScalingPlanResources_maxResults,
    describeScalingPlanResources_scalingPlanName,
    describeScalingPlanResources_scalingPlanVersion,
    describeScalingPlanResourcesResponse_nextToken,
    describeScalingPlanResourcesResponse_scalingPlanResources,
    describeScalingPlanResourcesResponse_httpStatus,

    -- ** CreateScalingPlan
    createScalingPlan_scalingPlanName,
    createScalingPlan_applicationSource,
    createScalingPlan_scalingInstructions,
    createScalingPlanResponse_httpStatus,
    createScalingPlanResponse_scalingPlanVersion,

    -- ** DeleteScalingPlan
    deleteScalingPlan_scalingPlanName,
    deleteScalingPlan_scalingPlanVersion,
    deleteScalingPlanResponse_httpStatus,

    -- ** UpdateScalingPlan
    updateScalingPlan_scalingInstructions,
    updateScalingPlan_applicationSource,
    updateScalingPlan_scalingPlanName,
    updateScalingPlan_scalingPlanVersion,
    updateScalingPlanResponse_httpStatus,

    -- ** DescribeScalingPlans
    describeScalingPlans_scalingPlanVersion,
    describeScalingPlans_scalingPlanNames,
    describeScalingPlans_nextToken,
    describeScalingPlans_applicationSources,
    describeScalingPlans_maxResults,
    describeScalingPlansResponse_scalingPlans,
    describeScalingPlansResponse_nextToken,
    describeScalingPlansResponse_httpStatus,

    -- ** GetScalingPlanResourceForecastData
    getScalingPlanResourceForecastData_scalingPlanName,
    getScalingPlanResourceForecastData_scalingPlanVersion,
    getScalingPlanResourceForecastData_serviceNamespace,
    getScalingPlanResourceForecastData_resourceId,
    getScalingPlanResourceForecastData_scalableDimension,
    getScalingPlanResourceForecastData_forecastDataType,
    getScalingPlanResourceForecastData_startTime,
    getScalingPlanResourceForecastData_endTime,
    getScalingPlanResourceForecastDataResponse_httpStatus,
    getScalingPlanResourceForecastDataResponse_datapoints,

    -- * Types

    -- ** ApplicationSource
    applicationSource_tagFilters,
    applicationSource_cloudFormationStackARN,

    -- ** CustomizedLoadMetricSpecification
    customizedLoadMetricSpecification_dimensions,
    customizedLoadMetricSpecification_unit,
    customizedLoadMetricSpecification_metricName,
    customizedLoadMetricSpecification_namespace,
    customizedLoadMetricSpecification_statistic,

    -- ** CustomizedScalingMetricSpecification
    customizedScalingMetricSpecification_dimensions,
    customizedScalingMetricSpecification_unit,
    customizedScalingMetricSpecification_metricName,
    customizedScalingMetricSpecification_namespace,
    customizedScalingMetricSpecification_statistic,

    -- ** Datapoint
    datapoint_value,
    datapoint_timestamp,

    -- ** MetricDimension
    metricDimension_name,
    metricDimension_value,

    -- ** PredefinedLoadMetricSpecification
    predefinedLoadMetricSpecification_resourceLabel,
    predefinedLoadMetricSpecification_predefinedLoadMetricType,

    -- ** PredefinedScalingMetricSpecification
    predefinedScalingMetricSpecification_resourceLabel,
    predefinedScalingMetricSpecification_predefinedScalingMetricType,

    -- ** ScalingInstruction
    scalingInstruction_scheduledActionBufferTime,
    scalingInstruction_predictiveScalingMaxCapacityBuffer,
    scalingInstruction_scalingPolicyUpdateBehavior,
    scalingInstruction_customizedLoadMetricSpecification,
    scalingInstruction_predictiveScalingMode,
    scalingInstruction_disableDynamicScaling,
    scalingInstruction_predictiveScalingMaxCapacityBehavior,
    scalingInstruction_predefinedLoadMetricSpecification,
    scalingInstruction_serviceNamespace,
    scalingInstruction_resourceId,
    scalingInstruction_scalableDimension,
    scalingInstruction_minCapacity,
    scalingInstruction_maxCapacity,
    scalingInstruction_targetTrackingConfigurations,

    -- ** ScalingPlan
    scalingPlan_creationTime,
    scalingPlan_statusStartTime,
    scalingPlan_statusMessage,
    scalingPlan_scalingPlanName,
    scalingPlan_scalingPlanVersion,
    scalingPlan_applicationSource,
    scalingPlan_scalingInstructions,
    scalingPlan_statusCode,

    -- ** ScalingPlanResource
    scalingPlanResource_scalingStatusMessage,
    scalingPlanResource_scalingPolicies,
    scalingPlanResource_scalingPlanName,
    scalingPlanResource_scalingPlanVersion,
    scalingPlanResource_serviceNamespace,
    scalingPlanResource_resourceId,
    scalingPlanResource_scalableDimension,
    scalingPlanResource_scalingStatusCode,

    -- ** ScalingPolicy
    scalingPolicy_targetTrackingConfiguration,
    scalingPolicy_policyName,
    scalingPolicy_policyType,

    -- ** TagFilter
    tagFilter_values,
    tagFilter_key,

    -- ** TargetTrackingConfiguration
    targetTrackingConfiguration_estimatedInstanceWarmup,
    targetTrackingConfiguration_predefinedScalingMetricSpecification,
    targetTrackingConfiguration_scaleInCooldown,
    targetTrackingConfiguration_disableScaleIn,
    targetTrackingConfiguration_customizedScalingMetricSpecification,
    targetTrackingConfiguration_scaleOutCooldown,
    targetTrackingConfiguration_targetValue,
  )
where

import Amazonka.AutoScalingPlans.CreateScalingPlan
import Amazonka.AutoScalingPlans.DeleteScalingPlan
import Amazonka.AutoScalingPlans.DescribeScalingPlanResources
import Amazonka.AutoScalingPlans.DescribeScalingPlans
import Amazonka.AutoScalingPlans.GetScalingPlanResourceForecastData
import Amazonka.AutoScalingPlans.Types.ApplicationSource
import Amazonka.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
import Amazonka.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Amazonka.AutoScalingPlans.Types.Datapoint
import Amazonka.AutoScalingPlans.Types.MetricDimension
import Amazonka.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
import Amazonka.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import Amazonka.AutoScalingPlans.Types.ScalingInstruction
import Amazonka.AutoScalingPlans.Types.ScalingPlan
import Amazonka.AutoScalingPlans.Types.ScalingPlanResource
import Amazonka.AutoScalingPlans.Types.ScalingPolicy
import Amazonka.AutoScalingPlans.Types.TagFilter
import Amazonka.AutoScalingPlans.Types.TargetTrackingConfiguration
import Amazonka.AutoScalingPlans.UpdateScalingPlan
