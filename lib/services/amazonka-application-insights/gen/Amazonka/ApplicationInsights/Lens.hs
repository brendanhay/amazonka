{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationInsights.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_autoConfigEnabled,
    createApplication_autoCreate,
    createApplication_cWEMonitorEnabled,
    createApplication_groupingType,
    createApplication_opsCenterEnabled,
    createApplication_opsItemSNSTopicArn,
    createApplication_resourceGroupName,
    createApplication_tags,
    createApplicationResponse_applicationInfo,
    createApplicationResponse_httpStatus,

    -- ** CreateComponent
    createComponent_resourceGroupName,
    createComponent_componentName,
    createComponent_resourceList,
    createComponentResponse_httpStatus,

    -- ** CreateLogPattern
    createLogPattern_resourceGroupName,
    createLogPattern_patternSetName,
    createLogPattern_patternName,
    createLogPattern_pattern,
    createLogPattern_rank,
    createLogPatternResponse_logPattern,
    createLogPatternResponse_resourceGroupName,
    createLogPatternResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_resourceGroupName,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_resourceGroupName,
    deleteComponent_componentName,
    deleteComponentResponse_httpStatus,

    -- ** DeleteLogPattern
    deleteLogPattern_resourceGroupName,
    deleteLogPattern_patternSetName,
    deleteLogPattern_patternName,
    deleteLogPatternResponse_httpStatus,

    -- ** DescribeApplication
    describeApplication_resourceGroupName,
    describeApplicationResponse_applicationInfo,
    describeApplicationResponse_httpStatus,

    -- ** DescribeComponent
    describeComponent_resourceGroupName,
    describeComponent_componentName,
    describeComponentResponse_applicationComponent,
    describeComponentResponse_resourceList,
    describeComponentResponse_httpStatus,

    -- ** DescribeComponentConfiguration
    describeComponentConfiguration_resourceGroupName,
    describeComponentConfiguration_componentName,
    describeComponentConfigurationResponse_componentConfiguration,
    describeComponentConfigurationResponse_monitor,
    describeComponentConfigurationResponse_tier,
    describeComponentConfigurationResponse_httpStatus,

    -- ** DescribeComponentConfigurationRecommendation
    describeComponentConfigurationRecommendation_resourceGroupName,
    describeComponentConfigurationRecommendation_componentName,
    describeComponentConfigurationRecommendation_tier,
    describeComponentConfigurationRecommendationResponse_componentConfiguration,
    describeComponentConfigurationRecommendationResponse_httpStatus,

    -- ** DescribeLogPattern
    describeLogPattern_resourceGroupName,
    describeLogPattern_patternSetName,
    describeLogPattern_patternName,
    describeLogPatternResponse_logPattern,
    describeLogPatternResponse_resourceGroupName,
    describeLogPatternResponse_httpStatus,

    -- ** DescribeObservation
    describeObservation_observationId,
    describeObservationResponse_observation,
    describeObservationResponse_httpStatus,

    -- ** DescribeProblem
    describeProblem_problemId,
    describeProblemResponse_problem,
    describeProblemResponse_httpStatus,

    -- ** DescribeProblemObservations
    describeProblemObservations_problemId,
    describeProblemObservationsResponse_relatedObservations,
    describeProblemObservationsResponse_httpStatus,

    -- ** ListApplications
    listApplications_maxResults,
    listApplications_nextToken,
    listApplicationsResponse_applicationInfoList,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListComponents
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_resourceGroupName,
    listComponentsResponse_applicationComponentList,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,

    -- ** ListConfigurationHistory
    listConfigurationHistory_endTime,
    listConfigurationHistory_eventStatus,
    listConfigurationHistory_maxResults,
    listConfigurationHistory_nextToken,
    listConfigurationHistory_resourceGroupName,
    listConfigurationHistory_startTime,
    listConfigurationHistoryResponse_eventList,
    listConfigurationHistoryResponse_nextToken,
    listConfigurationHistoryResponse_httpStatus,

    -- ** ListLogPatternSets
    listLogPatternSets_maxResults,
    listLogPatternSets_nextToken,
    listLogPatternSets_resourceGroupName,
    listLogPatternSetsResponse_logPatternSets,
    listLogPatternSetsResponse_nextToken,
    listLogPatternSetsResponse_resourceGroupName,
    listLogPatternSetsResponse_httpStatus,

    -- ** ListLogPatterns
    listLogPatterns_maxResults,
    listLogPatterns_nextToken,
    listLogPatterns_patternSetName,
    listLogPatterns_resourceGroupName,
    listLogPatternsResponse_logPatterns,
    listLogPatternsResponse_nextToken,
    listLogPatternsResponse_resourceGroupName,
    listLogPatternsResponse_httpStatus,

    -- ** ListProblems
    listProblems_componentName,
    listProblems_endTime,
    listProblems_maxResults,
    listProblems_nextToken,
    listProblems_resourceGroupName,
    listProblems_startTime,
    listProblemsResponse_nextToken,
    listProblemsResponse_problemList,
    listProblemsResponse_resourceGroupName,
    listProblemsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_autoConfigEnabled,
    updateApplication_cWEMonitorEnabled,
    updateApplication_opsCenterEnabled,
    updateApplication_opsItemSNSTopicArn,
    updateApplication_removeSNSTopic,
    updateApplication_resourceGroupName,
    updateApplicationResponse_applicationInfo,
    updateApplicationResponse_httpStatus,

    -- ** UpdateComponent
    updateComponent_newComponentName,
    updateComponent_resourceList,
    updateComponent_resourceGroupName,
    updateComponent_componentName,
    updateComponentResponse_httpStatus,

    -- ** UpdateComponentConfiguration
    updateComponentConfiguration_autoConfigEnabled,
    updateComponentConfiguration_componentConfiguration,
    updateComponentConfiguration_monitor,
    updateComponentConfiguration_tier,
    updateComponentConfiguration_resourceGroupName,
    updateComponentConfiguration_componentName,
    updateComponentConfigurationResponse_httpStatus,

    -- ** UpdateLogPattern
    updateLogPattern_pattern,
    updateLogPattern_rank,
    updateLogPattern_resourceGroupName,
    updateLogPattern_patternSetName,
    updateLogPattern_patternName,
    updateLogPatternResponse_logPattern,
    updateLogPatternResponse_resourceGroupName,
    updateLogPatternResponse_httpStatus,

    -- * Types

    -- ** ApplicationComponent
    applicationComponent_componentName,
    applicationComponent_componentRemarks,
    applicationComponent_detectedWorkload,
    applicationComponent_monitor,
    applicationComponent_osType,
    applicationComponent_resourceType,
    applicationComponent_tier,

    -- ** ApplicationInfo
    applicationInfo_autoConfigEnabled,
    applicationInfo_cWEMonitorEnabled,
    applicationInfo_discoveryType,
    applicationInfo_lifeCycle,
    applicationInfo_opsCenterEnabled,
    applicationInfo_opsItemSNSTopicArn,
    applicationInfo_remarks,
    applicationInfo_resourceGroupName,

    -- ** ConfigurationEvent
    configurationEvent_eventDetail,
    configurationEvent_eventResourceName,
    configurationEvent_eventResourceType,
    configurationEvent_eventStatus,
    configurationEvent_eventTime,
    configurationEvent_monitoredResourceARN,

    -- ** LogPattern
    logPattern_pattern,
    logPattern_patternName,
    logPattern_patternSetName,
    logPattern_rank,

    -- ** Observation
    observation_cloudWatchEventDetailType,
    observation_cloudWatchEventId,
    observation_cloudWatchEventSource,
    observation_codeDeployApplication,
    observation_codeDeployDeploymentGroup,
    observation_codeDeployDeploymentId,
    observation_codeDeployInstanceGroupId,
    observation_codeDeployState,
    observation_ebsCause,
    observation_ebsEvent,
    observation_ebsRequestId,
    observation_ebsResult,
    observation_ec2State,
    observation_endTime,
    observation_healthEventArn,
    observation_healthEventDescription,
    observation_healthEventTypeCategory,
    observation_healthEventTypeCode,
    observation_healthService,
    observation_id,
    observation_lineTime,
    observation_logFilter,
    observation_logGroup,
    observation_logText,
    observation_metricName,
    observation_metricNamespace,
    observation_rdsEventCategories,
    observation_rdsEventMessage,
    observation_s3EventName,
    observation_sourceARN,
    observation_sourceType,
    observation_startTime,
    observation_statesArn,
    observation_statesExecutionArn,
    observation_statesInput,
    observation_statesStatus,
    observation_unit,
    observation_value,
    observation_xRayErrorPercent,
    observation_xRayFaultPercent,
    observation_xRayNodeName,
    observation_xRayNodeType,
    observation_xRayRequestAverageLatency,
    observation_xRayRequestCount,
    observation_xRayThrottlePercent,

    -- ** Problem
    problem_affectedResource,
    problem_endTime,
    problem_feedback,
    problem_id,
    problem_insights,
    problem_lastRecurrenceTime,
    problem_recurringCount,
    problem_resourceGroupName,
    problem_severityLevel,
    problem_startTime,
    problem_status,
    problem_title,

    -- ** RelatedObservations
    relatedObservations_observationList,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.ApplicationInsights.CreateApplication
import Amazonka.ApplicationInsights.CreateComponent
import Amazonka.ApplicationInsights.CreateLogPattern
import Amazonka.ApplicationInsights.DeleteApplication
import Amazonka.ApplicationInsights.DeleteComponent
import Amazonka.ApplicationInsights.DeleteLogPattern
import Amazonka.ApplicationInsights.DescribeApplication
import Amazonka.ApplicationInsights.DescribeComponent
import Amazonka.ApplicationInsights.DescribeComponentConfiguration
import Amazonka.ApplicationInsights.DescribeComponentConfigurationRecommendation
import Amazonka.ApplicationInsights.DescribeLogPattern
import Amazonka.ApplicationInsights.DescribeObservation
import Amazonka.ApplicationInsights.DescribeProblem
import Amazonka.ApplicationInsights.DescribeProblemObservations
import Amazonka.ApplicationInsights.ListApplications
import Amazonka.ApplicationInsights.ListComponents
import Amazonka.ApplicationInsights.ListConfigurationHistory
import Amazonka.ApplicationInsights.ListLogPatternSets
import Amazonka.ApplicationInsights.ListLogPatterns
import Amazonka.ApplicationInsights.ListProblems
import Amazonka.ApplicationInsights.ListTagsForResource
import Amazonka.ApplicationInsights.TagResource
import Amazonka.ApplicationInsights.Types.ApplicationComponent
import Amazonka.ApplicationInsights.Types.ApplicationInfo
import Amazonka.ApplicationInsights.Types.ConfigurationEvent
import Amazonka.ApplicationInsights.Types.LogPattern
import Amazonka.ApplicationInsights.Types.Observation
import Amazonka.ApplicationInsights.Types.Problem
import Amazonka.ApplicationInsights.Types.RelatedObservations
import Amazonka.ApplicationInsights.Types.Tag
import Amazonka.ApplicationInsights.UntagResource
import Amazonka.ApplicationInsights.UpdateApplication
import Amazonka.ApplicationInsights.UpdateComponent
import Amazonka.ApplicationInsights.UpdateComponentConfiguration
import Amazonka.ApplicationInsights.UpdateLogPattern
