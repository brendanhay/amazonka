{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationInsights.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Lens
  ( -- * Operations

    -- ** CreateApplication
    createApplication_tags,
    createApplication_autoConfigEnabled,
    createApplication_opsItemSNSTopicArn,
    createApplication_cWEMonitorEnabled,
    createApplication_resourceGroupName,
    createApplication_groupingType,
    createApplication_opsCenterEnabled,
    createApplication_autoCreate,
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
    describeComponentResponse_resourceList,
    describeComponentResponse_applicationComponent,
    describeComponentResponse_httpStatus,

    -- ** DescribeComponentConfiguration
    describeComponentConfiguration_resourceGroupName,
    describeComponentConfiguration_componentName,
    describeComponentConfigurationResponse_tier,
    describeComponentConfigurationResponse_monitor,
    describeComponentConfigurationResponse_componentConfiguration,
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
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applicationInfoList,
    listApplicationsResponse_httpStatus,

    -- ** ListComponents
    listComponents_nextToken,
    listComponents_maxResults,
    listComponents_resourceGroupName,
    listComponentsResponse_nextToken,
    listComponentsResponse_applicationComponentList,
    listComponentsResponse_httpStatus,

    -- ** ListConfigurationHistory
    listConfigurationHistory_nextToken,
    listConfigurationHistory_endTime,
    listConfigurationHistory_resourceGroupName,
    listConfigurationHistory_maxResults,
    listConfigurationHistory_eventStatus,
    listConfigurationHistory_startTime,
    listConfigurationHistoryResponse_nextToken,
    listConfigurationHistoryResponse_eventList,
    listConfigurationHistoryResponse_httpStatus,

    -- ** ListLogPatternSets
    listLogPatternSets_nextToken,
    listLogPatternSets_maxResults,
    listLogPatternSets_resourceGroupName,
    listLogPatternSetsResponse_nextToken,
    listLogPatternSetsResponse_resourceGroupName,
    listLogPatternSetsResponse_logPatternSets,
    listLogPatternSetsResponse_httpStatus,

    -- ** ListLogPatterns
    listLogPatterns_nextToken,
    listLogPatterns_maxResults,
    listLogPatterns_patternSetName,
    listLogPatterns_resourceGroupName,
    listLogPatternsResponse_nextToken,
    listLogPatternsResponse_resourceGroupName,
    listLogPatternsResponse_logPatterns,
    listLogPatternsResponse_httpStatus,

    -- ** ListProblems
    listProblems_nextToken,
    listProblems_componentName,
    listProblems_endTime,
    listProblems_resourceGroupName,
    listProblems_maxResults,
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
    updateApplication_opsItemSNSTopicArn,
    updateApplication_cWEMonitorEnabled,
    updateApplication_removeSNSTopic,
    updateApplication_opsCenterEnabled,
    updateApplication_resourceGroupName,
    updateApplicationResponse_applicationInfo,
    updateApplicationResponse_httpStatus,

    -- ** UpdateComponent
    updateComponent_resourceList,
    updateComponent_newComponentName,
    updateComponent_resourceGroupName,
    updateComponent_componentName,
    updateComponentResponse_httpStatus,

    -- ** UpdateComponentConfiguration
    updateComponentConfiguration_autoConfigEnabled,
    updateComponentConfiguration_tier,
    updateComponentConfiguration_monitor,
    updateComponentConfiguration_componentConfiguration,
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
    applicationComponent_resourceType,
    applicationComponent_componentRemarks,
    applicationComponent_componentName,
    applicationComponent_detectedWorkload,
    applicationComponent_tier,
    applicationComponent_monitor,
    applicationComponent_osType,

    -- ** ApplicationInfo
    applicationInfo_lifeCycle,
    applicationInfo_autoConfigEnabled,
    applicationInfo_discoveryType,
    applicationInfo_opsItemSNSTopicArn,
    applicationInfo_cWEMonitorEnabled,
    applicationInfo_resourceGroupName,
    applicationInfo_remarks,
    applicationInfo_opsCenterEnabled,

    -- ** ConfigurationEvent
    configurationEvent_eventResourceName,
    configurationEvent_monitoredResourceARN,
    configurationEvent_eventStatus,
    configurationEvent_eventResourceType,
    configurationEvent_eventTime,
    configurationEvent_eventDetail,

    -- ** LogPattern
    logPattern_pattern,
    logPattern_patternName,
    logPattern_rank,
    logPattern_patternSetName,

    -- ** Observation
    observation_cloudWatchEventId,
    observation_xRayFaultPercent,
    observation_logFilter,
    observation_logGroup,
    observation_codeDeployState,
    observation_codeDeployInstanceGroupId,
    observation_codeDeployDeploymentGroup,
    observation_sourceARN,
    observation_xRayRequestAverageLatency,
    observation_healthEventDescription,
    observation_xRayThrottlePercent,
    observation_ebsCause,
    observation_healthEventTypeCategory,
    observation_xRayNodeType,
    observation_statesArn,
    observation_statesInput,
    observation_healthEventArn,
    observation_ebsRequestId,
    observation_xRayErrorPercent,
    observation_cloudWatchEventSource,
    observation_sourceType,
    observation_endTime,
    observation_id,
    observation_ebsEvent,
    observation_rdsEventCategories,
    observation_ec2State,
    observation_statesStatus,
    observation_healthEventTypeCode,
    observation_statesExecutionArn,
    observation_metricName,
    observation_s3EventName,
    observation_ebsResult,
    observation_cloudWatchEventDetailType,
    observation_codeDeployApplication,
    observation_lineTime,
    observation_codeDeployDeploymentId,
    observation_logText,
    observation_xRayNodeName,
    observation_unit,
    observation_healthService,
    observation_startTime,
    observation_metricNamespace,
    observation_rdsEventMessage,
    observation_xRayRequestCount,
    observation_value,

    -- ** Problem
    problem_affectedResource,
    problem_recurringCount,
    problem_feedback,
    problem_status,
    problem_endTime,
    problem_id,
    problem_insights,
    problem_resourceGroupName,
    problem_title,
    problem_severityLevel,
    problem_lastRecurrenceTime,
    problem_startTime,

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
