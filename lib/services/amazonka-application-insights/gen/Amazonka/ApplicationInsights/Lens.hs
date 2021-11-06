{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApplicationInsights.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Lens
  ( -- * Operations

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

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_resourceGroupName,
    deleteApplicationResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_cWEMonitorEnabled,
    updateApplication_opsItemSNSTopicArn,
    updateApplication_removeSNSTopic,
    updateApplication_opsCenterEnabled,
    updateApplication_resourceGroupName,
    updateApplicationResponse_applicationInfo,
    updateApplicationResponse_httpStatus,

    -- ** DescribeComponentConfigurationRecommendation
    describeComponentConfigurationRecommendation_resourceGroupName,
    describeComponentConfigurationRecommendation_componentName,
    describeComponentConfigurationRecommendation_tier,
    describeComponentConfigurationRecommendationResponse_componentConfiguration,
    describeComponentConfigurationRecommendationResponse_httpStatus,

    -- ** DescribeProblem
    describeProblem_problemId,
    describeProblemResponse_problem,
    describeProblemResponse_httpStatus,

    -- ** UpdateComponentConfiguration
    updateComponentConfiguration_componentConfiguration,
    updateComponentConfiguration_monitor,
    updateComponentConfiguration_tier,
    updateComponentConfiguration_resourceGroupName,
    updateComponentConfiguration_componentName,
    updateComponentConfigurationResponse_httpStatus,

    -- ** CreateApplication
    createApplication_cWEMonitorEnabled,
    createApplication_opsItemSNSTopicArn,
    createApplication_opsCenterEnabled,
    createApplication_tags,
    createApplication_resourceGroupName,
    createApplicationResponse_applicationInfo,
    createApplicationResponse_httpStatus,

    -- ** DescribeProblemObservations
    describeProblemObservations_problemId,
    describeProblemObservationsResponse_relatedObservations,
    describeProblemObservationsResponse_httpStatus,

    -- ** DescribeObservation
    describeObservation_observationId,
    describeObservationResponse_observation,
    describeObservationResponse_httpStatus,

    -- ** ListLogPatternSets
    listLogPatternSets_nextToken,
    listLogPatternSets_maxResults,
    listLogPatternSets_resourceGroupName,
    listLogPatternSetsResponse_resourceGroupName,
    listLogPatternSetsResponse_nextToken,
    listLogPatternSetsResponse_logPatternSets,
    listLogPatternSetsResponse_httpStatus,

    -- ** DescribeComponentConfiguration
    describeComponentConfiguration_resourceGroupName,
    describeComponentConfiguration_componentName,
    describeComponentConfigurationResponse_componentConfiguration,
    describeComponentConfigurationResponse_monitor,
    describeComponentConfigurationResponse_tier,
    describeComponentConfigurationResponse_httpStatus,

    -- ** ListProblems
    listProblems_resourceGroupName,
    listProblems_startTime,
    listProblems_nextToken,
    listProblems_endTime,
    listProblems_maxResults,
    listProblemsResponse_nextToken,
    listProblemsResponse_problemList,
    listProblemsResponse_httpStatus,

    -- ** ListLogPatterns
    listLogPatterns_nextToken,
    listLogPatterns_patternSetName,
    listLogPatterns_maxResults,
    listLogPatterns_resourceGroupName,
    listLogPatternsResponse_resourceGroupName,
    listLogPatternsResponse_nextToken,
    listLogPatternsResponse_logPatterns,
    listLogPatternsResponse_httpStatus,

    -- ** DeleteLogPattern
    deleteLogPattern_resourceGroupName,
    deleteLogPattern_patternSetName,
    deleteLogPattern_patternName,
    deleteLogPatternResponse_httpStatus,

    -- ** UpdateLogPattern
    updateLogPattern_pattern,
    updateLogPattern_rank,
    updateLogPattern_resourceGroupName,
    updateLogPattern_patternSetName,
    updateLogPattern_patternName,
    updateLogPatternResponse_logPattern,
    updateLogPatternResponse_resourceGroupName,
    updateLogPatternResponse_httpStatus,

    -- ** CreateLogPattern
    createLogPattern_resourceGroupName,
    createLogPattern_patternSetName,
    createLogPattern_patternName,
    createLogPattern_pattern,
    createLogPattern_rank,
    createLogPatternResponse_logPattern,
    createLogPatternResponse_resourceGroupName,
    createLogPatternResponse_httpStatus,

    -- ** ListConfigurationHistory
    listConfigurationHistory_resourceGroupName,
    listConfigurationHistory_startTime,
    listConfigurationHistory_eventStatus,
    listConfigurationHistory_nextToken,
    listConfigurationHistory_endTime,
    listConfigurationHistory_maxResults,
    listConfigurationHistoryResponse_nextToken,
    listConfigurationHistoryResponse_eventList,
    listConfigurationHistoryResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applicationInfoList,
    listApplicationsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateComponent
    createComponent_resourceGroupName,
    createComponent_componentName,
    createComponent_resourceList,
    createComponentResponse_httpStatus,

    -- ** ListComponents
    listComponents_nextToken,
    listComponents_maxResults,
    listComponents_resourceGroupName,
    listComponentsResponse_applicationComponentList,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_resourceGroupName,
    deleteComponent_componentName,
    deleteComponentResponse_httpStatus,

    -- ** UpdateComponent
    updateComponent_newComponentName,
    updateComponent_resourceList,
    updateComponent_resourceGroupName,
    updateComponent_componentName,
    updateComponentResponse_httpStatus,

    -- ** DescribeLogPattern
    describeLogPattern_resourceGroupName,
    describeLogPattern_patternSetName,
    describeLogPattern_patternName,
    describeLogPatternResponse_logPattern,
    describeLogPatternResponse_resourceGroupName,
    describeLogPatternResponse_httpStatus,

    -- * Types

    -- ** ApplicationComponent
    applicationComponent_osType,
    applicationComponent_resourceType,
    applicationComponent_detectedWorkload,
    applicationComponent_monitor,
    applicationComponent_tier,
    applicationComponent_componentName,
    applicationComponent_componentRemarks,

    -- ** ApplicationInfo
    applicationInfo_resourceGroupName,
    applicationInfo_cWEMonitorEnabled,
    applicationInfo_opsItemSNSTopicArn,
    applicationInfo_lifeCycle,
    applicationInfo_opsCenterEnabled,
    applicationInfo_remarks,

    -- ** ConfigurationEvent
    configurationEvent_monitoredResourceARN,
    configurationEvent_eventStatus,
    configurationEvent_eventResourceName,
    configurationEvent_eventTime,
    configurationEvent_eventDetail,
    configurationEvent_eventResourceType,

    -- ** LogPattern
    logPattern_pattern,
    logPattern_patternName,
    logPattern_patternSetName,
    logPattern_rank,

    -- ** Observation
    observation_codeDeployApplication,
    observation_rdsEventMessage,
    observation_codeDeployDeploymentId,
    observation_startTime,
    observation_sourceType,
    observation_sourceARN,
    observation_xRayRequestAverageLatency,
    observation_statesStatus,
    observation_codeDeployDeploymentGroup,
    observation_healthEventTypeCategory,
    observation_xRayRequestCount,
    observation_s3EventName,
    observation_metricName,
    observation_ec2State,
    observation_logGroup,
    observation_value,
    observation_healthEventDescription,
    observation_cloudWatchEventSource,
    observation_codeDeployState,
    observation_xRayErrorPercent,
    observation_statesArn,
    observation_cloudWatchEventId,
    observation_logText,
    observation_logFilter,
    observation_metricNamespace,
    observation_rdsEventCategories,
    observation_xRayNodeType,
    observation_endTime,
    observation_statesInput,
    observation_xRayNodeName,
    observation_id,
    observation_healthEventArn,
    observation_healthEventTypeCode,
    observation_ebsResult,
    observation_cloudWatchEventDetailType,
    observation_codeDeployInstanceGroupId,
    observation_ebsCause,
    observation_ebsEvent,
    observation_ebsRequestId,
    observation_xRayFaultPercent,
    observation_statesExecutionArn,
    observation_lineTime,
    observation_unit,
    observation_xRayThrottlePercent,
    observation_healthService,

    -- ** Problem
    problem_status,
    problem_resourceGroupName,
    problem_startTime,
    problem_insights,
    problem_endTime,
    problem_id,
    problem_severityLevel,
    problem_title,
    problem_affectedResource,
    problem_feedback,

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
