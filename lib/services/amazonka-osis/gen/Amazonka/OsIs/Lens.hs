{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OsIs.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Lens
  ( -- * Operations

    -- ** CreatePipeline
    createPipeline_logPublishingOptions,
    createPipeline_tags,
    createPipeline_vpcOptions,
    createPipeline_pipelineName,
    createPipeline_minUnits,
    createPipeline_maxUnits,
    createPipeline_pipelineConfigurationBody,
    createPipelineResponse_pipeline,
    createPipelineResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_pipelineName,
    deletePipelineResponse_httpStatus,

    -- ** GetPipeline
    getPipeline_pipelineName,
    getPipelineResponse_pipeline,
    getPipelineResponse_httpStatus,

    -- ** GetPipelineBlueprint
    getPipelineBlueprint_blueprintName,
    getPipelineBlueprintResponse_blueprint,
    getPipelineBlueprintResponse_httpStatus,

    -- ** GetPipelineChangeProgress
    getPipelineChangeProgress_pipelineName,
    getPipelineChangeProgressResponse_changeProgressStatuses,
    getPipelineChangeProgressResponse_httpStatus,

    -- ** ListPipelineBlueprints
    listPipelineBlueprintsResponse_blueprints,
    listPipelineBlueprintsResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_maxResults,
    listPipelines_nextToken,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelines,
    listPipelinesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartPipeline
    startPipeline_pipelineName,
    startPipelineResponse_pipeline,
    startPipelineResponse_httpStatus,

    -- ** StopPipeline
    stopPipeline_pipelineName,
    stopPipelineResponse_pipeline,
    stopPipelineResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_logPublishingOptions,
    updatePipeline_maxUnits,
    updatePipeline_minUnits,
    updatePipeline_pipelineConfigurationBody,
    updatePipeline_pipelineName,
    updatePipelineResponse_pipeline,
    updatePipelineResponse_httpStatus,

    -- ** ValidatePipeline
    validatePipeline_pipelineConfigurationBody,
    validatePipelineResponse_errors,
    validatePipelineResponse_isValid,
    validatePipelineResponse_httpStatus,

    -- * Types

    -- ** ChangeProgressStage
    changeProgressStage_description,
    changeProgressStage_lastUpdatedAt,
    changeProgressStage_name,
    changeProgressStage_status,

    -- ** ChangeProgressStatus
    changeProgressStatus_changeProgressStages,
    changeProgressStatus_startTime,
    changeProgressStatus_status,
    changeProgressStatus_totalNumberOfStages,

    -- ** CloudWatchLogDestination
    cloudWatchLogDestination_logGroup,

    -- ** LogPublishingOptions
    logPublishingOptions_cloudWatchLogDestination,
    logPublishingOptions_isLoggingEnabled,

    -- ** Pipeline
    pipeline_createdAt,
    pipeline_ingestEndpointUrls,
    pipeline_lastUpdatedAt,
    pipeline_logPublishingOptions,
    pipeline_maxUnits,
    pipeline_minUnits,
    pipeline_pipelineArn,
    pipeline_pipelineConfigurationBody,
    pipeline_pipelineName,
    pipeline_status,
    pipeline_statusReason,
    pipeline_vpcEndpoints,

    -- ** PipelineBlueprint
    pipelineBlueprint_blueprintName,
    pipelineBlueprint_pipelineConfigurationBody,

    -- ** PipelineBlueprintSummary
    pipelineBlueprintSummary_blueprintName,

    -- ** PipelineStatusReason
    pipelineStatusReason_description,

    -- ** PipelineSummary
    pipelineSummary_createdAt,
    pipelineSummary_lastUpdatedAt,
    pipelineSummary_maxUnits,
    pipelineSummary_minUnits,
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineName,
    pipelineSummary_status,
    pipelineSummary_statusReason,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ValidationMessage
    validationMessage_message,

    -- ** VpcEndpoint
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,
    vpcEndpoint_vpcOptions,

    -- ** VpcOptions
    vpcOptions_securityGroupIds,
    vpcOptions_subnetIds,
  )
where

import Amazonka.OsIs.CreatePipeline
import Amazonka.OsIs.DeletePipeline
import Amazonka.OsIs.GetPipeline
import Amazonka.OsIs.GetPipelineBlueprint
import Amazonka.OsIs.GetPipelineChangeProgress
import Amazonka.OsIs.ListPipelineBlueprints
import Amazonka.OsIs.ListPipelines
import Amazonka.OsIs.ListTagsForResource
import Amazonka.OsIs.StartPipeline
import Amazonka.OsIs.StopPipeline
import Amazonka.OsIs.TagResource
import Amazonka.OsIs.Types.ChangeProgressStage
import Amazonka.OsIs.Types.ChangeProgressStatus
import Amazonka.OsIs.Types.CloudWatchLogDestination
import Amazonka.OsIs.Types.LogPublishingOptions
import Amazonka.OsIs.Types.Pipeline
import Amazonka.OsIs.Types.PipelineBlueprint
import Amazonka.OsIs.Types.PipelineBlueprintSummary
import Amazonka.OsIs.Types.PipelineStatusReason
import Amazonka.OsIs.Types.PipelineSummary
import Amazonka.OsIs.Types.Tag
import Amazonka.OsIs.Types.ValidationMessage
import Amazonka.OsIs.Types.VpcEndpoint
import Amazonka.OsIs.Types.VpcOptions
import Amazonka.OsIs.UntagResource
import Amazonka.OsIs.UpdatePipeline
import Amazonka.OsIs.ValidatePipeline
