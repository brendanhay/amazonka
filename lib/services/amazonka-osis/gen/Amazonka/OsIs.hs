{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.OsIs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use the Amazon OpenSearch Ingestion API to create and manage ingestion
-- pipelines. OpenSearch Ingestion is a fully managed data collector that
-- delivers real-time log and trace data to OpenSearch Service domains. For
-- more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/ingestion.html Getting data into your cluster using OpenSearch Ingestion>.
module Amazonka.OsIs
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalException
    _InternalException,

    -- ** InvalidPaginationTokenException
    _InvalidPaginationTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** GetPipeline
    GetPipeline (GetPipeline'),
    newGetPipeline,
    GetPipelineResponse (GetPipelineResponse'),
    newGetPipelineResponse,

    -- ** GetPipelineBlueprint
    GetPipelineBlueprint (GetPipelineBlueprint'),
    newGetPipelineBlueprint,
    GetPipelineBlueprintResponse (GetPipelineBlueprintResponse'),
    newGetPipelineBlueprintResponse,

    -- ** GetPipelineChangeProgress
    GetPipelineChangeProgress (GetPipelineChangeProgress'),
    newGetPipelineChangeProgress,
    GetPipelineChangeProgressResponse (GetPipelineChangeProgressResponse'),
    newGetPipelineChangeProgressResponse,

    -- ** ListPipelineBlueprints
    ListPipelineBlueprints (ListPipelineBlueprints'),
    newListPipelineBlueprints,
    ListPipelineBlueprintsResponse (ListPipelineBlueprintsResponse'),
    newListPipelineBlueprintsResponse,

    -- ** ListPipelines
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartPipeline
    StartPipeline (StartPipeline'),
    newStartPipeline,
    StartPipelineResponse (StartPipelineResponse'),
    newStartPipelineResponse,

    -- ** StopPipeline
    StopPipeline (StopPipeline'),
    newStopPipeline,
    StopPipelineResponse (StopPipelineResponse'),
    newStopPipelineResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** ValidatePipeline
    ValidatePipeline (ValidatePipeline'),
    newValidatePipeline,
    ValidatePipelineResponse (ValidatePipelineResponse'),
    newValidatePipelineResponse,

    -- * Types

    -- ** ChangeProgressStageStatuses
    ChangeProgressStageStatuses (..),

    -- ** ChangeProgressStatuses
    ChangeProgressStatuses (..),

    -- ** PipelineStatus
    PipelineStatus (..),

    -- ** ChangeProgressStage
    ChangeProgressStage (ChangeProgressStage'),
    newChangeProgressStage,

    -- ** ChangeProgressStatus
    ChangeProgressStatus (ChangeProgressStatus'),
    newChangeProgressStatus,

    -- ** CloudWatchLogDestination
    CloudWatchLogDestination (CloudWatchLogDestination'),
    newCloudWatchLogDestination,

    -- ** LogPublishingOptions
    LogPublishingOptions (LogPublishingOptions'),
    newLogPublishingOptions,

    -- ** Pipeline
    Pipeline (Pipeline'),
    newPipeline,

    -- ** PipelineBlueprint
    PipelineBlueprint (PipelineBlueprint'),
    newPipelineBlueprint,

    -- ** PipelineBlueprintSummary
    PipelineBlueprintSummary (PipelineBlueprintSummary'),
    newPipelineBlueprintSummary,

    -- ** PipelineStatusReason
    PipelineStatusReason (PipelineStatusReason'),
    newPipelineStatusReason,

    -- ** PipelineSummary
    PipelineSummary (PipelineSummary'),
    newPipelineSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ValidationMessage
    ValidationMessage (ValidationMessage'),
    newValidationMessage,

    -- ** VpcEndpoint
    VpcEndpoint (VpcEndpoint'),
    newVpcEndpoint,

    -- ** VpcOptions
    VpcOptions (VpcOptions'),
    newVpcOptions,
  )
where

import Amazonka.OsIs.CreatePipeline
import Amazonka.OsIs.DeletePipeline
import Amazonka.OsIs.GetPipeline
import Amazonka.OsIs.GetPipelineBlueprint
import Amazonka.OsIs.GetPipelineChangeProgress
import Amazonka.OsIs.Lens
import Amazonka.OsIs.ListPipelineBlueprints
import Amazonka.OsIs.ListPipelines
import Amazonka.OsIs.ListTagsForResource
import Amazonka.OsIs.StartPipeline
import Amazonka.OsIs.StopPipeline
import Amazonka.OsIs.TagResource
import Amazonka.OsIs.Types
import Amazonka.OsIs.UntagResource
import Amazonka.OsIs.UpdatePipeline
import Amazonka.OsIs.ValidatePipeline
import Amazonka.OsIs.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'OsIs'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
