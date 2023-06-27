{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OsIs.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalException,
    _InvalidPaginationTokenException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ValidationException,

    -- * ChangeProgressStageStatuses
    ChangeProgressStageStatuses (..),

    -- * ChangeProgressStatuses
    ChangeProgressStatuses (..),

    -- * PipelineStatus
    PipelineStatus (..),

    -- * ChangeProgressStage
    ChangeProgressStage (..),
    newChangeProgressStage,
    changeProgressStage_description,
    changeProgressStage_lastUpdatedAt,
    changeProgressStage_name,
    changeProgressStage_status,

    -- * ChangeProgressStatus
    ChangeProgressStatus (..),
    newChangeProgressStatus,
    changeProgressStatus_changeProgressStages,
    changeProgressStatus_startTime,
    changeProgressStatus_status,
    changeProgressStatus_totalNumberOfStages,

    -- * CloudWatchLogDestination
    CloudWatchLogDestination (..),
    newCloudWatchLogDestination,
    cloudWatchLogDestination_logGroup,

    -- * LogPublishingOptions
    LogPublishingOptions (..),
    newLogPublishingOptions,
    logPublishingOptions_cloudWatchLogDestination,
    logPublishingOptions_isLoggingEnabled,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
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

    -- * PipelineBlueprint
    PipelineBlueprint (..),
    newPipelineBlueprint,
    pipelineBlueprint_blueprintName,
    pipelineBlueprint_pipelineConfigurationBody,

    -- * PipelineBlueprintSummary
    PipelineBlueprintSummary (..),
    newPipelineBlueprintSummary,
    pipelineBlueprintSummary_blueprintName,

    -- * PipelineStatusReason
    PipelineStatusReason (..),
    newPipelineStatusReason,
    pipelineStatusReason_description,

    -- * PipelineSummary
    PipelineSummary (..),
    newPipelineSummary,
    pipelineSummary_createdAt,
    pipelineSummary_lastUpdatedAt,
    pipelineSummary_maxUnits,
    pipelineSummary_minUnits,
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineName,
    pipelineSummary_status,
    pipelineSummary_statusReason,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ValidationMessage
    ValidationMessage (..),
    newValidationMessage,
    validationMessage_message,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,
    vpcEndpoint_vpcOptions,

    -- * VpcOptions
    VpcOptions (..),
    newVpcOptions,
    vpcOptions_securityGroupIds,
    vpcOptions_subnetIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OsIs.Types.ChangeProgressStage
import Amazonka.OsIs.Types.ChangeProgressStageStatuses
import Amazonka.OsIs.Types.ChangeProgressStatus
import Amazonka.OsIs.Types.ChangeProgressStatuses
import Amazonka.OsIs.Types.CloudWatchLogDestination
import Amazonka.OsIs.Types.LogPublishingOptions
import Amazonka.OsIs.Types.Pipeline
import Amazonka.OsIs.Types.PipelineBlueprint
import Amazonka.OsIs.Types.PipelineBlueprintSummary
import Amazonka.OsIs.Types.PipelineStatus
import Amazonka.OsIs.Types.PipelineStatusReason
import Amazonka.OsIs.Types.PipelineSummary
import Amazonka.OsIs.Types.Tag
import Amazonka.OsIs.Types.ValidationMessage
import Amazonka.OsIs.Types.VpcEndpoint
import Amazonka.OsIs.Types.VpcOptions
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-01-01@ of the Amazon OpenSearch Ingestion SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "OsIs",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "osis",
      Core.signingName = "osis",
      Core.version = "2022-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "OsIs",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You don\'t have permissions to access the resource.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The client attempted to remove a resource that is currently in use.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request failed because of an unknown error, exception, or failure
-- (the failure is internal to the service).
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | An invalid pagination token provided in the request.
_InvalidPaginationTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | You attempted to create more than the allowed number of tags.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409

-- | You attempted to create a resource that already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | You attempted to access or delete a resource that does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | An exception for missing or invalid input fields.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
