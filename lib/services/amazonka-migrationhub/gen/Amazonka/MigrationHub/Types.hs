{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHub.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _DryRunOperation,
    _HomeRegionNotSetException,
    _InternalServerError,
    _InvalidInputException,
    _PolicyErrorException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _ThrottlingException,
    _UnauthorizedOperation,

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * MigrationStatus
    MigrationStatus (..),

    -- * ResourceAttributeType
    ResourceAttributeType (..),

    -- * ApplicationState
    ApplicationState (..),
    newApplicationState,
    applicationState_applicationId,
    applicationState_applicationStatus,
    applicationState_lastUpdatedTime,

    -- * CreatedArtifact
    CreatedArtifact (..),
    newCreatedArtifact,
    createdArtifact_description,
    createdArtifact_name,

    -- * DiscoveredResource
    DiscoveredResource (..),
    newDiscoveredResource,
    discoveredResource_description,
    discoveredResource_configurationId,

    -- * MigrationTask
    MigrationTask (..),
    newMigrationTask,
    migrationTask_migrationTaskName,
    migrationTask_progressUpdateStream,
    migrationTask_resourceAttributeList,
    migrationTask_task,
    migrationTask_updateDateTime,

    -- * MigrationTaskSummary
    MigrationTaskSummary (..),
    newMigrationTaskSummary,
    migrationTaskSummary_migrationTaskName,
    migrationTaskSummary_progressPercent,
    migrationTaskSummary_progressUpdateStream,
    migrationTaskSummary_status,
    migrationTaskSummary_statusDetail,
    migrationTaskSummary_updateDateTime,

    -- * ProgressUpdateStreamSummary
    ProgressUpdateStreamSummary (..),
    newProgressUpdateStreamSummary,
    progressUpdateStreamSummary_progressUpdateStreamName,

    -- * ResourceAttribute
    ResourceAttribute (..),
    newResourceAttribute,
    resourceAttribute_type,
    resourceAttribute_value,

    -- * Task
    Task (..),
    newTask,
    task_progressPercent,
    task_statusDetail,
    task_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHub.Types.ApplicationState
import Amazonka.MigrationHub.Types.ApplicationStatus
import Amazonka.MigrationHub.Types.CreatedArtifact
import Amazonka.MigrationHub.Types.DiscoveredResource
import Amazonka.MigrationHub.Types.MigrationStatus
import Amazonka.MigrationHub.Types.MigrationTask
import Amazonka.MigrationHub.Types.MigrationTaskSummary
import Amazonka.MigrationHub.Types.ProgressUpdateStreamSummary
import Amazonka.MigrationHub.Types.ResourceAttribute
import Amazonka.MigrationHub.Types.ResourceAttributeType
import Amazonka.MigrationHub.Types.Task
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-05-31@ of the Amazon Migration Hub SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MigrationHub",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mgh",
      Core.signingName = "mgh",
      Core.version = "2017-05-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MigrationHub",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Exception raised to indicate a successfully authorized action when the
-- @DryRun@ flag is set to \"true\".
_DryRunOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DryRunOperation =
  Core._MatchServiceError
    defaultService
    "DryRunOperation"

-- | The home region is not set. Set the home region to continue.
_HomeRegionNotSetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HomeRegionNotSetException =
  Core._MatchServiceError
    defaultService
    "HomeRegionNotSetException"

-- | Exception raised when an internal, configuration, or dependency error is
-- encountered.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | Exception raised when the provided input violates a policy constraint or
-- is entered in the wrong format or data type.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Exception raised when there are problems accessing Application Discovery
-- Service (Application Discovery Service); most likely due to a
-- misconfigured policy or the @migrationhub-discovery@ role is missing or
-- not configured correctly.
_PolicyErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PolicyErrorException =
  Core._MatchServiceError
    defaultService
    "PolicyErrorException"

-- | Exception raised when the request references a resource (Application
-- Discovery Service configuration, update stream, migration task, etc.)
-- that does not exist in Application Discovery Service (Application
-- Discovery Service) or in Migration Hub\'s repository.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Exception raised when there is an internal, configuration, or dependency
-- error encountered.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Exception raised to indicate a request was not authorized when the
-- @DryRun@ flag is set to \"true\".
_UnauthorizedOperation :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperation =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperation"
