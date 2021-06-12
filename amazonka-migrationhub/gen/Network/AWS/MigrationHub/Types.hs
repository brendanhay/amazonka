{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnauthorizedOperation,
    _InvalidInputException,
    _ServiceUnavailableException,
    _ThrottlingException,
    _InternalServerError,
    _HomeRegionNotSetException,
    _PolicyErrorException,
    _AccessDeniedException,
    _ResourceNotFoundException,
    _DryRunOperation,

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
    migrationTask_resourceAttributeList,
    migrationTask_updateDateTime,
    migrationTask_task,
    migrationTask_migrationTaskName,
    migrationTask_progressUpdateStream,

    -- * MigrationTaskSummary
    MigrationTaskSummary (..),
    newMigrationTaskSummary,
    migrationTaskSummary_status,
    migrationTaskSummary_progressPercent,
    migrationTaskSummary_updateDateTime,
    migrationTaskSummary_statusDetail,
    migrationTaskSummary_migrationTaskName,
    migrationTaskSummary_progressUpdateStream,

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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ApplicationState
import Network.AWS.MigrationHub.Types.ApplicationStatus
import Network.AWS.MigrationHub.Types.CreatedArtifact
import Network.AWS.MigrationHub.Types.DiscoveredResource
import Network.AWS.MigrationHub.Types.MigrationStatus
import Network.AWS.MigrationHub.Types.MigrationTask
import Network.AWS.MigrationHub.Types.MigrationTaskSummary
import Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
import Network.AWS.MigrationHub.Types.ResourceAttribute
import Network.AWS.MigrationHub.Types.ResourceAttributeType
import Network.AWS.MigrationHub.Types.Task
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-05-31@ of the Amazon Migration Hub SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MigrationHub",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mgh",
      Core._serviceSigningName = "mgh",
      Core._serviceVersion = "2017-05-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MigrationHub",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Exception raised to indicate a request was not authorized when the
-- @DryRun@ flag is set to \"true\".
_UnauthorizedOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperation =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperation"

-- | Exception raised when the provided input violates a policy constraint or
-- is entered in the wrong format or data type.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Exception raised when there is an internal, configuration, or dependency
-- error encountered.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Exception raised when an internal, configuration, or dependency error is
-- encountered.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | The home region is not set. Set the home region to continue.
_HomeRegionNotSetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HomeRegionNotSetException =
  Core._MatchServiceError
    defaultService
    "HomeRegionNotSetException"

-- | Exception raised when there are problems accessing Application Discovery
-- Service (Application Discovery Service); most likely due to a
-- misconfigured policy or the @migrationhub-discovery@ role is missing or
-- not configured correctly.
_PolicyErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyErrorException =
  Core._MatchServiceError
    defaultService
    "PolicyErrorException"

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Exception raised when the request references a resource (Application
-- Discovery Service configuration, update stream, migration task, etc.)
-- that does not exist in Application Discovery Service (Application
-- Discovery Service) or in Migration Hub\'s repository.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Exception raised to indicate a successfully authorized action when the
-- @DryRun@ flag is set to \"true\".
_DryRunOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DryRunOperation =
  Core._MatchServiceError
    defaultService
    "DryRunOperation"
