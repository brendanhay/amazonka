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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-05-31@ of the Amazon Migration Hub SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "MigrationHub",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "mgh",
      Prelude._svcSigningName = "mgh",
      Prelude._svcVersion = "2017-05-31",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "MigrationHub",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception raised to indicate a request was not authorized when the
-- @DryRun@ flag is set to \"true\".
_UnauthorizedOperation :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedOperation =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedOperation"

-- | Exception raised when the provided input violates a policy constraint or
-- is entered in the wrong format or data type.
_InvalidInputException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInputException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Exception raised when there is an internal, configuration, or dependency
-- error encountered.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Exception raised when an internal, configuration, or dependency error is
-- encountered.
_InternalServerError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerError =
  Prelude._MatchServiceError
    defaultService
    "InternalServerError"

-- | The home region is not set. Set the home region to continue.
_HomeRegionNotSetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_HomeRegionNotSetException =
  Prelude._MatchServiceError
    defaultService
    "HomeRegionNotSetException"

-- | Exception raised when there are problems accessing Application Discovery
-- Service (Application Discovery Service); most likely due to a
-- misconfigured policy or the @migrationhub-discovery@ role is missing or
-- not configured correctly.
_PolicyErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PolicyErrorException =
  Prelude._MatchServiceError
    defaultService
    "PolicyErrorException"

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Exception raised when the request references a resource (Application
-- Discovery Service configuration, update stream, migration task, etc.)
-- that does not exist in Application Discovery Service (Application
-- Discovery Service) or in Migration Hub\'s repository.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Exception raised to indicate a successfully authorized action when the
-- @DryRun@ flag is set to \"true\".
_DryRunOperation :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DryRunOperation =
  Prelude._MatchServiceError
    defaultService
    "DryRunOperation"
