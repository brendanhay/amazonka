{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ConflictException,
    _TooManyRequestsException,

    -- * Capability
    Capability (..),

    -- * Status
    Status (..),

    -- * ApplicationDependencySummary
    ApplicationDependencySummary (..),
    newApplicationDependencySummary,
    applicationDependencySummary_applicationId,
    applicationDependencySummary_semanticVersion,

    -- * ApplicationPolicyStatement
    ApplicationPolicyStatement (..),
    newApplicationPolicyStatement,
    applicationPolicyStatement_statementId,
    applicationPolicyStatement_principalOrgIDs,
    applicationPolicyStatement_principals,
    applicationPolicyStatement_actions,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_creationTime,
    applicationSummary_spdxLicenseId,
    applicationSummary_labels,
    applicationSummary_homePageUrl,
    applicationSummary_description,
    applicationSummary_author,
    applicationSummary_applicationId,
    applicationSummary_name,

    -- * ParameterDefinition
    ParameterDefinition (..),
    newParameterDefinition,
    parameterDefinition_maxValue,
    parameterDefinition_minLength,
    parameterDefinition_allowedValues,
    parameterDefinition_minValue,
    parameterDefinition_description,
    parameterDefinition_constraintDescription,
    parameterDefinition_type,
    parameterDefinition_noEcho,
    parameterDefinition_maxLength,
    parameterDefinition_allowedPattern,
    parameterDefinition_defaultValue,
    parameterDefinition_referencedByResources,
    parameterDefinition_name,

    -- * ParameterValue
    ParameterValue (..),
    newParameterValue,
    parameterValue_value,
    parameterValue_name,

    -- * RollbackConfiguration
    RollbackConfiguration (..),
    newRollbackConfiguration,
    rollbackConfiguration_monitoringTimeInMinutes,
    rollbackConfiguration_rollbackTriggers,

    -- * RollbackTrigger
    RollbackTrigger (..),
    newRollbackTrigger,
    rollbackTrigger_type,
    rollbackTrigger_arn,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * Version
    Version (..),
    newVersion,
    version_sourceCodeArchiveUrl,
    version_sourceCodeUrl,
    version_templateUrl,
    version_parameterDefinitions,
    version_resourcesSupported,
    version_creationTime,
    version_requiredCapabilities,
    version_applicationId,
    version_semanticVersion,

    -- * VersionSummary
    VersionSummary (..),
    newVersionSummary,
    versionSummary_sourceCodeUrl,
    versionSummary_creationTime,
    versionSummary_applicationId,
    versionSummary_semanticVersion,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
import Network.AWS.ServerlessApplicationRepository.Types.Capability
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
import Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
import Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
import Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
import Network.AWS.ServerlessApplicationRepository.Types.Status
import Network.AWS.ServerlessApplicationRepository.Types.Tag
import Network.AWS.ServerlessApplicationRepository.Types.Version
import Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-08@ of the Amazon ServerlessApplicationRepository SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ServerlessApplicationRepository",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "serverlessrepo",
      Core._serviceSigningName = "serverlessrepo",
      Core._serviceVersion = "2017-09-08",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError
          "ServerlessApplicationRepository",
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

-- | The resource (for example, an access policy statement) specified in the
-- request doesn\'t exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Core.. Core.hasStatus 404

-- | One of the parameters in the request is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Core.. Core.hasStatus 400

-- | The AWS Serverless Application Repository service encountered an
-- internal error.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Core.. Core.hasStatus 500

-- | The client is not authenticated.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Core.. Core.hasStatus 403

-- | The resource already exists.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Core.. Core.hasStatus 409

-- | The client is sending more than the allowed number of requests per unit
-- of time.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Core.. Core.hasStatus 429
