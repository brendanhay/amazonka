{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServerlessApplicationRepository.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ConflictException,
    _BadRequestException,
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
    applicationPolicyStatement_principalOrgIDs,
    applicationPolicyStatement_statementId,
    applicationPolicyStatement_principals,
    applicationPolicyStatement_actions,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_spdxLicenseId,
    applicationSummary_homePageUrl,
    applicationSummary_labels,
    applicationSummary_creationTime,
    applicationSummary_description,
    applicationSummary_author,
    applicationSummary_applicationId,
    applicationSummary_name,

    -- * ParameterDefinition
    ParameterDefinition (..),
    newParameterDefinition,
    parameterDefinition_noEcho,
    parameterDefinition_type,
    parameterDefinition_maxLength,
    parameterDefinition_allowedPattern,
    parameterDefinition_defaultValue,
    parameterDefinition_minValue,
    parameterDefinition_description,
    parameterDefinition_minLength,
    parameterDefinition_allowedValues,
    parameterDefinition_maxValue,
    parameterDefinition_constraintDescription,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServerlessApplicationRepository.Types.ApplicationDependencySummary
import Amazonka.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
import Amazonka.ServerlessApplicationRepository.Types.ApplicationSummary
import Amazonka.ServerlessApplicationRepository.Types.Capability
import Amazonka.ServerlessApplicationRepository.Types.ParameterDefinition
import Amazonka.ServerlessApplicationRepository.Types.ParameterValue
import Amazonka.ServerlessApplicationRepository.Types.RollbackConfiguration
import Amazonka.ServerlessApplicationRepository.Types.RollbackTrigger
import Amazonka.ServerlessApplicationRepository.Types.Status
import Amazonka.ServerlessApplicationRepository.Types.Tag
import Amazonka.ServerlessApplicationRepository.Types.Version
import Amazonka.ServerlessApplicationRepository.Types.VersionSummary
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-09-08@ of the Amazon ServerlessApplicationRepository SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "ServerlessApplicationRepository",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "serverlessrepo",
      Core.signingName = "serverlessrepo",
      Core.version = "2017-09-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError
          "ServerlessApplicationRepository",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource (for example, an access policy statement) specified in the
-- request doesn\'t exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The AWS Serverless Application Repository service encountered an
-- internal error.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | The client is not authenticated.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The resource already exists.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | One of the parameters in the request is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The client is sending more than the allowed number of requests per unit
-- of time.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
