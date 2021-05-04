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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "ServerlessApplicationRepository",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "serverlessrepo",
      Prelude._svcSigningName = "serverlessrepo",
      Prelude._svcVersion = "2017-09-08",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError
          "ServerlessApplicationRepository",
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

-- | The resource (for example, an access policy statement) specified in the
-- request doesn\'t exist.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | One of the parameters in the request is invalid.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | The AWS Serverless Application Repository service encountered an
-- internal error.
_InternalServerErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Prelude.hasStatus 500

-- | The client is not authenticated.
_ForbiddenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ForbiddenException =
  Prelude._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Prelude.hasStatus 403

-- | The resource already exists.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Prelude.hasStatus 409

-- | The client is sending more than the allowed number of requests per unit
-- of time.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Prelude.hasStatus 429
