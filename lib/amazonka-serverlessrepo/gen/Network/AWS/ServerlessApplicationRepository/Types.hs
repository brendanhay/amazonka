-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ConflictException
    , _ForbiddenException
    , _NotFoundException
    , _TooManyRequestsException
    , _InternalServerErrorException
    , _BadRequestException

    -- * Status
    , Status (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tValue
    , tKey

    -- * ParameterValue
    , ParameterValue (..)
    , mkParameterValue
    , pvValue
    , pvName

    -- * VersionSummary
    , VersionSummary (..)
    , mkVersionSummary
    , vsCreationTime
    , vsApplicationId
    , vsSemanticVersion
    , vsSourceCodeUrl

    -- * ApplicationDependencySummary
    , ApplicationDependencySummary (..)
    , mkApplicationDependencySummary
    , adsApplicationId
    , adsSemanticVersion

    -- * ParameterDefinition
    , ParameterDefinition (..)
    , mkParameterDefinition
    , pdReferencedByResources
    , pdName
    , pdAllowedPattern
    , pdAllowedValues
    , pdConstraintDescription
    , pdDefaultValue
    , pdDescription
    , pdMaxLength
    , pdMaxValue
    , pdMinLength
    , pdMinValue
    , pdNoEcho
    , pdType

    -- * ApplicationPolicyStatement
    , ApplicationPolicyStatement (..)
    , mkApplicationPolicyStatement
    , apsPrincipals
    , apsActions
    , apsPrincipalOrgIDs
    , apsStatementId

    -- * Version
    , Version (..)
    , mkVersion
    , vTemplateUrl
    , vParameterDefinitions
    , vResourcesSupported
    , vCreationTime
    , vRequiredCapabilities
    , vApplicationId
    , vSemanticVersion
    , vSourceCodeArchiveUrl
    , vSourceCodeUrl

    -- * Capability
    , Capability (..)

    -- * ApplicationSummary
    , ApplicationSummary (..)
    , mkApplicationSummary
    , asDescription
    , asAuthor
    , asApplicationId
    , asName
    , asCreationTime
    , asHomePageUrl
    , asLabels
    , asSpdxLicenseId

    -- * RollbackConfiguration
    , RollbackConfiguration (..)
    , mkRollbackConfiguration
    , rcMonitoringTimeInMinutes
    , rcRollbackTriggers

    -- * RollbackTrigger
    , RollbackTrigger (..)
    , mkRollbackTrigger
    , rtType
    , rtArn
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ServerlessApplicationRepository.Types.Status
  
import Network.AWS.ServerlessApplicationRepository.Types.Tag
  
import Network.AWS.ServerlessApplicationRepository.Types.ParameterValue
  
import Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
  
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
  
  
  
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition
  
  
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
  
  
import Network.AWS.ServerlessApplicationRepository.Types.Version
  
  
import Network.AWS.ServerlessApplicationRepository.Types.Capability
  
import Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
  
import Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
  
import Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger
  
  

-- | API version @2017-09-08@ of the Amazon ServerlessApplicationRepository SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ServerlessApplicationRepository",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "serverlessrepo",
                 Core._svcVersion = "2017-09-08", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError =
                   Core.parseJSONError "ServerlessApplicationRepository",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The resource already exists.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The client is not authenticated.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException
  = Core._MatchServiceError mkServiceConfig "ForbiddenException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _ForbiddenException #-}
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead"  #-}

-- | The resource (for example, an access policy statement) specified in the request doesn't exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The client is sending more than the allowed number of requests per unit of time.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | The AWS Serverless Application Repository service encountered an internal error.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException
  = Core._MatchServiceError mkServiceConfig
      "InternalServerErrorException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServerErrorException #-}
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead"  #-}

-- | One of the parameters in the request is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException
  = Core._MatchServiceError mkServiceConfig "BadRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _BadRequestException #-}
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead"  #-}
