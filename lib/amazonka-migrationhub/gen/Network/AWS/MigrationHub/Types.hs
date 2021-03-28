-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _AccessDeniedException
    , _HomeRegionNotSetException
    , _DryRunOperation
    , _PolicyErrorException
    , _ThrottlingException
    , _InternalServerError
    , _InvalidInputException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _UnauthorizedOperation

    -- * MigrationStatus
    , MigrationStatus (..)

    -- * MigrationTaskSummary
    , MigrationTaskSummary (..)
    , mkMigrationTaskSummary
    , mtsMigrationTaskName
    , mtsProgressPercent
    , mtsProgressUpdateStream
    , mtsStatus
    , mtsStatusDetail
    , mtsUpdateDateTime

    -- * ResourceName
    , ResourceName (..)

    -- * ConfigurationId
    , ConfigurationId (..)

    -- * ApplicationState
    , ApplicationState (..)
    , mkApplicationState
    , asApplicationId
    , asApplicationStatus
    , asLastUpdatedTime

    -- * DiscoveredResource
    , DiscoveredResource (..)
    , mkDiscoveredResource
    , drConfigurationId
    , drDescription

    -- * Token
    , Token (..)

    -- * CreatedArtifact
    , CreatedArtifact (..)
    , mkCreatedArtifact
    , caName
    , caDescription

    -- * ResourceAttribute
    , ResourceAttribute (..)
    , mkResourceAttribute
    , raType
    , raValue

    -- * CreatedArtifactName
    , CreatedArtifactName (..)

    -- * ProgressUpdateStreamSummary
    , ProgressUpdateStreamSummary (..)
    , mkProgressUpdateStreamSummary
    , pussProgressUpdateStreamName

    -- * ApplicationId
    , ApplicationId (..)

    -- * StatusDetail
    , StatusDetail (..)

    -- * Task
    , Task (..)
    , mkTask
    , tStatus
    , tProgressPercent
    , tStatusDetail

    -- * ProgressUpdateStream
    , ProgressUpdateStream (..)

    -- * ApplicationStatus
    , ApplicationStatus (..)

    -- * MigrationTaskName
    , MigrationTaskName (..)

    -- * ResourceAttributeType
    , ResourceAttributeType (..)

    -- * MigrationTask
    , MigrationTask (..)
    , mkMigrationTask
    , mtMigrationTaskName
    , mtProgressUpdateStream
    , mtResourceAttributeList
    , mtTask
    , mtUpdateDateTime

    -- * NextToken
    , NextToken (..)

    -- * ProgressUpdateStreamName
    , ProgressUpdateStreamName (..)

    -- * Description
    , Description (..)

    -- * Name
    , Name (..)

    -- * Value
    , Value (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.MigrationHub.Types.MigrationStatus
  
  
  
import Network.AWS.MigrationHub.Types.MigrationTaskSummary
  
import Network.AWS.MigrationHub.Types.ResourceName
  
import Network.AWS.MigrationHub.Types.ConfigurationId
  
import Network.AWS.MigrationHub.Types.ApplicationState
  
import Network.AWS.MigrationHub.Types.DiscoveredResource
  
import Network.AWS.MigrationHub.Types.Token
  
  
import Network.AWS.MigrationHub.Types.CreatedArtifact
  
import Network.AWS.MigrationHub.Types.ResourceAttribute
  
import Network.AWS.MigrationHub.Types.CreatedArtifactName
  
import Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
  
import Network.AWS.MigrationHub.Types.ApplicationId
  
  
  
  
import Network.AWS.MigrationHub.Types.StatusDetail
  
import Network.AWS.MigrationHub.Types.Task
  
  
  
import Network.AWS.MigrationHub.Types.ProgressUpdateStream
  
import Network.AWS.MigrationHub.Types.ApplicationStatus
  
import Network.AWS.MigrationHub.Types.MigrationTaskName
  
import Network.AWS.MigrationHub.Types.ResourceAttributeType
  
  
  
import Network.AWS.MigrationHub.Types.MigrationTask
  
import Network.AWS.MigrationHub.Types.NextToken
  
import Network.AWS.MigrationHub.Types.ProgressUpdateStreamName
  
import Network.AWS.MigrationHub.Types.Description
  
import Network.AWS.MigrationHub.Types.Name
  
import Network.AWS.MigrationHub.Types.Value
  

-- | API version @2017-05-31@ of the Amazon Migration Hub SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "MigrationHub",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "mgh",
                 Core._svcVersion = "2017-05-31", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "MigrationHub",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The home region is not set. Set the home region to continue.
_HomeRegionNotSetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HomeRegionNotSetException
  = Core._MatchServiceError mkServiceConfig
      "HomeRegionNotSetException"
{-# INLINEABLE _HomeRegionNotSetException #-}
{-# DEPRECATED _HomeRegionNotSetException "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised to indicate a successfully authorized action when the @DryRun@ flag is set to "true".
_DryRunOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DryRunOperation
  = Core._MatchServiceError mkServiceConfig "DryRunOperation"
{-# INLINEABLE _DryRunOperation #-}
{-# DEPRECATED _DryRunOperation "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised when there are problems accessing Application Discovery Service (Application Discovery Service); most likely due to a misconfigured policy or the @migrationhub-discovery@ role is missing or not configured correctly.
_PolicyErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyErrorException
  = Core._MatchServiceError mkServiceConfig "PolicyErrorException"
{-# INLINEABLE _PolicyErrorException #-}
{-# DEPRECATED _PolicyErrorException "Use generic-lens or generic-optics instead"  #-}

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException
  = Core._MatchServiceError mkServiceConfig "ThrottlingException"
{-# INLINEABLE _ThrottlingException #-}
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised when an internal, configuration, or dependency error is encountered.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError
  = Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# INLINEABLE _InternalServerError #-}
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised when the provided input violates a policy constraint or is entered in the wrong format or data type.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised when there is an internal, configuration, or dependency error encountered.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "ServiceUnavailableException"
{-# INLINEABLE _ServiceUnavailableException #-}
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised when the request references a resource (Application Discovery Service configuration, update stream, migration task, etc.) that does not exist in Application Discovery Service (Application Discovery Service) or in Migration Hub's repository.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Exception raised to indicate a request was not authorized when the @DryRun@ flag is set to "true".
_UnauthorizedOperation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperation
  = Core._MatchServiceError mkServiceConfig "UnauthorizedOperation"
{-# INLINEABLE _UnauthorizedOperation #-}
{-# DEPRECATED _UnauthorizedOperation "Use generic-lens or generic-optics instead"  #-}
