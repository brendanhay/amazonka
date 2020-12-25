-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _NotFoundException,
    _TooManyRequestsException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _UnauthorizedException,
    _BadRequestException,
    _LimitExceededException,
    _AccountActionRequiredException,

    -- * BundleTitle
    BundleTitle (..),

    -- * Feature
    Feature (..),

    -- * Platform
    Platform (..),

    -- * AttributeValue
    AttributeValue (..),

    -- * ProjectDetails
    ProjectDetails (..),
    mkProjectDetails,
    pdConsoleUrl,
    pdCreatedDate,
    pdLastUpdatedDate,
    pdName,
    pdProjectId,
    pdRegion,
    pdResources,
    pdState,

    -- * ResourceType
    ResourceType (..),

    -- * ResourceName
    ResourceName (..),

    -- * ShareUrl
    ShareUrl (..),

    -- * BundleId
    BundleId (..),

    -- * ConsoleUrl
    ConsoleUrl (..),

    -- * NextToken
    NextToken (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * DownloadUrl
    DownloadUrl (..),

    -- * BundleDescription
    BundleDescription (..),

    -- * Resource
    Resource (..),
    mkResource,
    rArn,
    rAttributes,
    rFeature,
    rName,
    rType,

    -- * ProjectState
    ProjectState (..),

    -- * ProjectName
    ProjectName (..),

    -- * ProjectRegion
    ProjectRegion (..),

    -- * ProjectId
    ProjectId (..),

    -- * IconUrl
    IconUrl (..),

    -- * AttributeKey
    AttributeKey (..),

    -- * BundleVersion
    BundleVersion (..),

    -- * BundleDetails
    BundleDetails (..),
    mkBundleDetails,
    bdAvailablePlatforms,
    bdBundleId,
    bdDescription,
    bdIconUrl,
    bdTitle,
    bdVersion,

    -- * SnapshotId
    SnapshotId (..),

    -- * ProjectSummary
    ProjectSummary (..),
    mkProjectSummary,
    psName,
    psProjectId,

    -- * Name
    Name (..),

    -- * Region
    Region (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.AttributeKey
import Network.AWS.Mobile.Types.AttributeValue
import Network.AWS.Mobile.Types.BundleDescription
import Network.AWS.Mobile.Types.BundleDetails
import Network.AWS.Mobile.Types.BundleId
import Network.AWS.Mobile.Types.BundleTitle
import Network.AWS.Mobile.Types.BundleVersion
import Network.AWS.Mobile.Types.ConsoleUrl
import Network.AWS.Mobile.Types.DownloadUrl
import Network.AWS.Mobile.Types.Feature
import Network.AWS.Mobile.Types.IconUrl
import Network.AWS.Mobile.Types.Name
import Network.AWS.Mobile.Types.NextToken
import Network.AWS.Mobile.Types.Platform
import Network.AWS.Mobile.Types.ProjectDetails
import Network.AWS.Mobile.Types.ProjectId
import Network.AWS.Mobile.Types.ProjectName
import Network.AWS.Mobile.Types.ProjectRegion
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.ProjectSummary
import Network.AWS.Mobile.Types.Region
import Network.AWS.Mobile.Types.Resource
import Network.AWS.Mobile.Types.ResourceArn
import Network.AWS.Mobile.Types.ResourceName
import Network.AWS.Mobile.Types.ResourceType
import Network.AWS.Mobile.Types.ShareUrl
import Network.AWS.Mobile.Types.SnapshotId
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-01@ of the Amazon Mobile SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Mobile",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "mobile",
      Core._svcVersion = "2017-07-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Mobile",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | No entity can be found with the specified identifier.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Too many requests have been received for this AWS account in too short a time. The request should be retried after some time delay.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | The service has encountered an unexpected error condition which prevents it from servicing the request.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalFailureException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | The service is temporarily unavailable. The request should be retried after some time delay.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
    Core.. Core.hasStatues 503
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | Credentials of the caller are insufficient to authorize the request.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError mkServiceConfig "UnauthorizedException"
    Core.. Core.hasStatues 401
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead." #-}

-- | The request cannot be processed because some parameter is not valid or the project state prevents the operation from being performed.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}

-- | There are too many AWS Mobile Hub projects in the account or the account has exceeded the maximum number of resources in some AWS service. You should create another sub-account using AWS Organizations or remove some resources and retry your request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | Account Action is required in order to continue the request.
_AccountActionRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountActionRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "AccountActionRequiredException"
    Core.. Core.hasStatues 403
{-# DEPRECATED _AccountActionRequiredException "Use generic-lens or generic-optics instead." #-}
