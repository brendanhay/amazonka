-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _ConcurrentAccessException,
    _BadRequestException,
    _LimitExceededException,

    -- * EnvironmentLifecycleStatus
    EnvironmentLifecycleStatus (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Environment
    Environment (..),
    mkEnvironment,
    eArn,
    eConnectionType,
    eDescription,
    eId,
    eLifecycle,
    eName,
    eOwnerArn,
    eType,

    -- * String
    String (..),

    -- * MemberPermissions
    MemberPermissions (..),

    -- * EnvironmentLifecycle
    EnvironmentLifecycle (..),
    mkEnvironmentLifecycle,
    elFailureResource,
    elReason,
    elStatus,

    -- * SubnetId
    SubnetId (..),

    -- * InstanceType
    InstanceType (..),

    -- * EnvironmentStatus
    EnvironmentStatus (..),

    -- * UserArn
    UserArn (..),

    -- * EnvironmentName
    EnvironmentName (..),

    -- * EnvironmentType
    EnvironmentType (..),

    -- * EnvironmentMember
    EnvironmentMember (..),
    mkEnvironmentMember,
    emEnvironmentId,
    emLastAccess,
    emPermissions,
    emUserArn,
    emUserId,

    -- * TagKey
    TagKey (..),

    -- * EnvironmentArn
    EnvironmentArn (..),

    -- * EnvironmentId
    EnvironmentId (..),

    -- * Permissions
    Permissions (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * ConnectionType
    ConnectionType (..),

    -- * Name
    Name (..),

    -- * Description
    Description (..),

    -- * OwnerArn
    OwnerArn (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * Message
    Message (..),

    -- * Arn
    Arn (..),

    -- * Id
    Id (..),

    -- * ResourceARN
    ResourceARN (..),
  )
where

import Network.AWS.Cloud9.Types.Arn
import Network.AWS.Cloud9.Types.ClientRequestToken
import Network.AWS.Cloud9.Types.ConnectionType
import Network.AWS.Cloud9.Types.Description
import Network.AWS.Cloud9.Types.Environment
import Network.AWS.Cloud9.Types.EnvironmentArn
import Network.AWS.Cloud9.Types.EnvironmentId
import Network.AWS.Cloud9.Types.EnvironmentLifecycle
import Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
import Network.AWS.Cloud9.Types.EnvironmentMember
import Network.AWS.Cloud9.Types.EnvironmentName
import Network.AWS.Cloud9.Types.EnvironmentStatus
import Network.AWS.Cloud9.Types.EnvironmentType
import Network.AWS.Cloud9.Types.Id
import Network.AWS.Cloud9.Types.InstanceType
import Network.AWS.Cloud9.Types.Key
import Network.AWS.Cloud9.Types.MemberPermissions
import Network.AWS.Cloud9.Types.Message
import Network.AWS.Cloud9.Types.Name
import Network.AWS.Cloud9.Types.OwnerArn
import Network.AWS.Cloud9.Types.Permissions
import Network.AWS.Cloud9.Types.ResourceARN
import Network.AWS.Cloud9.Types.String
import Network.AWS.Cloud9.Types.SubnetId
import Network.AWS.Cloud9.Types.Tag
import Network.AWS.Cloud9.Types.TagKey
import Network.AWS.Cloud9.Types.UserArn
import Network.AWS.Cloud9.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-23@ of the Amazon Cloud9 SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Cloud9",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cloud9",
      Core._svcVersion = "2017-09-23",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Cloud9",
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

-- | A conflict occurred.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | An access permissions issue occurred.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError mkServiceConfig "ForbiddenException"
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead." #-}

-- | The target resource cannot be found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | Too many service requests were made over the given time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | An internal server error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | A concurrent access issue occurred.
_ConcurrentAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentAccessException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentAccessException"
{-# DEPRECATED _ConcurrentAccessException "Use generic-lens or generic-optics instead." #-}

-- | The target request is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}

-- | A service limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
