{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _ConcurrentAccessException,
    _InternalServerErrorException,
    _ForbiddenException,
    _LimitExceededException,
    _ConflictException,
    _TooManyRequestsException,

    -- * ConnectionType
    ConnectionType (..),

    -- * EnvironmentLifecycleStatus
    EnvironmentLifecycleStatus (..),

    -- * EnvironmentStatus
    EnvironmentStatus (..),

    -- * EnvironmentType
    EnvironmentType (..),

    -- * MemberPermissions
    MemberPermissions (..),

    -- * Permissions
    Permissions (..),

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_lifecycle,
    environment_connectionType,
    environment_id,
    environment_arn,
    environment_name,
    environment_ownerArn,
    environment_description,
    environment_type,

    -- * EnvironmentLifecycle
    EnvironmentLifecycle (..),
    newEnvironmentLifecycle,
    environmentLifecycle_status,
    environmentLifecycle_reason,
    environmentLifecycle_failureResource,

    -- * EnvironmentMember
    EnvironmentMember (..),
    newEnvironmentMember,
    environmentMember_userArn,
    environmentMember_permissions,
    environmentMember_environmentId,
    environmentMember_userId,
    environmentMember_lastAccess,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Network.AWS.Cloud9.Types.ConnectionType
import Network.AWS.Cloud9.Types.Environment
import Network.AWS.Cloud9.Types.EnvironmentLifecycle
import Network.AWS.Cloud9.Types.EnvironmentLifecycleStatus
import Network.AWS.Cloud9.Types.EnvironmentMember
import Network.AWS.Cloud9.Types.EnvironmentStatus
import Network.AWS.Cloud9.Types.EnvironmentType
import Network.AWS.Cloud9.Types.MemberPermissions
import Network.AWS.Cloud9.Types.Permissions
import Network.AWS.Cloud9.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-23@ of the Amazon Cloud9 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Cloud9",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloud9",
      Core._serviceSigningName = "cloud9",
      Core._serviceVersion = "2017-09-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Cloud9",
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

-- | The target resource cannot be found.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | The target request is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | A concurrent access issue occurred.
_ConcurrentAccessException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentAccessException =
  Core._MatchServiceError
    defaultService
    "ConcurrentAccessException"

-- | An internal server error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"

-- | An access permissions issue occurred.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"

-- | A service limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | A conflict occurred.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Too many service requests were made over the given time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
