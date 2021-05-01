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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-23@ of the Amazon Cloud9 SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Cloud9",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "cloud9",
      Prelude._svcVersion = "2017-09-23",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "Cloud9",
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

-- | The target resource cannot be found.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"

-- | The target request is invalid.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"

-- | A concurrent access issue occurred.
_ConcurrentAccessException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentAccessException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentAccessException"

-- | An internal server error occurred.
_InternalServerErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerErrorException"

-- | An access permissions issue occurred.
_ForbiddenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ForbiddenException =
  Prelude._MatchServiceError
    defaultService
    "ForbiddenException"

-- | A service limit was exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | A conflict occurred.
_ConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConflictException =
  Prelude._MatchServiceError
    defaultService
    "ConflictException"

-- | Too many service requests were made over the given time period.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
