{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Cloud9.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _InternalServerErrorException,
    _ConcurrentAccessException,
    _LimitExceededException,
    _ForbiddenException,
    _ConflictException,
    _BadRequestException,
    _TooManyRequestsException,

    -- * ConnectionType
    ConnectionType (..),

    -- * EnvironmentLifecycleStatus
    EnvironmentLifecycleStatus (..),

    -- * EnvironmentStatus
    EnvironmentStatus (..),

    -- * EnvironmentType
    EnvironmentType (..),

    -- * ManagedCredentialsAction
    ManagedCredentialsAction (..),

    -- * ManagedCredentialsStatus
    ManagedCredentialsStatus (..),

    -- * MemberPermissions
    MemberPermissions (..),

    -- * Permissions
    Permissions (..),

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_name,
    environment_lifecycle,
    environment_connectionType,
    environment_managedCredentialsStatus,
    environment_description,
    environment_id,
    environment_type,
    environment_arn,
    environment_ownerArn,

    -- * EnvironmentLifecycle
    EnvironmentLifecycle (..),
    newEnvironmentLifecycle,
    environmentLifecycle_status,
    environmentLifecycle_reason,
    environmentLifecycle_failureResource,

    -- * EnvironmentMember
    EnvironmentMember (..),
    newEnvironmentMember,
    environmentMember_lastAccess,
    environmentMember_permissions,
    environmentMember_userId,
    environmentMember_userArn,
    environmentMember_environmentId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Amazonka.Cloud9.Types.ConnectionType
import Amazonka.Cloud9.Types.Environment
import Amazonka.Cloud9.Types.EnvironmentLifecycle
import Amazonka.Cloud9.Types.EnvironmentLifecycleStatus
import Amazonka.Cloud9.Types.EnvironmentMember
import Amazonka.Cloud9.Types.EnvironmentStatus
import Amazonka.Cloud9.Types.EnvironmentType
import Amazonka.Cloud9.Types.ManagedCredentialsAction
import Amazonka.Cloud9.Types.ManagedCredentialsStatus
import Amazonka.Cloud9.Types.MemberPermissions
import Amazonka.Cloud9.Types.Permissions
import Amazonka.Cloud9.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-09-23@ of the Amazon Cloud9 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Cloud9",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloud9",
      Core.signingName = "cloud9",
      Core.version = "2017-09-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Cloud9",
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

-- | The target resource cannot be found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | An internal server error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"

-- | A concurrent access issue occurred.
_ConcurrentAccessException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentAccessException =
  Core._MatchServiceError
    defaultService
    "ConcurrentAccessException"

-- | A service limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | An access permissions issue occurred.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"

-- | A conflict occurred.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The target request is invalid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | Too many service requests were made over the given time period.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
