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
    cloud9Service,

    -- * Errors

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
    mkEnvironment,
    eArn,
    eLifecycle,
    eOwnerARN,
    eName,
    eId,
    eType,
    eConnectionType,
    eDescription,

    -- * EnvironmentLifecycle
    EnvironmentLifecycle (..),
    mkEnvironmentLifecycle,
    elStatus,
    elFailureResource,
    elReason,

    -- * EnvironmentMember
    EnvironmentMember (..),
    mkEnvironmentMember,
    emLastAccess,
    emUserId,
    emUserARN,
    emPermissions,
    emEnvironmentId,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-09-23@ of the Amazon Cloud9 SDK configuration.
cloud9Service :: Lude.Service
cloud9Service =
  Lude.Service
    { Lude._svcAbbrev = "Cloud9",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloud9",
      Lude._svcVersion = "2017-09-23",
      Lude._svcEndpoint = Lude.defaultEndpoint cloud9Service,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Cloud9",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
