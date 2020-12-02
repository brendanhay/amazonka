{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types
  ( -- * Service Configuration
    cloud9,

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
    Environment,
    environment,
    eArn,
    eLifecycle,
    eOwnerARN,
    eName,
    eId,
    eType,
    eConnectionType,
    eDescription,

    -- * EnvironmentLifecycle
    EnvironmentLifecycle,
    environmentLifecycle,
    elStatus,
    elFailureResource,
    elReason,

    -- * EnvironmentMember
    EnvironmentMember,
    environmentMember,
    emLastAccess,
    emUserId,
    emUserARN,
    emPermissions,
    emEnvironmentId,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-23@ of the Amazon Cloud9 SDK configuration.
cloud9 :: Service
cloud9 =
  Service
    { _svcAbbrev = "Cloud9",
      _svcSigner = v4,
      _svcPrefix = "cloud9",
      _svcVersion = "2017-09-23",
      _svcEndpoint = defaultEndpoint cloud9,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Cloud9",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
