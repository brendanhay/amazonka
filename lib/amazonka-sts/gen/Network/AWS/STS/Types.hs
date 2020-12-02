{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types
  ( -- * Service Configuration
    sts,

    -- * Errors

    -- * AssumedRoleUser
    AssumedRoleUser,
    assumedRoleUser,
    aruAssumedRoleId,
    aruARN,

    -- * FederatedUser
    FederatedUser,
    federatedUser,
    fuFederatedUserId,
    fuARN,

    -- * PolicyDescriptorType
    PolicyDescriptorType,
    policyDescriptorType,
    pdtArn,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.STS.Types.AssumedRoleUser
import Network.AWS.STS.Types.FederatedUser
import Network.AWS.STS.Types.PolicyDescriptorType
import Network.AWS.STS.Types.Tag
import Network.AWS.Sign.V4

-- | API version @2011-06-15@ of the Amazon Security Token Service SDK configuration.
sts :: Service
sts =
  Service
    { _svcAbbrev = "STS",
      _svcSigner = v4,
      _svcPrefix = "sts",
      _svcVersion = "2011-06-15",
      _svcEndpoint = defaultEndpoint sts,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "STS",
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
      | has (hasCode "IDPCommunicationError" . hasStatus 400) e =
        Just "idp_unreachable_error"
      | otherwise = Nothing
