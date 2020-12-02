{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types
  ( -- * Service Configuration
    kinesisVideoMedia,

    -- * Errors

    -- * StartSelectorType
    StartSelectorType (..),

    -- * StartSelector
    StartSelector,
    startSelector,
    ssContinuationToken,
    ssAfterFragmentNumber,
    ssStartTimestamp,
    ssStartSelectorType,
  )
where

import Network.AWS.KinesisVideoMedia.Types.StartSelector
import Network.AWS.KinesisVideoMedia.Types.StartSelectorType
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-09-30@ of the Amazon Kinesis Video Streams Media SDK configuration.
kinesisVideoMedia :: Service
kinesisVideoMedia =
  Service
    { _svcAbbrev = "KinesisVideoMedia",
      _svcSigner = v4,
      _svcPrefix = "kinesisvideo",
      _svcVersion = "2017-09-30",
      _svcEndpoint = defaultEndpoint kinesisVideoMedia,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "KinesisVideoMedia",
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
