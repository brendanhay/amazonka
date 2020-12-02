{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types
  ( -- * Service Configuration
    sns,

    -- * Errors

    -- * Endpoint
    Endpoint,
    endpoint,
    eAttributes,
    eEndpointARN,

    -- * MessageAttributeValue
    MessageAttributeValue,
    messageAttributeValue,
    mavBinaryValue,
    mavStringValue,
    mavDataType,

    -- * PlatformApplication
    PlatformApplication,
    platformApplication,
    paPlatformApplicationARN,
    paAttributes,

    -- * Subscription
    Subscription,
    subscription,
    sProtocol,
    sOwner,
    sTopicARN,
    sEndpoint,
    sSubscriptionARN,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Topic
    Topic,
    topic,
    tTopicARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SNS.Types.Endpoint
import Network.AWS.SNS.Types.MessageAttributeValue
import Network.AWS.SNS.Types.PlatformApplication
import Network.AWS.SNS.Types.Subscription
import Network.AWS.SNS.Types.Tag
import Network.AWS.SNS.Types.Topic
import Network.AWS.Sign.V4

-- | API version @2010-03-31@ of the Amazon Simple Notification Service SDK configuration.
sns :: Service
sns =
  Service
    { _svcAbbrev = "SNS",
      _svcSigner = v4,
      _svcPrefix = "sns",
      _svcVersion = "2010-03-31",
      _svcEndpoint = defaultEndpoint sns,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "SNS",
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
