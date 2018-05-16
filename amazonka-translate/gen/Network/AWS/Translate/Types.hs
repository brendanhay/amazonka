{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types
    (
    -- * Service Configuration
      translate

    -- * Errors
    , _InvalidRequestException
    , _UnsupportedLanguagePairException
    , _DetectedLanguageLowConfidenceException
    , _TooManyRequestsException
    , _InternalServerException
    , _ServiceUnavailableException
    , _TextSizeLimitExceededException
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Translate.Types.Product
import Network.AWS.Translate.Types.Sum

-- | API version @2017-07-01@ of the Amazon Translate SDK configuration.
translate :: Service
translate =
  Service
    { _svcAbbrev = "Translate"
    , _svcSigner = v4
    , _svcPrefix = "translate"
    , _svcVersion = "2017-07-01"
    , _svcEndpoint = defaultEndpoint translate
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Translate"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request is invalid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError translate "InvalidRequestException"


-- | Amazon Translate cannot translate input text in the source language into this target language. For more information, see 'how-to-error-msg' .
--
--
_UnsupportedLanguagePairException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedLanguagePairException =
  _MatchServiceError translate "UnsupportedLanguagePairException"


-- | The confidence that Amazon Comprehend accurately detected the source language is low. If a low confidence level is acceptable for your application, you can use the language in the exception to call Amazon Translate again. For more information, see the <https://docs.aws.amazon.com/comprehend/latest/dg/API_DetectDominantLanguage.html DetectDominantLanguage> operation in the /Amazon Comprehend Developer Guide/ .
--
--
_DetectedLanguageLowConfidenceException :: AsError a => Getting (First ServiceError) a ServiceError
_DetectedLanguageLowConfidenceException =
  _MatchServiceError translate "DetectedLanguageLowConfidenceException"


-- | The number of requests exceeds the limit. Resubmit your request later.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError translate "TooManyRequestsException"


-- | An internal server error occurred. Retry your request.
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException =
  _MatchServiceError translate "InternalServerException"


-- | Amazon Translate is unavailable. Retry your request later.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError translate "ServiceUnavailableException"


-- | The size of the input text exceeds the length constraint for the @Text@ field. Try again with a shorter text.
--
--
_TextSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TextSizeLimitExceededException =
  _MatchServiceError translate "TextSizeLimitExceededException"

