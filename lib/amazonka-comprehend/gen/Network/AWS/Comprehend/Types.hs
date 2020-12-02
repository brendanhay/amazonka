{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types
    (
    -- * Service Configuration
      comprehend

    -- * Errors
    , _InvalidRequestException
    , _TooManyRequestsException
    , _InternalServerException
    , _BatchSizeLimitExceededException
    , _UnsupportedLanguageException
    , _JobNotFoundException
    , _InvalidFilterException
    , _TextSizeLimitExceededException

    -- * EntityType
    , EntityType (..)

    -- * InputFormat
    , InputFormat (..)

    -- * JobStatus
    , JobStatus (..)

    -- * LanguageCode
    , LanguageCode (..)

    -- * SentimentType
    , SentimentType (..)

    -- * BatchDetectDominantLanguageItemResult
    , BatchDetectDominantLanguageItemResult
    , batchDetectDominantLanguageItemResult
    , bddlirLanguages
    , bddlirIndex

    -- * BatchDetectEntitiesItemResult
    , BatchDetectEntitiesItemResult
    , batchDetectEntitiesItemResult
    , bdeirEntities
    , bdeirIndex

    -- * BatchDetectKeyPhrasesItemResult
    , BatchDetectKeyPhrasesItemResult
    , batchDetectKeyPhrasesItemResult
    , bdkpirIndex
    , bdkpirKeyPhrases

    -- * BatchDetectSentimentItemResult
    , BatchDetectSentimentItemResult
    , batchDetectSentimentItemResult
    , bdsirSentiment
    , bdsirSentimentScore
    , bdsirIndex

    -- * BatchItemError
    , BatchItemError
    , batchItemError
    , bieErrorCode
    , bieErrorMessage
    , bieIndex

    -- * DominantLanguage
    , DominantLanguage
    , dominantLanguage
    , dlLanguageCode
    , dlScore

    -- * Entity
    , Entity
    , entity
    , eBeginOffset
    , eText
    , eScore
    , eEndOffset
    , eType

    -- * InputDataConfig
    , InputDataConfig
    , inputDataConfig
    , idcInputFormat
    , idcS3URI

    -- * KeyPhrase
    , KeyPhrase
    , keyPhrase
    , kpBeginOffset
    , kpText
    , kpScore
    , kpEndOffset

    -- * OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcS3URI

    -- * SentimentScore
    , SentimentScore
    , sentimentScore
    , ssMixed
    , ssNegative
    , ssNeutral
    , ssPositive

    -- * TopicsDetectionJobFilter
    , TopicsDetectionJobFilter
    , topicsDetectionJobFilter
    , tdjfSubmitTimeAfter
    , tdjfSubmitTimeBefore
    , tdjfJobName
    , tdjfJobStatus

    -- * TopicsDetectionJobProperties
    , TopicsDetectionJobProperties
    , topicsDetectionJobProperties
    , tdjpJobId
    , tdjpJobName
    , tdjpInputDataConfig
    , tdjpEndTime
    , tdjpOutputDataConfig
    , tdjpNumberOfTopics
    , tdjpJobStatus
    , tdjpMessage
    , tdjpSubmitTime
    ) where

import Network.AWS.Comprehend.Types.Product
import Network.AWS.Comprehend.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-27@ of the Amazon Comprehend SDK configuration.
comprehend :: Service
comprehend =
  Service
    { _svcAbbrev = "Comprehend"
    , _svcSigner = v4
    , _svcPrefix = "comprehend"
    , _svcVersion = "2017-11-27"
    , _svcEndpoint = defaultEndpoint comprehend
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Comprehend"
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
  _MatchServiceError comprehend "InvalidRequestException"


-- | The number of requests exceeds the limit. Resubmit your request later.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException =
  _MatchServiceError comprehend "TooManyRequestsException"


-- | An internal server error occurred. Retry your request.
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException =
  _MatchServiceError comprehend "InternalServerException"


-- | The number of documents in the request exceeds the limit of 25. Try your request again with fewer documents.
--
--
_BatchSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_BatchSizeLimitExceededException =
  _MatchServiceError comprehend "BatchSizeLimitExceededException"


-- | Amazon Comprehend can't process the language of the input text. For all APIs except @DetectDominantLanguage@ , Amazon Comprehend accepts only English or Spanish text. For the @DetectDominantLanguage@ API, Amazon Comprehend detects 100 languages. For a list of languages, see 'how-languages'
--
--
_UnsupportedLanguageException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedLanguageException =
  _MatchServiceError comprehend "UnsupportedLanguageException"


-- | The specified job was not found. Check the job ID and try again.
--
--
_JobNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_JobNotFoundException = _MatchServiceError comprehend "JobNotFoundException"


-- | The filter specified for the @ListTopicDetectionJobs@ operation is invalid. Specify a different filter.
--
--
_InvalidFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilterException = _MatchServiceError comprehend "InvalidFilterException"


-- | The size of the input text exceeds the limit. Use a smaller document.
--
--
_TextSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TextSizeLimitExceededException =
  _MatchServiceError comprehend "TextSizeLimitExceededException"

