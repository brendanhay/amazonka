{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types
    (
    -- * Service Configuration
      transcribe

    -- * Errors
    , _ConflictException
    , _NotFoundException
    , _InternalFailureException
    , _BadRequestException
    , _LimitExceededException

    -- * LanguageCode
    , LanguageCode (..)

    -- * MediaFormat
    , MediaFormat (..)

    -- * TranscriptionJobStatus
    , TranscriptionJobStatus (..)

    -- * VocabularyState
    , VocabularyState (..)

    -- * Media
    , Media
    , media
    , mMediaFileURI

    -- * Settings
    , Settings
    , settings
    , sVocabularyName
    , sMaxSpeakerLabels
    , sShowSpeakerLabels

    -- * Transcript
    , Transcript
    , transcript
    , tTranscriptFileURI

    -- * TranscriptionJob
    , TranscriptionJob
    , transcriptionJob
    , tjCreationTime
    , tjFailureReason
    , tjLanguageCode
    , tjSettings
    , tjCompletionTime
    , tjMedia
    , tjMediaFormat
    , tjTranscriptionJobStatus
    , tjTranscriptionJobName
    , tjTranscript
    , tjMediaSampleRateHertz

    -- * TranscriptionJobSummary
    , TranscriptionJobSummary
    , transcriptionJobSummary
    , tjsCreationTime
    , tjsFailureReason
    , tjsLanguageCode
    , tjsCompletionTime
    , tjsTranscriptionJobStatus
    , tjsTranscriptionJobName

    -- * VocabularyInfo
    , VocabularyInfo
    , vocabularyInfo
    , viLanguageCode
    , viVocabularyName
    , viLastModifiedTime
    , viVocabularyState
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Transcribe.Types.Product
import Network.AWS.Transcribe.Types.Sum

-- | API version @2017-10-26@ of the Amazon Transcribe Service SDK configuration.
transcribe :: Service
transcribe =
  Service
    { _svcAbbrev = "Transcribe"
    , _svcSigner = v4
    , _svcPrefix = "transcribe"
    , _svcVersion = "2017-10-26"
    , _svcEndpoint = defaultEndpoint transcribe
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Transcribe"
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


-- | The @JobName@ field is a duplicate of a previously entered job name. Resend your request with a different name.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException = _MatchServiceError transcribe "ConflictException"


-- | We can't find the requested transcription job or custom vocabulary. Check the name and try your request again.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError transcribe "NotFoundException"


-- | There was an internal error. Check the error message and try your request again.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError transcribe "InternalFailureException"


-- | Your request didn't pass one or more validation tests. For example, a name already exists when createing a resource or a name may not exist when getting a transcription job or custom vocabulary. See the exception @Message@ field for more information.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException = _MatchServiceError transcribe "BadRequestException"


-- | Either you have sent too many requests or your input file is too long. Wait before you resend your request, or use a smaller file and resend the request.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError transcribe "LimitExceededException"

