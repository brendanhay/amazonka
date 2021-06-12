{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidSsmlException,
    _InvalidLexiconException,
    _InvalidS3KeyException,
    _LexiconNotFoundException,
    _MaxLexemeLengthExceededException,
    _SynthesisTaskNotFoundException,
    _TextLengthExceededException,
    _UnsupportedPlsAlphabetException,
    _MaxLexiconsNumberExceededException,
    _InvalidNextTokenException,
    _MarksNotSupportedForFormatException,
    _InvalidSampleRateException,
    _ServiceFailureException,
    _UnsupportedPlsLanguageException,
    _InvalidSnsTopicArnException,
    _InvalidTaskIdException,
    _LanguageNotSupportedException,
    _LexiconSizeExceededException,
    _InvalidS3BucketException,
    _SsmlMarksNotSupportedForTextTypeException,
    _EngineNotSupportedException,

    -- * Engine
    Engine (..),

    -- * Gender
    Gender (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * OutputFormat
    OutputFormat (..),

    -- * SpeechMarkType
    SpeechMarkType (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * TextType
    TextType (..),

    -- * VoiceId
    VoiceId (..),

    -- * Lexicon
    Lexicon (..),
    newLexicon,
    lexicon_name,
    lexicon_content,

    -- * LexiconAttributes
    LexiconAttributes (..),
    newLexiconAttributes,
    lexiconAttributes_languageCode,
    lexiconAttributes_lexiconArn,
    lexiconAttributes_alphabet,
    lexiconAttributes_lexemesCount,
    lexiconAttributes_lastModified,
    lexiconAttributes_size,

    -- * LexiconDescription
    LexiconDescription (..),
    newLexiconDescription,
    lexiconDescription_attributes,
    lexiconDescription_name,

    -- * SynthesisTask
    SynthesisTask (..),
    newSynthesisTask,
    synthesisTask_languageCode,
    synthesisTask_creationTime,
    synthesisTask_outputUri,
    synthesisTask_speechMarkTypes,
    synthesisTask_lexiconNames,
    synthesisTask_voiceId,
    synthesisTask_taskId,
    synthesisTask_textType,
    synthesisTask_outputFormat,
    synthesisTask_sampleRate,
    synthesisTask_taskStatus,
    synthesisTask_engine,
    synthesisTask_requestCharacters,
    synthesisTask_taskStatusReason,
    synthesisTask_snsTopicArn,

    -- * Voice
    Voice (..),
    newVoice,
    voice_languageCode,
    voice_id,
    voice_gender,
    voice_name,
    voice_supportedEngines,
    voice_additionalLanguageCodes,
    voice_languageName,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.Gender
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.Lexicon
import Network.AWS.Polly.Types.LexiconAttributes
import Network.AWS.Polly.Types.LexiconDescription
import Network.AWS.Polly.Types.OutputFormat
import Network.AWS.Polly.Types.SpeechMarkType
import Network.AWS.Polly.Types.SynthesisTask
import Network.AWS.Polly.Types.TaskStatus
import Network.AWS.Polly.Types.TextType
import Network.AWS.Polly.Types.Voice
import Network.AWS.Polly.Types.VoiceId
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Polly",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "polly",
      Core._serviceSigningName = "polly",
      Core._serviceVersion = "2016-06-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Polly",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The SSML you provided is invalid. Verify the SSML syntax, spelling of
-- tags and values, and then try again.
_InvalidSsmlException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSsmlException =
  Core._MatchServiceError
    defaultService
    "InvalidSsmlException"
    Core.. Core.hasStatus 400

-- | Amazon Polly can\'t find the specified lexicon. Verify that the
-- lexicon\'s name is spelled correctly, and then try again.
_InvalidLexiconException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLexiconException =
  Core._MatchServiceError
    defaultService
    "InvalidLexiconException"
    Core.. Core.hasStatus 400

-- | The provided Amazon S3 key prefix is invalid. Please provide a valid S3
-- object key name.
_InvalidS3KeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyException =
  Core._MatchServiceError
    defaultService
    "InvalidS3KeyException"
    Core.. Core.hasStatus 400

-- | Amazon Polly can\'t find the specified lexicon. This could be caused by
-- a lexicon that is missing, its name is misspelled or specifying a
-- lexicon that is in a different region.
--
-- Verify that the lexicon exists, is in the region (see ListLexicons) and
-- that you spelled its name is spelled correctly. Then try again.
_LexiconNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LexiconNotFoundException =
  Core._MatchServiceError
    defaultService
    "LexiconNotFoundException"
    Core.. Core.hasStatus 404

-- | The maximum size of the lexeme would be exceeded by this operation.
_MaxLexemeLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxLexemeLengthExceededException =
  Core._MatchServiceError
    defaultService
    "MaxLexemeLengthExceededException"
    Core.. Core.hasStatus 400

-- | The Speech Synthesis task with requested Task ID cannot be found.
_SynthesisTaskNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SynthesisTaskNotFoundException =
  Core._MatchServiceError
    defaultService
    "SynthesisTaskNotFoundException"
    Core.. Core.hasStatus 400

-- | The value of the \"Text\" parameter is longer than the accepted limits.
-- For the @SynthesizeSpeech@ API, the limit for input text is a maximum of
-- 6000 characters total, of which no more than 3000 can be billed
-- characters. For the @StartSpeechSynthesisTask@ API, the maximum is
-- 200,000 characters, of which no more than 100,000 can be billed
-- characters. SSML tags are not counted as billed characters.
_TextLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TextLengthExceededException =
  Core._MatchServiceError
    defaultService
    "TextLengthExceededException"
    Core.. Core.hasStatus 400

-- | The alphabet specified by the lexicon is not a supported alphabet. Valid
-- values are @x-sampa@ and @ipa@.
_UnsupportedPlsAlphabetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlsAlphabetException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlsAlphabetException"
    Core.. Core.hasStatus 400

-- | The maximum number of lexicons would be exceeded by this operation.
_MaxLexiconsNumberExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxLexiconsNumberExceededException =
  Core._MatchServiceError
    defaultService
    "MaxLexiconsNumberExceededException"
    Core.. Core.hasStatus 400

-- | The NextToken is invalid. Verify that it\'s spelled correctly, and then
-- try again.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Core.. Core.hasStatus 400

-- | Speech marks are not supported for the @OutputFormat@ selected. Speech
-- marks are only available for content in @json@ format.
_MarksNotSupportedForFormatException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MarksNotSupportedForFormatException =
  Core._MatchServiceError
    defaultService
    "MarksNotSupportedForFormatException"
    Core.. Core.hasStatus 400

-- | The specified sample rate is not valid.
_InvalidSampleRateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSampleRateException =
  Core._MatchServiceError
    defaultService
    "InvalidSampleRateException"
    Core.. Core.hasStatus 400

-- | An unknown condition has caused a service failure.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Core.. Core.hasStatus 500

-- | The language specified in the lexicon is unsupported. For a list of
-- supported languages, see
-- <https://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html Lexicon Attributes>.
_UnsupportedPlsLanguageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlsLanguageException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlsLanguageException"
    Core.. Core.hasStatus 400

-- | The provided SNS topic ARN is invalid. Please provide a valid SNS topic
-- ARN and try again.
_InvalidSnsTopicArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSnsTopicArnException =
  Core._MatchServiceError
    defaultService
    "InvalidSnsTopicArnException"
    Core.. Core.hasStatus 400

-- | The provided Task ID is not valid. Please provide a valid Task ID and
-- try again.
_InvalidTaskIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTaskIdException =
  Core._MatchServiceError
    defaultService
    "InvalidTaskIdException"
    Core.. Core.hasStatus 400

-- | The language specified is not currently supported by Amazon Polly in
-- this capacity.
_LanguageNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LanguageNotSupportedException =
  Core._MatchServiceError
    defaultService
    "LanguageNotSupportedException"
    Core.. Core.hasStatus 400

-- | The maximum size of the specified lexicon would be exceeded by this
-- operation.
_LexiconSizeExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LexiconSizeExceededException =
  Core._MatchServiceError
    defaultService
    "LexiconSizeExceededException"
    Core.. Core.hasStatus 400

-- | The provided Amazon S3 bucket name is invalid. Please check your input
-- with S3 bucket naming requirements and try again.
_InvalidS3BucketException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketException =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketException"
    Core.. Core.hasStatus 400

-- | SSML speech marks are not supported for plain text-type input.
_SsmlMarksNotSupportedForTextTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SsmlMarksNotSupportedForTextTypeException =
  Core._MatchServiceError
    defaultService
    "SsmlMarksNotSupportedForTextTypeException"
    Core.. Core.hasStatus 400

-- | This engine is not compatible with the voice that you have designated.
-- Choose a new voice that is compatible with the engine or change the
-- engine and restart the operation.
_EngineNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EngineNotSupportedException =
  Core._MatchServiceError
    defaultService
    "EngineNotSupportedException"
    Core.. Core.hasStatus 400
