{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Polly.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _LanguageNotSupportedException,
    _LexiconNotFoundException,
    _InvalidS3KeyException,
    _UnsupportedPlsLanguageException,
    _LexiconSizeExceededException,
    _InvalidSampleRateException,
    _InvalidSsmlException,
    _MaxLexemeLengthExceededException,
    _MaxLexiconsNumberExceededException,
    _InvalidSnsTopicArnException,
    _InvalidNextTokenException,
    _EngineNotSupportedException,
    _MarksNotSupportedForFormatException,
    _TextLengthExceededException,
    _InvalidLexiconException,
    _SsmlMarksNotSupportedForTextTypeException,
    _UnsupportedPlsAlphabetException,
    _InvalidTaskIdException,
    _SynthesisTaskNotFoundException,
    _ServiceFailureException,
    _InvalidS3BucketException,

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
    lexiconAttributes_alphabet,
    lexiconAttributes_lexiconArn,
    lexiconAttributes_size,
    lexiconAttributes_languageCode,
    lexiconAttributes_lastModified,
    lexiconAttributes_lexemesCount,

    -- * LexiconDescription
    LexiconDescription (..),
    newLexiconDescription,
    lexiconDescription_name,
    lexiconDescription_attributes,

    -- * SynthesisTask
    SynthesisTask (..),
    newSynthesisTask,
    synthesisTask_voiceId,
    synthesisTask_taskStatusReason,
    synthesisTask_taskId,
    synthesisTask_sampleRate,
    synthesisTask_taskStatus,
    synthesisTask_speechMarkTypes,
    synthesisTask_outputFormat,
    synthesisTask_outputUri,
    synthesisTask_snsTopicArn,
    synthesisTask_languageCode,
    synthesisTask_requestCharacters,
    synthesisTask_lexiconNames,
    synthesisTask_engine,
    synthesisTask_creationTime,
    synthesisTask_textType,

    -- * Voice
    Voice (..),
    newVoice,
    voice_supportedEngines,
    voice_name,
    voice_additionalLanguageCodes,
    voice_id,
    voice_languageCode,
    voice_languageName,
    voice_gender,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Polly.Types.Engine
import Amazonka.Polly.Types.Gender
import Amazonka.Polly.Types.LanguageCode
import Amazonka.Polly.Types.Lexicon
import Amazonka.Polly.Types.LexiconAttributes
import Amazonka.Polly.Types.LexiconDescription
import Amazonka.Polly.Types.OutputFormat
import Amazonka.Polly.Types.SpeechMarkType
import Amazonka.Polly.Types.SynthesisTask
import Amazonka.Polly.Types.TaskStatus
import Amazonka.Polly.Types.TextType
import Amazonka.Polly.Types.Voice
import Amazonka.Polly.Types.VoiceId
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Polly",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "polly",
      Core.signingName = "polly",
      Core.version = "2016-06-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Polly",
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

-- | The language specified is not currently supported by Amazon Polly in
-- this capacity.
_LanguageNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LanguageNotSupportedException =
  Core._MatchServiceError
    defaultService
    "LanguageNotSupportedException"
    Prelude.. Core.hasStatus 400

-- | Amazon Polly can\'t find the specified lexicon. This could be caused by
-- a lexicon that is missing, its name is misspelled or specifying a
-- lexicon that is in a different region.
--
-- Verify that the lexicon exists, is in the region (see ListLexicons) and
-- that you spelled its name is spelled correctly. Then try again.
_LexiconNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LexiconNotFoundException =
  Core._MatchServiceError
    defaultService
    "LexiconNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The provided Amazon S3 key prefix is invalid. Please provide a valid S3
-- object key name.
_InvalidS3KeyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyException =
  Core._MatchServiceError
    defaultService
    "InvalidS3KeyException"
    Prelude.. Core.hasStatus 400

-- | The language specified in the lexicon is unsupported. For a list of
-- supported languages, see
-- <https://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html Lexicon Attributes>.
_UnsupportedPlsLanguageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlsLanguageException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlsLanguageException"
    Prelude.. Core.hasStatus 400

-- | The maximum size of the specified lexicon would be exceeded by this
-- operation.
_LexiconSizeExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LexiconSizeExceededException =
  Core._MatchServiceError
    defaultService
    "LexiconSizeExceededException"
    Prelude.. Core.hasStatus 400

-- | The specified sample rate is not valid.
_InvalidSampleRateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSampleRateException =
  Core._MatchServiceError
    defaultService
    "InvalidSampleRateException"
    Prelude.. Core.hasStatus 400

-- | The SSML you provided is invalid. Verify the SSML syntax, spelling of
-- tags and values, and then try again.
_InvalidSsmlException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSsmlException =
  Core._MatchServiceError
    defaultService
    "InvalidSsmlException"
    Prelude.. Core.hasStatus 400

-- | The maximum size of the lexeme would be exceeded by this operation.
_MaxLexemeLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaxLexemeLengthExceededException =
  Core._MatchServiceError
    defaultService
    "MaxLexemeLengthExceededException"
    Prelude.. Core.hasStatus 400

-- | The maximum number of lexicons would be exceeded by this operation.
_MaxLexiconsNumberExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaxLexiconsNumberExceededException =
  Core._MatchServiceError
    defaultService
    "MaxLexiconsNumberExceededException"
    Prelude.. Core.hasStatus 400

-- | The provided SNS topic ARN is invalid. Please provide a valid SNS topic
-- ARN and try again.
_InvalidSnsTopicArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSnsTopicArnException =
  Core._MatchServiceError
    defaultService
    "InvalidSnsTopicArnException"
    Prelude.. Core.hasStatus 400

-- | The NextToken is invalid. Verify that it\'s spelled correctly, and then
-- try again.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | This engine is not compatible with the voice that you have designated.
-- Choose a new voice that is compatible with the engine or change the
-- engine and restart the operation.
_EngineNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EngineNotSupportedException =
  Core._MatchServiceError
    defaultService
    "EngineNotSupportedException"
    Prelude.. Core.hasStatus 400

-- | Speech marks are not supported for the @OutputFormat@ selected. Speech
-- marks are only available for content in @json@ format.
_MarksNotSupportedForFormatException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MarksNotSupportedForFormatException =
  Core._MatchServiceError
    defaultService
    "MarksNotSupportedForFormatException"
    Prelude.. Core.hasStatus 400

-- | The value of the \"Text\" parameter is longer than the accepted limits.
-- For the @SynthesizeSpeech@ API, the limit for input text is a maximum of
-- 6000 characters total, of which no more than 3000 can be billed
-- characters. For the @StartSpeechSynthesisTask@ API, the maximum is
-- 200,000 characters, of which no more than 100,000 can be billed
-- characters. SSML tags are not counted as billed characters.
_TextLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextLengthExceededException =
  Core._MatchServiceError
    defaultService
    "TextLengthExceededException"
    Prelude.. Core.hasStatus 400

-- | Amazon Polly can\'t find the specified lexicon. Verify that the
-- lexicon\'s name is spelled correctly, and then try again.
_InvalidLexiconException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLexiconException =
  Core._MatchServiceError
    defaultService
    "InvalidLexiconException"
    Prelude.. Core.hasStatus 400

-- | SSML speech marks are not supported for plain text-type input.
_SsmlMarksNotSupportedForTextTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SsmlMarksNotSupportedForTextTypeException =
  Core._MatchServiceError
    defaultService
    "SsmlMarksNotSupportedForTextTypeException"
    Prelude.. Core.hasStatus 400

-- | The alphabet specified by the lexicon is not a supported alphabet. Valid
-- values are @x-sampa@ and @ipa@.
_UnsupportedPlsAlphabetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlsAlphabetException =
  Core._MatchServiceError
    defaultService
    "UnsupportedPlsAlphabetException"
    Prelude.. Core.hasStatus 400

-- | The provided Task ID is not valid. Please provide a valid Task ID and
-- try again.
_InvalidTaskIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTaskIdException =
  Core._MatchServiceError
    defaultService
    "InvalidTaskIdException"
    Prelude.. Core.hasStatus 400

-- | The Speech Synthesis task with requested Task ID cannot be found.
_SynthesisTaskNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SynthesisTaskNotFoundException =
  Core._MatchServiceError
    defaultService
    "SynthesisTaskNotFoundException"
    Prelude.. Core.hasStatus 400

-- | An unknown condition has caused a service failure.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Core.hasStatus 500

-- | The provided Amazon S3 bucket name is invalid. Please check your input
-- with S3 bucket naming requirements and try again.
_InvalidS3BucketException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketException =
  Core._MatchServiceError
    defaultService
    "InvalidS3BucketException"
    Prelude.. Core.hasStatus 400
