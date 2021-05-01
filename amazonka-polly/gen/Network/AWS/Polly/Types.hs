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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Polly",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "polly",
      Prelude._svcSigningName = "polly",
      Prelude._svcVersion = "2016-06-10",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "Polly",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The SSML you provided is invalid. Verify the SSML syntax, spelling of
-- tags and values, and then try again.
_InvalidSsmlException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSsmlException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSsmlException"
    Prelude.. Prelude.hasStatus 400

-- | Amazon Polly can\'t find the specified lexicon. Verify that the
-- lexicon\'s name is spelled correctly, and then try again.
_InvalidLexiconException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLexiconException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLexiconException"
    Prelude.. Prelude.hasStatus 400

-- | The provided Amazon S3 key prefix is invalid. Please provide a valid S3
-- object key name.
_InvalidS3KeyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3KeyException =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3KeyException"
    Prelude.. Prelude.hasStatus 400

-- | Amazon Polly can\'t find the specified lexicon. This could be caused by
-- a lexicon that is missing, its name is misspelled or specifying a
-- lexicon that is in a different region.
--
-- Verify that the lexicon exists, is in the region (see ListLexicons) and
-- that you spelled its name is spelled correctly. Then try again.
_LexiconNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LexiconNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "LexiconNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The maximum size of the lexeme would be exceeded by this operation.
_MaxLexemeLengthExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaxLexemeLengthExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaxLexemeLengthExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The Speech Synthesis task with requested Task ID cannot be found.
_SynthesisTaskNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SynthesisTaskNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "SynthesisTaskNotFoundException"
    Prelude.. Prelude.hasStatus 400

-- | The value of the \"Text\" parameter is longer than the accepted limits.
-- For the @SynthesizeSpeech@ API, the limit for input text is a maximum of
-- 6000 characters total, of which no more than 3000 can be billed
-- characters. For the @StartSpeechSynthesisTask@ API, the maximum is
-- 200,000 characters, of which no more than 100,000 can be billed
-- characters. SSML tags are not counted as billed characters.
_TextLengthExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TextLengthExceededException =
  Prelude._MatchServiceError
    defaultService
    "TextLengthExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The alphabet specified by the lexicon is not a supported alphabet. Valid
-- values are @x-sampa@ and @ipa@.
_UnsupportedPlsAlphabetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedPlsAlphabetException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedPlsAlphabetException"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of lexicons would be exceeded by this operation.
_MaxLexiconsNumberExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaxLexiconsNumberExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaxLexiconsNumberExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The NextToken is invalid. Verify that it\'s spelled correctly, and then
-- try again.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Prelude.hasStatus 400

-- | Speech marks are not supported for the @OutputFormat@ selected. Speech
-- marks are only available for content in @json@ format.
_MarksNotSupportedForFormatException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MarksNotSupportedForFormatException =
  Prelude._MatchServiceError
    defaultService
    "MarksNotSupportedForFormatException"
    Prelude.. Prelude.hasStatus 400

-- | The specified sample rate is not valid.
_InvalidSampleRateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSampleRateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSampleRateException"
    Prelude.. Prelude.hasStatus 400

-- | An unknown condition has caused a service failure.
_ServiceFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceFailureException =
  Prelude._MatchServiceError
    defaultService
    "ServiceFailureException"
    Prelude.. Prelude.hasStatus 500

-- | The language specified in the lexicon is unsupported. For a list of
-- supported languages, see
-- <https://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html Lexicon Attributes>.
_UnsupportedPlsLanguageException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedPlsLanguageException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedPlsLanguageException"
    Prelude.. Prelude.hasStatus 400

-- | The provided SNS topic ARN is invalid. Please provide a valid SNS topic
-- ARN and try again.
_InvalidSnsTopicArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSnsTopicArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSnsTopicArnException"
    Prelude.. Prelude.hasStatus 400

-- | The provided Task ID is not valid. Please provide a valid Task ID and
-- try again.
_InvalidTaskIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTaskIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTaskIdException"
    Prelude.. Prelude.hasStatus 400

-- | The language specified is not currently supported by Amazon Polly in
-- this capacity.
_LanguageNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LanguageNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "LanguageNotSupportedException"
    Prelude.. Prelude.hasStatus 400

-- | The maximum size of the specified lexicon would be exceeded by this
-- operation.
_LexiconSizeExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LexiconSizeExceededException =
  Prelude._MatchServiceError
    defaultService
    "LexiconSizeExceededException"
    Prelude.. Prelude.hasStatus 400

-- | The provided Amazon S3 bucket name is invalid. Please check your input
-- with S3 bucket naming requirements and try again.
_InvalidS3BucketException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidS3BucketException =
  Prelude._MatchServiceError
    defaultService
    "InvalidS3BucketException"
    Prelude.. Prelude.hasStatus 400

-- | SSML speech marks are not supported for plain text-type input.
_SsmlMarksNotSupportedForTextTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SsmlMarksNotSupportedForTextTypeException =
  Prelude._MatchServiceError
    defaultService
    "SsmlMarksNotSupportedForTextTypeException"
    Prelude.. Prelude.hasStatus 400

-- | This engine is not compatible with the voice that you have designated.
-- Choose a new voice that is compatible with the engine or change the
-- engine and restart the operation.
_EngineNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EngineNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "EngineNotSupportedException"
    Prelude.. Prelude.hasStatus 400
