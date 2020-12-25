-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidSnsTopicArnException,
    _UnsupportedPlsLanguageException,
    _InvalidSsmlException,
    _InvalidSampleRateException,
    _EngineNotSupportedException,
    _MaxLexiconsNumberExceededException,
    _TextLengthExceededException,
    _MaxLexemeLengthExceededException,
    _InvalidTaskIdException,
    _InvalidLexiconException,
    _ServiceFailureException,
    _UnsupportedPlsAlphabetException,
    _InvalidNextTokenException,
    _MarksNotSupportedForFormatException,
    _SynthesisTaskNotFoundException,
    _SsmlMarksNotSupportedForTextTypeException,
    _InvalidS3BucketException,
    _LexiconSizeExceededException,
    _LanguageNotSupportedException,
    _LexiconNotFoundException,
    _InvalidS3KeyException,

    -- * LanguageCode
    LanguageCode (..),

    -- * SnsTopicArn
    SnsTopicArn (..),

    -- * LexiconName
    LexiconName (..),

    -- * LexiconAttributes
    LexiconAttributes (..),
    mkLexiconAttributes,
    laAlphabet,
    laLanguageCode,
    laLastModified,
    laLexemesCount,
    laLexiconArn,
    laSize,

    -- * LanguageName
    LanguageName (..),

    -- * Text
    Text (..),

    -- * TaskStatusReason
    TaskStatusReason (..),

    -- * OutputS3KeyPrefix
    OutputS3KeyPrefix (..),

    -- * TaskId
    TaskId (..),

    -- * Engine
    Engine (..),

    -- * NextToken
    NextToken (..),

    -- * LexiconDescription
    LexiconDescription (..),
    mkLexiconDescription,
    ldAttributes,
    ldName,

    -- * SampleRate
    SampleRate (..),

    -- * OutputFormat
    OutputFormat (..),

    -- * LexiconArn
    LexiconArn (..),

    -- * Alphabet
    Alphabet (..),

    -- * Gender
    Gender (..),

    -- * Lexicon
    Lexicon (..),
    mkLexicon,
    lContent,
    lName,

    -- * TextType
    TextType (..),

    -- * SpeechMarkType
    SpeechMarkType (..),

    -- * VoiceId
    VoiceId (..),

    -- * Voice
    Voice (..),
    mkVoice,
    vAdditionalLanguageCodes,
    vGender,
    vId,
    vLanguageCode,
    vLanguageName,
    vName,
    vSupportedEngines,

    -- * OutputS3BucketName
    OutputS3BucketName (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * SynthesisTask
    SynthesisTask (..),
    mkSynthesisTask,
    stCreationTime,
    stEngine,
    stLanguageCode,
    stLexiconNames,
    stOutputFormat,
    stOutputUri,
    stRequestCharacters,
    stSampleRate,
    stSnsTopicArn,
    stSpeechMarkTypes,
    stTaskId,
    stTaskStatus,
    stTaskStatusReason,
    stTextType,
    stVoiceId,

    -- * ContentType
    ContentType (..),

    -- * OutputUri
    OutputUri (..),

    -- * Content
    Content (..),

    -- * Name
    Name (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.Alphabet
import Network.AWS.Polly.Types.Content
import Network.AWS.Polly.Types.ContentType
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.Gender
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.LanguageName
import Network.AWS.Polly.Types.Lexicon
import Network.AWS.Polly.Types.LexiconArn
import Network.AWS.Polly.Types.LexiconAttributes
import Network.AWS.Polly.Types.LexiconDescription
import Network.AWS.Polly.Types.LexiconName
import Network.AWS.Polly.Types.Name
import Network.AWS.Polly.Types.NextToken
import Network.AWS.Polly.Types.OutputFormat
import Network.AWS.Polly.Types.OutputS3BucketName
import Network.AWS.Polly.Types.OutputS3KeyPrefix
import Network.AWS.Polly.Types.OutputUri
import Network.AWS.Polly.Types.SampleRate
import Network.AWS.Polly.Types.SnsTopicArn
import Network.AWS.Polly.Types.SpeechMarkType
import Network.AWS.Polly.Types.SynthesisTask
import Network.AWS.Polly.Types.TaskId
import Network.AWS.Polly.Types.TaskStatus
import Network.AWS.Polly.Types.TaskStatusReason
import Network.AWS.Polly.Types.Text
import Network.AWS.Polly.Types.TextType
import Network.AWS.Polly.Types.Voice
import Network.AWS.Polly.Types.VoiceId
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Polly",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "polly",
      Core._svcVersion = "2016-06-10",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Polly",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The provided SNS topic ARN is invalid. Please provide a valid SNS topic ARN and try again.
_InvalidSnsTopicArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSnsTopicArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSnsTopicArnException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidSnsTopicArnException "Use generic-lens or generic-optics instead." #-}

-- | The language specified in the lexicon is unsupported. For a list of supported languages, see <https://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html Lexicon Attributes> .
_UnsupportedPlsLanguageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlsLanguageException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedPlsLanguageException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnsupportedPlsLanguageException "Use generic-lens or generic-optics instead." #-}

-- | The SSML you provided is invalid. Verify the SSML syntax, spelling of tags and values, and then try again.
_InvalidSsmlException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSsmlException =
  Core._MatchServiceError mkServiceConfig "InvalidSsmlException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidSsmlException "Use generic-lens or generic-optics instead." #-}

-- | The specified sample rate is not valid.
_InvalidSampleRateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSampleRateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSampleRateException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidSampleRateException "Use generic-lens or generic-optics instead." #-}

-- | This engine is not compatible with the voice that you have designated. Choose a new voice that is compatible with the engine or change the engine and restart the operation.
_EngineNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EngineNotSupportedException =
  Core._MatchServiceError
    mkServiceConfig
    "EngineNotSupportedException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _EngineNotSupportedException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of lexicons would be exceeded by this operation.
_MaxLexiconsNumberExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxLexiconsNumberExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaxLexiconsNumberExceededException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _MaxLexiconsNumberExceededException "Use generic-lens or generic-optics instead." #-}

-- | The value of the "Text" parameter is longer than the accepted limits. For the @SynthesizeSpeech@ API, the limit for input text is a maximum of 6000 characters total, of which no more than 3000 can be billed characters. For the @StartSpeechSynthesisTask@ API, the maximum is 200,000 characters, of which no more than 100,000 can be billed characters. SSML tags are not counted as billed characters.
_TextLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TextLengthExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "TextLengthExceededException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _TextLengthExceededException "Use generic-lens or generic-optics instead." #-}

-- | The maximum size of the lexeme would be exceeded by this operation.
_MaxLexemeLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaxLexemeLengthExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaxLexemeLengthExceededException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _MaxLexemeLengthExceededException "Use generic-lens or generic-optics instead." #-}

-- | The provided Task ID is not valid. Please provide a valid Task ID and try again.
_InvalidTaskIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTaskIdException =
  Core._MatchServiceError mkServiceConfig "InvalidTaskIdException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidTaskIdException "Use generic-lens or generic-optics instead." #-}

-- | Amazon Polly can't find the specified lexicon. Verify that the lexicon's name is spelled correctly, and then try again.
_InvalidLexiconException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLexiconException =
  Core._MatchServiceError mkServiceConfig "InvalidLexiconException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidLexiconException "Use generic-lens or generic-optics instead." #-}

-- | An unknown condition has caused a service failure.
_ServiceFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceFailureException =
  Core._MatchServiceError mkServiceConfig "ServiceFailureException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _ServiceFailureException "Use generic-lens or generic-optics instead." #-}

-- | The alphabet specified by the lexicon is not a supported alphabet. Valid values are @x-sampa@ and @ipa@ .
_UnsupportedPlsAlphabetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedPlsAlphabetException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedPlsAlphabetException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _UnsupportedPlsAlphabetException "Use generic-lens or generic-optics instead." #-}

-- | The NextToken is invalid. Verify that it's spelled correctly, and then try again.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | Speech marks are not supported for the @OutputFormat@ selected. Speech marks are only available for content in @json@ format.
_MarksNotSupportedForFormatException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MarksNotSupportedForFormatException =
  Core._MatchServiceError
    mkServiceConfig
    "MarksNotSupportedForFormatException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _MarksNotSupportedForFormatException "Use generic-lens or generic-optics instead." #-}

-- | The Speech Synthesis task with requested Task ID cannot be found.
_SynthesisTaskNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SynthesisTaskNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "SynthesisTaskNotFoundException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SynthesisTaskNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | SSML speech marks are not supported for plain text-type input.
_SsmlMarksNotSupportedForTextTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SsmlMarksNotSupportedForTextTypeException =
  Core._MatchServiceError
    mkServiceConfig
    "SsmlMarksNotSupportedForTextTypeException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _SsmlMarksNotSupportedForTextTypeException "Use generic-lens or generic-optics instead." #-}

-- | The provided Amazon S3 bucket name is invalid. Please check your input with S3 bucket naming requirements and try again.
_InvalidS3BucketException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidS3BucketException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidS3BucketException "Use generic-lens or generic-optics instead." #-}

-- | The maximum size of the specified lexicon would be exceeded by this operation.
_LexiconSizeExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LexiconSizeExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "LexiconSizeExceededException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LexiconSizeExceededException "Use generic-lens or generic-optics instead." #-}

-- | The language specified is not currently supported by Amazon Polly in this capacity.
_LanguageNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LanguageNotSupportedException =
  Core._MatchServiceError
    mkServiceConfig
    "LanguageNotSupportedException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LanguageNotSupportedException "Use generic-lens or generic-optics instead." #-}

-- | Amazon Polly can't find the specified lexicon. This could be caused by a lexicon that is missing, its name is misspelled or specifying a lexicon that is in a different region.
--
-- Verify that the lexicon exists, is in the region (see 'ListLexicons' ) and that you spelled its name is spelled correctly. Then try again.
_LexiconNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LexiconNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "LexiconNotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _LexiconNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The provided Amazon S3 key prefix is invalid. Please provide a valid S3 object key name.
_InvalidS3KeyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3KeyException =
  Core._MatchServiceError mkServiceConfig "InvalidS3KeyException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidS3KeyException "Use generic-lens or generic-optics instead." #-}
