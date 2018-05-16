{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types
    (
    -- * Service Configuration
      polly

    -- * Errors
    , _UnsupportedPlsLanguageException
    , _InvalidSsmlException
    , _InvalidSampleRateException
    , _MaxLexiconsNumberExceededException
    , _TextLengthExceededException
    , _MaxLexemeLengthExceededException
    , _InvalidLexiconException
    , _ServiceFailureException
    , _UnsupportedPlsAlphabetException
    , _InvalidNextTokenException
    , _MarksNotSupportedForFormatException
    , _SsmlMarksNotSupportedForTextTypeException
    , _LexiconSizeExceededException
    , _LexiconNotFoundException

    -- * Gender
    , Gender (..)

    -- * LanguageCode
    , LanguageCode (..)

    -- * OutputFormat
    , OutputFormat (..)

    -- * SpeechMarkType
    , SpeechMarkType (..)

    -- * TextType
    , TextType (..)

    -- * VoiceId
    , VoiceId (..)

    -- * Lexicon
    , Lexicon
    , lexicon
    , lContent
    , lName

    -- * LexiconAttributes
    , LexiconAttributes
    , lexiconAttributes
    , laLanguageCode
    , laSize
    , laLexemesCount
    , laLexiconARN
    , laAlphabet
    , laLastModified

    -- * LexiconDescription
    , LexiconDescription
    , lexiconDescription
    , ldAttributes
    , ldName

    -- * Voice
    , Voice
    , voice
    , vLanguageCode
    , vLanguageName
    , vGender
    , vName
    , vId
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types.Product
import Network.AWS.Polly.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
polly :: Service
polly =
  Service
    { _svcAbbrev = "Polly"
    , _svcSigner = v4
    , _svcPrefix = "polly"
    , _svcVersion = "2016-06-10"
    , _svcEndpoint = defaultEndpoint polly
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Polly"
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


-- | The language specified in the lexicon is unsupported. For a list of supported languages, see <http://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html Lexicon Attributes> .
--
--
_UnsupportedPlsLanguageException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlsLanguageException =
  _MatchServiceError polly "UnsupportedPlsLanguageException" . hasStatus 400


-- | The SSML you provided is invalid. Verify the SSML syntax, spelling of tags and values, and then try again.
--
--
_InvalidSsmlException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSsmlException =
  _MatchServiceError polly "InvalidSsmlException" . hasStatus 400


-- | The specified sample rate is not valid.
--
--
_InvalidSampleRateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSampleRateException =
  _MatchServiceError polly "InvalidSampleRateException" . hasStatus 400


-- | The maximum number of lexicons would be exceeded by this operation.
--
--
_MaxLexiconsNumberExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxLexiconsNumberExceededException =
  _MatchServiceError polly "MaxLexiconsNumberExceededException" . hasStatus 400


-- | The value of the "Text" parameter is longer than the accepted limits. The limit for input text is a maximum of 3000 characters total, of which no more than 1500 can be billed characters. SSML tags are not counted as billed characters.
--
--
_TextLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TextLengthExceededException =
  _MatchServiceError polly "TextLengthExceededException" . hasStatus 400


-- | The maximum size of the lexeme would be exceeded by this operation.
--
--
_MaxLexemeLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxLexemeLengthExceededException =
  _MatchServiceError polly "MaxLexemeLengthExceededException" . hasStatus 400


-- | Amazon Polly can't find the specified lexicon. Verify that the lexicon's name is spelled correctly, and then try again.
--
--
_InvalidLexiconException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLexiconException =
  _MatchServiceError polly "InvalidLexiconException" . hasStatus 400


-- | An unknown condition has caused a service failure.
--
--
_ServiceFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceFailureException =
  _MatchServiceError polly "ServiceFailureException" . hasStatus 500


-- | The alphabet specified by the lexicon is not a supported alphabet. Valid values are @x-sampa@ and @ipa@ .
--
--
_UnsupportedPlsAlphabetException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlsAlphabetException =
  _MatchServiceError polly "UnsupportedPlsAlphabetException" . hasStatus 400


-- | The NextToken is invalid. Verify that it's spelled correctly, and then try again.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError polly "InvalidNextTokenException" . hasStatus 400


-- | Speech marks are not supported for the @OutputFormat@ selected. Speech marks are only available for content in @json@ format.
--
--
_MarksNotSupportedForFormatException :: AsError a => Getting (First ServiceError) a ServiceError
_MarksNotSupportedForFormatException =
  _MatchServiceError polly "MarksNotSupportedForFormatException" . hasStatus 400


-- | SSML speech marks are not supported for plain text-type input.
--
--
_SsmlMarksNotSupportedForTextTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_SsmlMarksNotSupportedForTextTypeException =
  _MatchServiceError polly "SsmlMarksNotSupportedForTextTypeException" .
  hasStatus 400


-- | The maximum size of the specified lexicon would be exceeded by this operation.
--
--
_LexiconSizeExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LexiconSizeExceededException =
  _MatchServiceError polly "LexiconSizeExceededException" . hasStatus 400


-- | Amazon Polly can't find the specified lexicon. This could be caused by a lexicon that is missing, its name is misspelled or specifying a lexicon that is in a different region.
--
--
-- Verify that the lexicon exists, is in the region (see 'ListLexicons' ) and that you spelled its name is spelled correctly. Then try again.
--
_LexiconNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_LexiconNotFoundException =
  _MatchServiceError polly "LexiconNotFoundException" . hasStatus 404

