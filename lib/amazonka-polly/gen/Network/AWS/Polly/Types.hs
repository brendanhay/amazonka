{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types
  ( -- * Service Configuration
    polly,

    -- * Errors

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
    Lexicon,
    lexicon,
    lContent,
    lName,

    -- * LexiconAttributes
    LexiconAttributes,
    lexiconAttributes,
    laLanguageCode,
    laSize,
    laLexemesCount,
    laLexiconARN,
    laAlphabet,
    laLastModified,

    -- * LexiconDescription
    LexiconDescription,
    lexiconDescription,
    ldAttributes,
    ldName,

    -- * SynthesisTask
    SynthesisTask,
    synthesisTask,
    stCreationTime,
    stLanguageCode,
    stSNSTopicARN,
    stTaskStatusReason,
    stTaskId,
    stRequestCharacters,
    stEngine,
    stSpeechMarkTypes,
    stSampleRate,
    stOutputFormat,
    stTextType,
    stVoiceId,
    stLexiconNames,
    stTaskStatus,
    stOutputURI,

    -- * Voice
    Voice,
    voice,
    vLanguageCode,
    vLanguageName,
    vGender,
    vName,
    vId,
    vAdditionalLanguageCodes,
    vSupportedEngines,
  )
where

import Network.AWS.Lens
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
polly :: Service
polly =
  Service
    { _svcAbbrev = "Polly",
      _svcSigner = v4,
      _svcPrefix = "polly",
      _svcVersion = "2016-06-10",
      _svcEndpoint = defaultEndpoint polly,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Polly",
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
