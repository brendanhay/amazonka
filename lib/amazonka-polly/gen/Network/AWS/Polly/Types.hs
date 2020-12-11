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
    pollyService,

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
    Lexicon (..),
    mkLexicon,
    lContent,
    lName,

    -- * LexiconAttributes
    LexiconAttributes (..),
    mkLexiconAttributes,
    laLanguageCode,
    laSize,
    laLexemesCount,
    laLexiconARN,
    laAlphabet,
    laLastModified,

    -- * LexiconDescription
    LexiconDescription (..),
    mkLexiconDescription,
    ldAttributes,
    ldName,

    -- * SynthesisTask
    SynthesisTask (..),
    mkSynthesisTask,
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
    Voice (..),
    mkVoice,
    vLanguageCode,
    vLanguageName,
    vGender,
    vName,
    vId,
    vAdditionalLanguageCodes,
    vSupportedEngines,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
pollyService :: Lude.Service
pollyService =
  Lude.Service
    { Lude._svcAbbrev = "Polly",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "polly",
      Lude._svcVersion = "2016-06-10",
      Lude._svcEndpoint = Lude.defaultEndpoint pollyService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Polly",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
