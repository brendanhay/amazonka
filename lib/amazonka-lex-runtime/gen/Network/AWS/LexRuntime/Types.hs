-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types
  ( -- * Service configuration
    lexRuntimeService,

    -- * Errors

    -- * ConfirmationStatus
    ConfirmationStatus (..),

    -- * ContentType
    ContentType (..),

    -- * DialogActionType
    DialogActionType (..),

    -- * DialogState
    DialogState (..),

    -- * FulfillmentState
    FulfillmentState (..),

    -- * MessageFormatType
    MessageFormatType (..),

    -- * ActiveContext
    ActiveContext (..),
    mkActiveContext,
    acTimeToLive,
    acName,
    acParameters,

    -- * ActiveContextTimeToLive
    ActiveContextTimeToLive (..),
    mkActiveContextTimeToLive,
    acttlTurnsToLive,
    acttlTimeToLiveInSeconds,

    -- * Button
    Button (..),
    mkButton,
    bText,
    bValue,

    -- * DialogAction
    DialogAction (..),
    mkDialogAction,
    daSlots,
    daIntentName,
    daFulfillmentState,
    daType,
    daMessageFormat,
    daMessage,
    daSlotToElicit,

    -- * GenericAttachment
    GenericAttachment (..),
    mkGenericAttachment,
    gaButtons,
    gaSubTitle,
    gaImageURL,
    gaAttachmentLinkURL,
    gaTitle,

    -- * IntentConfidence
    IntentConfidence (..),
    mkIntentConfidence,
    icScore,

    -- * IntentSummary
    IntentSummary (..),
    mkIntentSummary,
    isCheckpointLabel,
    isSlots,
    isIntentName,
    isDialogActionType,
    isFulfillmentState,
    isConfirmationStatus,
    isSlotToElicit,

    -- * PredictedIntent
    PredictedIntent (..),
    mkPredictedIntent,
    piNluIntentConfidence,
    piSlots,
    piIntentName,

    -- * ResponseCard
    ResponseCard (..),
    mkResponseCard,
    rcGenericAttachments,
    rcVersion,
    rcContentType,

    -- * SentimentResponse
    SentimentResponse (..),
    mkSentimentResponse,
    sSentimentScore,
    sSentimentLabel,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.ActiveContext
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import Network.AWS.LexRuntime.Types.Button
import Network.AWS.LexRuntime.Types.ConfirmationStatus
import Network.AWS.LexRuntime.Types.ContentType
import Network.AWS.LexRuntime.Types.DialogAction
import Network.AWS.LexRuntime.Types.DialogActionType
import Network.AWS.LexRuntime.Types.DialogState
import Network.AWS.LexRuntime.Types.FulfillmentState
import Network.AWS.LexRuntime.Types.GenericAttachment
import Network.AWS.LexRuntime.Types.IntentConfidence
import Network.AWS.LexRuntime.Types.IntentSummary
import Network.AWS.LexRuntime.Types.MessageFormatType
import Network.AWS.LexRuntime.Types.PredictedIntent
import Network.AWS.LexRuntime.Types.ResponseCard
import Network.AWS.LexRuntime.Types.SentimentResponse
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
lexRuntimeService :: Lude.Service
lexRuntimeService =
  Lude.Service
    { Lude._svcAbbrev = "LexRuntime",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "runtime.lex",
      Lude._svcVersion = "2016-11-28",
      Lude._svcEndpoint = Lude.defaultEndpoint lexRuntimeService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "LexRuntime",
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
