{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Runtime.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _DependencyFailedException,
    _ConflictException,
    _ThrottlingException,
    _InternalServerException,
    _BadGatewayException,
    _ResourceNotFoundException,

    -- * ConfirmationState
    ConfirmationState (..),

    -- * ConversationMode
    ConversationMode (..),

    -- * DialogActionType
    DialogActionType (..),

    -- * InputMode
    InputMode (..),

    -- * IntentState
    IntentState (..),

    -- * MessageContentType
    MessageContentType (..),

    -- * PlaybackInterruptionReason
    PlaybackInterruptionReason (..),

    -- * SentimentType
    SentimentType (..),

    -- * Shape
    Shape (..),

    -- * ActiveContext
    ActiveContext (..),
    newActiveContext,
    activeContext_name,
    activeContext_timeToLive,
    activeContext_contextAttributes,

    -- * ActiveContextTimeToLive
    ActiveContextTimeToLive (..),
    newActiveContextTimeToLive,
    activeContextTimeToLive_timeToLiveInSeconds,
    activeContextTimeToLive_turnsToLive,

    -- * AudioInputEvent
    AudioInputEvent (..),
    newAudioInputEvent,
    audioInputEvent_audioChunk,
    audioInputEvent_clientTimestampMillis,
    audioInputEvent_eventId,
    audioInputEvent_contentType,

    -- * AudioResponseEvent
    AudioResponseEvent (..),
    newAudioResponseEvent,
    audioResponseEvent_audioChunk,
    audioResponseEvent_contentType,
    audioResponseEvent_eventId,

    -- * Button
    Button (..),
    newButton,
    button_text,
    button_value,

    -- * ConfidenceScore
    ConfidenceScore (..),
    newConfidenceScore,
    confidenceScore_score,

    -- * ConfigurationEvent
    ConfigurationEvent (..),
    newConfigurationEvent,
    configurationEvent_welcomeMessages,
    configurationEvent_disablePlayback,
    configurationEvent_sessionState,
    configurationEvent_requestAttributes,
    configurationEvent_clientTimestampMillis,
    configurationEvent_eventId,
    configurationEvent_responseContentType,

    -- * DTMFInputEvent
    DTMFInputEvent (..),
    newDTMFInputEvent,
    dTMFInputEvent_clientTimestampMillis,
    dTMFInputEvent_eventId,
    dTMFInputEvent_inputCharacter,

    -- * DialogAction
    DialogAction (..),
    newDialogAction,
    dialogAction_slotToElicit,
    dialogAction_type,

    -- * DisconnectionEvent
    DisconnectionEvent (..),
    newDisconnectionEvent,
    disconnectionEvent_clientTimestampMillis,
    disconnectionEvent_eventId,

    -- * HeartbeatEvent
    HeartbeatEvent (..),
    newHeartbeatEvent,
    heartbeatEvent_eventId,

    -- * ImageResponseCard
    ImageResponseCard (..),
    newImageResponseCard,
    imageResponseCard_buttons,
    imageResponseCard_subtitle,
    imageResponseCard_imageUrl,
    imageResponseCard_title,

    -- * Intent
    Intent (..),
    newIntent,
    intent_slots,
    intent_state,
    intent_confirmationState,
    intent_name,

    -- * IntentResultEvent
    IntentResultEvent (..),
    newIntentResultEvent,
    intentResultEvent_sessionState,
    intentResultEvent_inputMode,
    intentResultEvent_sessionId,
    intentResultEvent_requestAttributes,
    intentResultEvent_interpretations,
    intentResultEvent_eventId,

    -- * Interpretation
    Interpretation (..),
    newInterpretation,
    interpretation_sentimentResponse,
    interpretation_intent,
    interpretation_nluConfidence,

    -- * Message
    Message (..),
    newMessage,
    message_imageResponseCard,
    message_content,
    message_contentType,

    -- * PlaybackCompletionEvent
    PlaybackCompletionEvent (..),
    newPlaybackCompletionEvent,
    playbackCompletionEvent_clientTimestampMillis,
    playbackCompletionEvent_eventId,

    -- * PlaybackInterruptionEvent
    PlaybackInterruptionEvent (..),
    newPlaybackInterruptionEvent,
    playbackInterruptionEvent_causedByEventId,
    playbackInterruptionEvent_eventReason,
    playbackInterruptionEvent_eventId,

    -- * SentimentResponse
    SentimentResponse (..),
    newSentimentResponse,
    sentimentResponse_sentiment,
    sentimentResponse_sentimentScore,

    -- * SentimentScore
    SentimentScore (..),
    newSentimentScore,
    sentimentScore_mixed,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_positive,

    -- * SessionState
    SessionState (..),
    newSessionState,
    sessionState_originatingRequestId,
    sessionState_intent,
    sessionState_activeContexts,
    sessionState_dialogAction,
    sessionState_sessionAttributes,

    -- * Slot
    Slot (..),
    newSlot,
    slot_values,
    slot_value,
    slot_shape,

    -- * StartConversationRequestEventStream
    StartConversationRequestEventStream (..),
    newStartConversationRequestEventStream,
    startConversationRequestEventStream_playbackCompletionEvent,
    startConversationRequestEventStream_dTMFInputEvent,
    startConversationRequestEventStream_configurationEvent,
    startConversationRequestEventStream_disconnectionEvent,
    startConversationRequestEventStream_audioInputEvent,
    startConversationRequestEventStream_textInputEvent,

    -- * StartConversationResponseEventStream
    StartConversationResponseEventStream (..),
    newStartConversationResponseEventStream,
    startConversationResponseEventStream_validationException,
    startConversationResponseEventStream_accessDeniedException,
    startConversationResponseEventStream_transcriptEvent,
    startConversationResponseEventStream_textResponseEvent,
    startConversationResponseEventStream_dependencyFailedException,
    startConversationResponseEventStream_intentResultEvent,
    startConversationResponseEventStream_conflictException,
    startConversationResponseEventStream_playbackInterruptionEvent,
    startConversationResponseEventStream_heartbeatEvent,
    startConversationResponseEventStream_throttlingException,
    startConversationResponseEventStream_internalServerException,
    startConversationResponseEventStream_badGatewayException,
    startConversationResponseEventStream_resourceNotFoundException,
    startConversationResponseEventStream_audioResponseEvent,

    -- * TextInputEvent
    TextInputEvent (..),
    newTextInputEvent,
    textInputEvent_clientTimestampMillis,
    textInputEvent_eventId,
    textInputEvent_text,

    -- * TextResponseEvent
    TextResponseEvent (..),
    newTextResponseEvent,
    textResponseEvent_messages,
    textResponseEvent_eventId,

    -- * TranscriptEvent
    TranscriptEvent (..),
    newTranscriptEvent,
    transcriptEvent_transcript,
    transcriptEvent_eventId,

    -- * Value
    Value (..),
    newValue,
    value_originalValue,
    value_resolvedValues,
    value_interpretedValue,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.ActiveContext
import Network.AWS.LexV2Runtime.Types.ActiveContextTimeToLive
import Network.AWS.LexV2Runtime.Types.AudioInputEvent
import Network.AWS.LexV2Runtime.Types.AudioResponseEvent
import Network.AWS.LexV2Runtime.Types.Button
import Network.AWS.LexV2Runtime.Types.ConfidenceScore
import Network.AWS.LexV2Runtime.Types.ConfigurationEvent
import Network.AWS.LexV2Runtime.Types.ConfirmationState
import Network.AWS.LexV2Runtime.Types.ConversationMode
import Network.AWS.LexV2Runtime.Types.DTMFInputEvent
import Network.AWS.LexV2Runtime.Types.DialogAction
import Network.AWS.LexV2Runtime.Types.DialogActionType
import Network.AWS.LexV2Runtime.Types.DisconnectionEvent
import Network.AWS.LexV2Runtime.Types.HeartbeatEvent
import Network.AWS.LexV2Runtime.Types.ImageResponseCard
import Network.AWS.LexV2Runtime.Types.InputMode
import Network.AWS.LexV2Runtime.Types.Intent
import Network.AWS.LexV2Runtime.Types.IntentResultEvent
import Network.AWS.LexV2Runtime.Types.IntentState
import Network.AWS.LexV2Runtime.Types.Interpretation
import Network.AWS.LexV2Runtime.Types.Message
import Network.AWS.LexV2Runtime.Types.MessageContentType
import Network.AWS.LexV2Runtime.Types.PlaybackCompletionEvent
import Network.AWS.LexV2Runtime.Types.PlaybackInterruptionEvent
import Network.AWS.LexV2Runtime.Types.PlaybackInterruptionReason
import Network.AWS.LexV2Runtime.Types.SentimentResponse
import Network.AWS.LexV2Runtime.Types.SentimentScore
import Network.AWS.LexV2Runtime.Types.SentimentType
import Network.AWS.LexV2Runtime.Types.SessionState
import Network.AWS.LexV2Runtime.Types.Shape
import Network.AWS.LexV2Runtime.Types.Slot
import Network.AWS.LexV2Runtime.Types.StartConversationRequestEventStream
import Network.AWS.LexV2Runtime.Types.StartConversationResponseEventStream
import Network.AWS.LexV2Runtime.Types.TextInputEvent
import Network.AWS.LexV2Runtime.Types.TextResponseEvent
import Network.AWS.LexV2Runtime.Types.TranscriptEvent
import Network.AWS.LexV2Runtime.Types.Value
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-08-07@ of the Amazon Lex Runtime V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "LexV2Runtime",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "runtime-v2-lex",
      Core._serviceSigningName = "lex",
      Core._serviceVersion = "2020-08-07",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LexV2Runtime",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- |
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- |
_DependencyFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependencyFailedException =
  Core._MatchServiceError
    defaultService
    "DependencyFailedException"
    Prelude.. Core.hasStatus 424

-- |
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- |
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- |
_BadGatewayException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadGatewayException =
  Core._MatchServiceError
    defaultService
    "BadGatewayException"
    Prelude.. Core.hasStatus 502

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
