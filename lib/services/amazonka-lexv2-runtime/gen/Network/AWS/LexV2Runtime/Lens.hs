{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Runtime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Lens
  ( -- * Operations

    -- ** PutSession
    putSession_responseContentType,
    putSession_messages,
    putSession_requestAttributes,
    putSession_botId,
    putSession_botAliasId,
    putSession_localeId,
    putSession_sessionState,
    putSession_sessionId,
    putSessionResponse_sessionState,
    putSessionResponse_messages,
    putSessionResponse_sessionId,
    putSessionResponse_requestAttributes,
    putSessionResponse_contentType,
    putSessionResponse_httpStatus,
    putSessionResponse_audioStream,

    -- ** RecognizeUtterance
    recognizeUtterance_responseContentType,
    recognizeUtterance_sessionState,
    recognizeUtterance_requestAttributes,
    recognizeUtterance_botId,
    recognizeUtterance_botAliasId,
    recognizeUtterance_localeId,
    recognizeUtterance_requestContentType,
    recognizeUtterance_sessionId,
    recognizeUtterance_inputStream,
    recognizeUtteranceResponse_sessionState,
    recognizeUtteranceResponse_inputMode,
    recognizeUtteranceResponse_messages,
    recognizeUtteranceResponse_inputTranscript,
    recognizeUtteranceResponse_sessionId,
    recognizeUtteranceResponse_requestAttributes,
    recognizeUtteranceResponse_interpretations,
    recognizeUtteranceResponse_contentType,
    recognizeUtteranceResponse_httpStatus,
    recognizeUtteranceResponse_audioStream,

    -- ** DeleteSession
    deleteSession_botId,
    deleteSession_botAliasId,
    deleteSession_sessionId,
    deleteSession_localeId,
    deleteSessionResponse_botId,
    deleteSessionResponse_botAliasId,
    deleteSessionResponse_localeId,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_httpStatus,

    -- ** StartConversation
    startConversation_conversationMode,
    startConversation_botId,
    startConversation_botAliasId,
    startConversation_localeId,
    startConversation_requestEventStream,
    startConversation_sessionId,
    startConversationResponse_responseEventStream,
    startConversationResponse_httpStatus,

    -- ** GetSession
    getSession_botId,
    getSession_botAliasId,
    getSession_localeId,
    getSession_sessionId,
    getSessionResponse_sessionState,
    getSessionResponse_messages,
    getSessionResponse_sessionId,
    getSessionResponse_interpretations,
    getSessionResponse_httpStatus,

    -- ** RecognizeText
    recognizeText_sessionState,
    recognizeText_requestAttributes,
    recognizeText_botId,
    recognizeText_botAliasId,
    recognizeText_localeId,
    recognizeText_text,
    recognizeText_sessionId,
    recognizeTextResponse_sessionState,
    recognizeTextResponse_messages,
    recognizeTextResponse_sessionId,
    recognizeTextResponse_requestAttributes,
    recognizeTextResponse_interpretations,
    recognizeTextResponse_httpStatus,

    -- * Types

    -- ** ActiveContext
    activeContext_name,
    activeContext_timeToLive,
    activeContext_contextAttributes,

    -- ** ActiveContextTimeToLive
    activeContextTimeToLive_timeToLiveInSeconds,
    activeContextTimeToLive_turnsToLive,

    -- ** AudioInputEvent
    audioInputEvent_audioChunk,
    audioInputEvent_clientTimestampMillis,
    audioInputEvent_eventId,
    audioInputEvent_contentType,

    -- ** AudioResponseEvent
    audioResponseEvent_audioChunk,
    audioResponseEvent_contentType,
    audioResponseEvent_eventId,

    -- ** Button
    button_text,
    button_value,

    -- ** ConfidenceScore
    confidenceScore_score,

    -- ** ConfigurationEvent
    configurationEvent_welcomeMessages,
    configurationEvent_disablePlayback,
    configurationEvent_sessionState,
    configurationEvent_requestAttributes,
    configurationEvent_clientTimestampMillis,
    configurationEvent_eventId,
    configurationEvent_responseContentType,

    -- ** DTMFInputEvent
    dTMFInputEvent_clientTimestampMillis,
    dTMFInputEvent_eventId,
    dTMFInputEvent_inputCharacter,

    -- ** DialogAction
    dialogAction_slotToElicit,
    dialogAction_type,

    -- ** DisconnectionEvent
    disconnectionEvent_clientTimestampMillis,
    disconnectionEvent_eventId,

    -- ** HeartbeatEvent
    heartbeatEvent_eventId,

    -- ** ImageResponseCard
    imageResponseCard_buttons,
    imageResponseCard_subtitle,
    imageResponseCard_imageUrl,
    imageResponseCard_title,

    -- ** Intent
    intent_slots,
    intent_state,
    intent_confirmationState,
    intent_name,

    -- ** IntentResultEvent
    intentResultEvent_sessionState,
    intentResultEvent_inputMode,
    intentResultEvent_sessionId,
    intentResultEvent_requestAttributes,
    intentResultEvent_interpretations,
    intentResultEvent_eventId,

    -- ** Interpretation
    interpretation_sentimentResponse,
    interpretation_intent,
    interpretation_nluConfidence,

    -- ** Message
    message_imageResponseCard,
    message_content,
    message_contentType,

    -- ** PlaybackCompletionEvent
    playbackCompletionEvent_clientTimestampMillis,
    playbackCompletionEvent_eventId,

    -- ** PlaybackInterruptionEvent
    playbackInterruptionEvent_causedByEventId,
    playbackInterruptionEvent_eventReason,
    playbackInterruptionEvent_eventId,

    -- ** SentimentResponse
    sentimentResponse_sentiment,
    sentimentResponse_sentimentScore,

    -- ** SentimentScore
    sentimentScore_mixed,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_positive,

    -- ** SessionState
    sessionState_originatingRequestId,
    sessionState_intent,
    sessionState_activeContexts,
    sessionState_dialogAction,
    sessionState_sessionAttributes,

    -- ** Slot
    slot_values,
    slot_value,
    slot_shape,

    -- ** StartConversationRequestEventStream
    startConversationRequestEventStream_playbackCompletionEvent,
    startConversationRequestEventStream_dTMFInputEvent,
    startConversationRequestEventStream_configurationEvent,
    startConversationRequestEventStream_disconnectionEvent,
    startConversationRequestEventStream_audioInputEvent,
    startConversationRequestEventStream_textInputEvent,

    -- ** StartConversationResponseEventStream
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

    -- ** TextInputEvent
    textInputEvent_clientTimestampMillis,
    textInputEvent_eventId,
    textInputEvent_text,

    -- ** TextResponseEvent
    textResponseEvent_messages,
    textResponseEvent_eventId,

    -- ** TranscriptEvent
    transcriptEvent_transcript,
    transcriptEvent_eventId,

    -- ** Value
    value_originalValue,
    value_resolvedValues,
    value_interpretedValue,
  )
where

import Network.AWS.LexV2Runtime.DeleteSession
import Network.AWS.LexV2Runtime.GetSession
import Network.AWS.LexV2Runtime.PutSession
import Network.AWS.LexV2Runtime.RecognizeText
import Network.AWS.LexV2Runtime.RecognizeUtterance
import Network.AWS.LexV2Runtime.StartConversation
import Network.AWS.LexV2Runtime.Types.ActiveContext
import Network.AWS.LexV2Runtime.Types.ActiveContextTimeToLive
import Network.AWS.LexV2Runtime.Types.AudioInputEvent
import Network.AWS.LexV2Runtime.Types.AudioResponseEvent
import Network.AWS.LexV2Runtime.Types.Button
import Network.AWS.LexV2Runtime.Types.ConfidenceScore
import Network.AWS.LexV2Runtime.Types.ConfigurationEvent
import Network.AWS.LexV2Runtime.Types.DTMFInputEvent
import Network.AWS.LexV2Runtime.Types.DialogAction
import Network.AWS.LexV2Runtime.Types.DisconnectionEvent
import Network.AWS.LexV2Runtime.Types.HeartbeatEvent
import Network.AWS.LexV2Runtime.Types.ImageResponseCard
import Network.AWS.LexV2Runtime.Types.Intent
import Network.AWS.LexV2Runtime.Types.IntentResultEvent
import Network.AWS.LexV2Runtime.Types.Interpretation
import Network.AWS.LexV2Runtime.Types.Message
import Network.AWS.LexV2Runtime.Types.PlaybackCompletionEvent
import Network.AWS.LexV2Runtime.Types.PlaybackInterruptionEvent
import Network.AWS.LexV2Runtime.Types.SentimentResponse
import Network.AWS.LexV2Runtime.Types.SentimentScore
import Network.AWS.LexV2Runtime.Types.SessionState
import Network.AWS.LexV2Runtime.Types.Slot
import Network.AWS.LexV2Runtime.Types.StartConversationRequestEventStream
import Network.AWS.LexV2Runtime.Types.StartConversationResponseEventStream
import Network.AWS.LexV2Runtime.Types.TextInputEvent
import Network.AWS.LexV2Runtime.Types.TextResponseEvent
import Network.AWS.LexV2Runtime.Types.TranscriptEvent
import Network.AWS.LexV2Runtime.Types.Value
