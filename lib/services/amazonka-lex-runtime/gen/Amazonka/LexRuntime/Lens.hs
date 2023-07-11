{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexRuntime.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Lens
  ( -- * Operations

    -- ** DeleteSession
    deleteSession_botName,
    deleteSession_botAlias,
    deleteSession_userId,
    deleteSessionResponse_botAlias,
    deleteSessionResponse_botName,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_userId,
    deleteSessionResponse_httpStatus,

    -- ** GetSession
    getSession_checkpointLabelFilter,
    getSession_botName,
    getSession_botAlias,
    getSession_userId,
    getSessionResponse_activeContexts,
    getSessionResponse_dialogAction,
    getSessionResponse_recentIntentSummaryView,
    getSessionResponse_sessionAttributes,
    getSessionResponse_sessionId,
    getSessionResponse_httpStatus,

    -- ** PostContent
    postContent_accept,
    postContent_activeContexts,
    postContent_requestAttributes,
    postContent_sessionAttributes,
    postContent_botName,
    postContent_botAlias,
    postContent_userId,
    postContent_contentType,
    postContent_inputStream,
    postContentResponse_activeContexts,
    postContentResponse_alternativeIntents,
    postContentResponse_botVersion,
    postContentResponse_contentType,
    postContentResponse_dialogState,
    postContentResponse_encodedInputTranscript,
    postContentResponse_encodedMessage,
    postContentResponse_inputTranscript,
    postContentResponse_intentName,
    postContentResponse_message,
    postContentResponse_messageFormat,
    postContentResponse_nluIntentConfidence,
    postContentResponse_sentimentResponse,
    postContentResponse_sessionAttributes,
    postContentResponse_sessionId,
    postContentResponse_slotToElicit,
    postContentResponse_slots,
    postContentResponse_httpStatus,
    postContentResponse_audioStream,

    -- ** PostText
    postText_activeContexts,
    postText_requestAttributes,
    postText_sessionAttributes,
    postText_botName,
    postText_botAlias,
    postText_userId,
    postText_inputText,
    postTextResponse_activeContexts,
    postTextResponse_alternativeIntents,
    postTextResponse_botVersion,
    postTextResponse_dialogState,
    postTextResponse_intentName,
    postTextResponse_message,
    postTextResponse_messageFormat,
    postTextResponse_nluIntentConfidence,
    postTextResponse_responseCard,
    postTextResponse_sentimentResponse,
    postTextResponse_sessionAttributes,
    postTextResponse_sessionId,
    postTextResponse_slotToElicit,
    postTextResponse_slots,
    postTextResponse_httpStatus,

    -- ** PutSession
    putSession_accept,
    putSession_activeContexts,
    putSession_dialogAction,
    putSession_recentIntentSummaryView,
    putSession_sessionAttributes,
    putSession_botName,
    putSession_botAlias,
    putSession_userId,
    putSessionResponse_activeContexts,
    putSessionResponse_contentType,
    putSessionResponse_dialogState,
    putSessionResponse_encodedMessage,
    putSessionResponse_intentName,
    putSessionResponse_message,
    putSessionResponse_messageFormat,
    putSessionResponse_sessionAttributes,
    putSessionResponse_sessionId,
    putSessionResponse_slotToElicit,
    putSessionResponse_slots,
    putSessionResponse_httpStatus,
    putSessionResponse_audioStream,

    -- * Types

    -- ** ActiveContext
    activeContext_name,
    activeContext_timeToLive,
    activeContext_parameters,

    -- ** ActiveContextTimeToLive
    activeContextTimeToLive_timeToLiveInSeconds,
    activeContextTimeToLive_turnsToLive,

    -- ** Button
    button_text,
    button_value,

    -- ** DialogAction
    dialogAction_fulfillmentState,
    dialogAction_intentName,
    dialogAction_message,
    dialogAction_messageFormat,
    dialogAction_slotToElicit,
    dialogAction_slots,
    dialogAction_type,

    -- ** GenericAttachment
    genericAttachment_attachmentLinkUrl,
    genericAttachment_buttons,
    genericAttachment_imageUrl,
    genericAttachment_subTitle,
    genericAttachment_title,

    -- ** IntentConfidence
    intentConfidence_score,

    -- ** IntentSummary
    intentSummary_checkpointLabel,
    intentSummary_confirmationStatus,
    intentSummary_fulfillmentState,
    intentSummary_intentName,
    intentSummary_slotToElicit,
    intentSummary_slots,
    intentSummary_dialogActionType,

    -- ** PredictedIntent
    predictedIntent_intentName,
    predictedIntent_nluIntentConfidence,
    predictedIntent_slots,

    -- ** ResponseCard
    responseCard_contentType,
    responseCard_genericAttachments,
    responseCard_version,

    -- ** SentimentResponse
    sentimentResponse_sentimentLabel,
    sentimentResponse_sentimentScore,
  )
where

import Amazonka.LexRuntime.DeleteSession
import Amazonka.LexRuntime.GetSession
import Amazonka.LexRuntime.PostContent
import Amazonka.LexRuntime.PostText
import Amazonka.LexRuntime.PutSession
import Amazonka.LexRuntime.Types.ActiveContext
import Amazonka.LexRuntime.Types.ActiveContextTimeToLive
import Amazonka.LexRuntime.Types.Button
import Amazonka.LexRuntime.Types.DialogAction
import Amazonka.LexRuntime.Types.GenericAttachment
import Amazonka.LexRuntime.Types.IntentConfidence
import Amazonka.LexRuntime.Types.IntentSummary
import Amazonka.LexRuntime.Types.PredictedIntent
import Amazonka.LexRuntime.Types.ResponseCard
import Amazonka.LexRuntime.Types.SentimentResponse
