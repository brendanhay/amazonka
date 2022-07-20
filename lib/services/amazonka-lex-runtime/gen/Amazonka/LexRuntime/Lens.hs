{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexRuntime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Lens
  ( -- * Operations

    -- ** DeleteSession
    deleteSession_botName,
    deleteSession_botAlias,
    deleteSession_userId,
    deleteSessionResponse_botName,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_userId,
    deleteSessionResponse_botAlias,
    deleteSessionResponse_httpStatus,

    -- ** GetSession
    getSession_checkpointLabelFilter,
    getSession_botName,
    getSession_botAlias,
    getSession_userId,
    getSessionResponse_dialogAction,
    getSessionResponse_sessionAttributes,
    getSessionResponse_sessionId,
    getSessionResponse_activeContexts,
    getSessionResponse_recentIntentSummaryView,
    getSessionResponse_httpStatus,

    -- ** PostContent
    postContent_sessionAttributes,
    postContent_accept,
    postContent_requestAttributes,
    postContent_activeContexts,
    postContent_botName,
    postContent_botAlias,
    postContent_userId,
    postContent_contentType,
    postContent_inputStream,
    postContentResponse_message,
    postContentResponse_botVersion,
    postContentResponse_slotToElicit,
    postContentResponse_dialogState,
    postContentResponse_sessionAttributes,
    postContentResponse_messageFormat,
    postContentResponse_encodedMessage,
    postContentResponse_sentimentResponse,
    postContentResponse_sessionId,
    postContentResponse_intentName,
    postContentResponse_alternativeIntents,
    postContentResponse_inputTranscript,
    postContentResponse_activeContexts,
    postContentResponse_nluIntentConfidence,
    postContentResponse_slots,
    postContentResponse_encodedInputTranscript,
    postContentResponse_contentType,
    postContentResponse_httpStatus,
    postContentResponse_audioStream,

    -- ** PostText
    postText_sessionAttributes,
    postText_requestAttributes,
    postText_activeContexts,
    postText_botName,
    postText_botAlias,
    postText_userId,
    postText_inputText,
    postTextResponse_message,
    postTextResponse_botVersion,
    postTextResponse_slotToElicit,
    postTextResponse_dialogState,
    postTextResponse_sessionAttributes,
    postTextResponse_messageFormat,
    postTextResponse_sentimentResponse,
    postTextResponse_responseCard,
    postTextResponse_sessionId,
    postTextResponse_intentName,
    postTextResponse_alternativeIntents,
    postTextResponse_activeContexts,
    postTextResponse_nluIntentConfidence,
    postTextResponse_slots,
    postTextResponse_httpStatus,

    -- ** PutSession
    putSession_dialogAction,
    putSession_sessionAttributes,
    putSession_accept,
    putSession_activeContexts,
    putSession_recentIntentSummaryView,
    putSession_botName,
    putSession_botAlias,
    putSession_userId,
    putSessionResponse_message,
    putSessionResponse_slotToElicit,
    putSessionResponse_dialogState,
    putSessionResponse_sessionAttributes,
    putSessionResponse_messageFormat,
    putSessionResponse_encodedMessage,
    putSessionResponse_sessionId,
    putSessionResponse_intentName,
    putSessionResponse_activeContexts,
    putSessionResponse_slots,
    putSessionResponse_contentType,
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
    dialogAction_message,
    dialogAction_slotToElicit,
    dialogAction_messageFormat,
    dialogAction_intentName,
    dialogAction_slots,
    dialogAction_type,

    -- ** GenericAttachment
    genericAttachment_imageUrl,
    genericAttachment_subTitle,
    genericAttachment_buttons,
    genericAttachment_title,
    genericAttachment_attachmentLinkUrl,

    -- ** IntentConfidence
    intentConfidence_score,

    -- ** IntentSummary
    intentSummary_fulfillmentState,
    intentSummary_confirmationStatus,
    intentSummary_checkpointLabel,
    intentSummary_slotToElicit,
    intentSummary_intentName,
    intentSummary_slots,
    intentSummary_dialogActionType,

    -- ** PredictedIntent
    predictedIntent_intentName,
    predictedIntent_nluIntentConfidence,
    predictedIntent_slots,

    -- ** ResponseCard
    responseCard_genericAttachments,
    responseCard_version,
    responseCard_contentType,

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
