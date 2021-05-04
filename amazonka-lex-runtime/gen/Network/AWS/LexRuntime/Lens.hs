{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Lens
  ( -- * Operations

    -- ** PutSession
    putSession_sessionAttributes,
    putSession_dialogAction,
    putSession_accept,
    putSession_recentIntentSummaryView,
    putSession_activeContexts,
    putSession_botName,
    putSession_botAlias,
    putSession_userId,
    putSessionResponse_dialogState,
    putSessionResponse_sessionAttributes,
    putSessionResponse_contentType,
    putSessionResponse_message,
    putSessionResponse_sessionId,
    putSessionResponse_intentName,
    putSessionResponse_messageFormat,
    putSessionResponse_slots,
    putSessionResponse_slotToElicit,
    putSessionResponse_activeContexts,
    putSessionResponse_httpStatus,
    putSessionResponse_audioStream,

    -- ** DeleteSession
    deleteSession_botName,
    deleteSession_botAlias,
    deleteSession_userId,
    deleteSessionResponse_botAlias,
    deleteSessionResponse_botName,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_userId,
    deleteSessionResponse_httpStatus,

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
    postContentResponse_dialogState,
    postContentResponse_sessionAttributes,
    postContentResponse_contentType,
    postContentResponse_message,
    postContentResponse_sessionId,
    postContentResponse_intentName,
    postContentResponse_botVersion,
    postContentResponse_inputTranscript,
    postContentResponse_messageFormat,
    postContentResponse_slots,
    postContentResponse_nluIntentConfidence,
    postContentResponse_sentimentResponse,
    postContentResponse_slotToElicit,
    postContentResponse_activeContexts,
    postContentResponse_alternativeIntents,
    postContentResponse_httpStatus,
    postContentResponse_audioStream,

    -- ** GetSession
    getSession_checkpointLabelFilter,
    getSession_botName,
    getSession_botAlias,
    getSession_userId,
    getSessionResponse_sessionAttributes,
    getSessionResponse_dialogAction,
    getSessionResponse_sessionId,
    getSessionResponse_recentIntentSummaryView,
    getSessionResponse_activeContexts,
    getSessionResponse_httpStatus,

    -- ** PostText
    postText_sessionAttributes,
    postText_requestAttributes,
    postText_activeContexts,
    postText_botName,
    postText_botAlias,
    postText_userId,
    postText_inputText,
    postTextResponse_responseCard,
    postTextResponse_dialogState,
    postTextResponse_sessionAttributes,
    postTextResponse_message,
    postTextResponse_sessionId,
    postTextResponse_intentName,
    postTextResponse_botVersion,
    postTextResponse_messageFormat,
    postTextResponse_slots,
    postTextResponse_nluIntentConfidence,
    postTextResponse_sentimentResponse,
    postTextResponse_slotToElicit,
    postTextResponse_activeContexts,
    postTextResponse_alternativeIntents,
    postTextResponse_httpStatus,

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
    dialogAction_message,
    dialogAction_intentName,
    dialogAction_messageFormat,
    dialogAction_fulfillmentState,
    dialogAction_slots,
    dialogAction_slotToElicit,
    dialogAction_type,

    -- ** GenericAttachment
    genericAttachment_title,
    genericAttachment_buttons,
    genericAttachment_attachmentLinkUrl,
    genericAttachment_imageUrl,
    genericAttachment_subTitle,

    -- ** IntentConfidence
    intentConfidence_score,

    -- ** IntentSummary
    intentSummary_intentName,
    intentSummary_fulfillmentState,
    intentSummary_slots,
    intentSummary_checkpointLabel,
    intentSummary_slotToElicit,
    intentSummary_confirmationStatus,
    intentSummary_dialogActionType,

    -- ** PredictedIntent
    predictedIntent_intentName,
    predictedIntent_slots,
    predictedIntent_nluIntentConfidence,

    -- ** ResponseCard
    responseCard_contentType,
    responseCard_genericAttachments,
    responseCard_version,

    -- ** SentimentResponse
    sentimentResponse_sentimentScore,
    sentimentResponse_sentimentLabel,
  )
where

import Network.AWS.LexRuntime.DeleteSession
import Network.AWS.LexRuntime.GetSession
import Network.AWS.LexRuntime.PostContent
import Network.AWS.LexRuntime.PostText
import Network.AWS.LexRuntime.PutSession
import Network.AWS.LexRuntime.Types.ActiveContext
import Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
import Network.AWS.LexRuntime.Types.Button
import Network.AWS.LexRuntime.Types.DialogAction
import Network.AWS.LexRuntime.Types.GenericAttachment
import Network.AWS.LexRuntime.Types.IntentConfidence
import Network.AWS.LexRuntime.Types.IntentSummary
import Network.AWS.LexRuntime.Types.PredictedIntent
import Network.AWS.LexRuntime.Types.ResponseCard
import Network.AWS.LexRuntime.Types.SentimentResponse
