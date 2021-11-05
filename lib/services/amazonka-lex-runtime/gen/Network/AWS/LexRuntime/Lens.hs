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

    -- ** PutSession
    putSession_accept,
    putSession_activeContexts,
    putSession_recentIntentSummaryView,
    putSession_dialogAction,
    putSession_sessionAttributes,
    putSession_botName,
    putSession_botAlias,
    putSession_userId,
    putSessionResponse_slots,
    putSessionResponse_intentName,
    putSessionResponse_encodedMessage,
    putSessionResponse_dialogState,
    putSessionResponse_activeContexts,
    putSessionResponse_messageFormat,
    putSessionResponse_message,
    putSessionResponse_sessionId,
    putSessionResponse_slotToElicit,
    putSessionResponse_contentType,
    putSessionResponse_sessionAttributes,
    putSessionResponse_httpStatus,
    putSessionResponse_audioStream,

    -- ** DeleteSession
    deleteSession_botName,
    deleteSession_botAlias,
    deleteSession_userId,
    deleteSessionResponse_botAlias,
    deleteSessionResponse_botName,
    deleteSessionResponse_userId,
    deleteSessionResponse_sessionId,
    deleteSessionResponse_httpStatus,

    -- ** PostText
    postText_activeContexts,
    postText_requestAttributes,
    postText_sessionAttributes,
    postText_botName,
    postText_botAlias,
    postText_userId,
    postText_inputText,
    postTextResponse_sentimentResponse,
    postTextResponse_nluIntentConfidence,
    postTextResponse_slots,
    postTextResponse_responseCard,
    postTextResponse_intentName,
    postTextResponse_botVersion,
    postTextResponse_dialogState,
    postTextResponse_activeContexts,
    postTextResponse_alternativeIntents,
    postTextResponse_messageFormat,
    postTextResponse_message,
    postTextResponse_sessionId,
    postTextResponse_slotToElicit,
    postTextResponse_sessionAttributes,
    postTextResponse_httpStatus,

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
    postContentResponse_sentimentResponse,
    postContentResponse_nluIntentConfidence,
    postContentResponse_slots,
    postContentResponse_encodedInputTranscript,
    postContentResponse_intentName,
    postContentResponse_botVersion,
    postContentResponse_encodedMessage,
    postContentResponse_dialogState,
    postContentResponse_activeContexts,
    postContentResponse_alternativeIntents,
    postContentResponse_inputTranscript,
    postContentResponse_messageFormat,
    postContentResponse_message,
    postContentResponse_sessionId,
    postContentResponse_slotToElicit,
    postContentResponse_contentType,
    postContentResponse_sessionAttributes,
    postContentResponse_httpStatus,
    postContentResponse_audioStream,

    -- ** GetSession
    getSession_checkpointLabelFilter,
    getSession_botName,
    getSession_botAlias,
    getSession_userId,
    getSessionResponse_activeContexts,
    getSessionResponse_sessionId,
    getSessionResponse_recentIntentSummaryView,
    getSessionResponse_dialogAction,
    getSessionResponse_sessionAttributes,
    getSessionResponse_httpStatus,

    -- * Types

    -- ** ActiveContext
    activeContext_name,
    activeContext_timeToLive,
    activeContext_parameters,

    -- ** ActiveContextTimeToLive
    activeContextTimeToLive_turnsToLive,
    activeContextTimeToLive_timeToLiveInSeconds,

    -- ** Button
    button_text,
    button_value,

    -- ** DialogAction
    dialogAction_slots,
    dialogAction_intentName,
    dialogAction_fulfillmentState,
    dialogAction_messageFormat,
    dialogAction_message,
    dialogAction_slotToElicit,
    dialogAction_type,

    -- ** GenericAttachment
    genericAttachment_buttons,
    genericAttachment_subTitle,
    genericAttachment_imageUrl,
    genericAttachment_attachmentLinkUrl,
    genericAttachment_title,

    -- ** IntentConfidence
    intentConfidence_score,

    -- ** IntentSummary
    intentSummary_checkpointLabel,
    intentSummary_slots,
    intentSummary_intentName,
    intentSummary_fulfillmentState,
    intentSummary_confirmationStatus,
    intentSummary_slotToElicit,
    intentSummary_dialogActionType,

    -- ** PredictedIntent
    predictedIntent_nluIntentConfidence,
    predictedIntent_slots,
    predictedIntent_intentName,

    -- ** ResponseCard
    responseCard_genericAttachments,
    responseCard_version,
    responseCard_contentType,

    -- ** SentimentResponse
    sentimentResponse_sentimentScore,
    sentimentResponse_sentimentLabel,
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
