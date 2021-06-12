{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.PutSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new session or modifies an existing session with an Amazon Lex
-- bot. Use this operation to enable your application to set the state of
-- the bot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/how-session-api.html Managing Sessions>.
module Network.AWS.LexRuntime.PutSession
  ( -- * Creating a Request
    PutSession (..),
    newPutSession,

    -- * Request Lenses
    putSession_sessionAttributes,
    putSession_dialogAction,
    putSession_accept,
    putSession_recentIntentSummaryView,
    putSession_activeContexts,
    putSession_botName,
    putSession_botAlias,
    putSession_userId,

    -- * Destructuring the Response
    PutSessionResponse (..),
    newPutSessionResponse,

    -- * Response Lenses
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
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutSession' smart constructor.
data PutSession = PutSession'
  { -- | Map of key\/value pairs representing the session-specific context
    -- information. It contains application information passed between Amazon
    -- Lex and a client application.
    sessionAttributes :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | Sets the next action that the bot should take to fulfill the
    -- conversation.
    dialogAction :: Core.Maybe DialogAction,
    -- | The message that Amazon Lex returns in the response can be either text
    -- or speech based depending on the value of this field.
    --
    -- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex returns
    --     text in the response.
    --
    -- -   If the value begins with @audio\/@, Amazon Lex returns speech in the
    --     response. Amazon Lex uses Amazon Polly to generate the speech in the
    --     configuration that you specify. For example, if you specify
    --     @audio\/mpeg@ as the value, Amazon Lex returns speech in the MPEG
    --     format.
    --
    -- -   If the value is @audio\/pcm@, the speech is returned as @audio\/pcm@
    --     in 16-bit, little endian format.
    --
    -- -   The following are the accepted values:
    --
    --     -   @audio\/mpeg@
    --
    --     -   @audio\/ogg@
    --
    --     -   @audio\/pcm@
    --
    --     -   @audio\/*@ (defaults to mpeg)
    --
    --     -   @text\/plain; charset=utf-8@
    accept :: Core.Maybe Core.Text,
    -- | A summary of the recent intents for the bot. You can use the intent
    -- summary view to set a checkpoint label on an intent and modify
    -- attributes of intents. You can also use it to remove or add intent
    -- summary objects to the list.
    --
    -- An intent that you modify or add to the list must make sense for the
    -- bot. For example, the intent name must be valid for the bot. You must
    -- provide valid values for:
    --
    -- -   @intentName@
    --
    -- -   slot names
    --
    -- -   @slotToElict@
    --
    -- If you send the @recentIntentSummaryView@ parameter in a @PutSession@
    -- request, the contents of the new summary view replaces the old summary
    -- view. For example, if a @GetSession@ request returns three intents in
    -- the summary view and you call @PutSession@ with one intent in the
    -- summary view, the next call to @GetSession@ will only return one intent.
    recentIntentSummaryView :: Core.Maybe [IntentSummary],
    -- | A list of contexts active for the request. A context can be activated
    -- when a previous intent is fulfilled, or by including the context in the
    -- request,
    --
    -- If you don\'t specify a list of contexts, Amazon Lex will use the
    -- current list of contexts for the session. If you specify an empty list,
    -- all contexts for the session are cleared.
    activeContexts :: Core.Maybe (Core.Sensitive [ActiveContext]),
    -- | The name of the bot that contains the session data.
    botName :: Core.Text,
    -- | The alias in use for the bot that contains the session data.
    botAlias :: Core.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify
    -- a user\'s conversation with your bot.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionAttributes', 'putSession_sessionAttributes' - Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
--
-- 'dialogAction', 'putSession_dialogAction' - Sets the next action that the bot should take to fulfill the
-- conversation.
--
-- 'accept', 'putSession_accept' - The message that Amazon Lex returns in the response can be either text
-- or speech based depending on the value of this field.
--
-- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex returns speech in the
--     response. Amazon Lex uses Amazon Polly to generate the speech in the
--     configuration that you specify. For example, if you specify
--     @audio\/mpeg@ as the value, Amazon Lex returns speech in the MPEG
--     format.
--
-- -   If the value is @audio\/pcm@, the speech is returned as @audio\/pcm@
--     in 16-bit, little endian format.
--
-- -   The following are the accepted values:
--
--     -   @audio\/mpeg@
--
--     -   @audio\/ogg@
--
--     -   @audio\/pcm@
--
--     -   @audio\/*@ (defaults to mpeg)
--
--     -   @text\/plain; charset=utf-8@
--
-- 'recentIntentSummaryView', 'putSession_recentIntentSummaryView' - A summary of the recent intents for the bot. You can use the intent
-- summary view to set a checkpoint label on an intent and modify
-- attributes of intents. You can also use it to remove or add intent
-- summary objects to the list.
--
-- An intent that you modify or add to the list must make sense for the
-- bot. For example, the intent name must be valid for the bot. You must
-- provide valid values for:
--
-- -   @intentName@
--
-- -   slot names
--
-- -   @slotToElict@
--
-- If you send the @recentIntentSummaryView@ parameter in a @PutSession@
-- request, the contents of the new summary view replaces the old summary
-- view. For example, if a @GetSession@ request returns three intents in
-- the summary view and you call @PutSession@ with one intent in the
-- summary view, the next call to @GetSession@ will only return one intent.
--
-- 'activeContexts', 'putSession_activeContexts' - A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
--
-- 'botName', 'putSession_botName' - The name of the bot that contains the session data.
--
-- 'botAlias', 'putSession_botAlias' - The alias in use for the bot that contains the session data.
--
-- 'userId', 'putSession_userId' - The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
newPutSession ::
  -- | 'botName'
  Core.Text ->
  -- | 'botAlias'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  PutSession
newPutSession pBotName_ pBotAlias_ pUserId_ =
  PutSession'
    { sessionAttributes = Core.Nothing,
      dialogAction = Core.Nothing,
      accept = Core.Nothing,
      recentIntentSummaryView = Core.Nothing,
      activeContexts = Core.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_
    }

-- | Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
putSession_sessionAttributes :: Lens.Lens' PutSession (Core.Maybe (Core.HashMap Core.Text Core.Text))
putSession_sessionAttributes = Lens.lens (\PutSession' {sessionAttributes} -> sessionAttributes) (\s@PutSession' {} a -> s {sessionAttributes = a} :: PutSession) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | Sets the next action that the bot should take to fulfill the
-- conversation.
putSession_dialogAction :: Lens.Lens' PutSession (Core.Maybe DialogAction)
putSession_dialogAction = Lens.lens (\PutSession' {dialogAction} -> dialogAction) (\s@PutSession' {} a -> s {dialogAction = a} :: PutSession)

-- | The message that Amazon Lex returns in the response can be either text
-- or speech based depending on the value of this field.
--
-- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex returns speech in the
--     response. Amazon Lex uses Amazon Polly to generate the speech in the
--     configuration that you specify. For example, if you specify
--     @audio\/mpeg@ as the value, Amazon Lex returns speech in the MPEG
--     format.
--
-- -   If the value is @audio\/pcm@, the speech is returned as @audio\/pcm@
--     in 16-bit, little endian format.
--
-- -   The following are the accepted values:
--
--     -   @audio\/mpeg@
--
--     -   @audio\/ogg@
--
--     -   @audio\/pcm@
--
--     -   @audio\/*@ (defaults to mpeg)
--
--     -   @text\/plain; charset=utf-8@
putSession_accept :: Lens.Lens' PutSession (Core.Maybe Core.Text)
putSession_accept = Lens.lens (\PutSession' {accept} -> accept) (\s@PutSession' {} a -> s {accept = a} :: PutSession)

-- | A summary of the recent intents for the bot. You can use the intent
-- summary view to set a checkpoint label on an intent and modify
-- attributes of intents. You can also use it to remove or add intent
-- summary objects to the list.
--
-- An intent that you modify or add to the list must make sense for the
-- bot. For example, the intent name must be valid for the bot. You must
-- provide valid values for:
--
-- -   @intentName@
--
-- -   slot names
--
-- -   @slotToElict@
--
-- If you send the @recentIntentSummaryView@ parameter in a @PutSession@
-- request, the contents of the new summary view replaces the old summary
-- view. For example, if a @GetSession@ request returns three intents in
-- the summary view and you call @PutSession@ with one intent in the
-- summary view, the next call to @GetSession@ will only return one intent.
putSession_recentIntentSummaryView :: Lens.Lens' PutSession (Core.Maybe [IntentSummary])
putSession_recentIntentSummaryView = Lens.lens (\PutSession' {recentIntentSummaryView} -> recentIntentSummaryView) (\s@PutSession' {} a -> s {recentIntentSummaryView = a} :: PutSession) Core.. Lens.mapping Lens._Coerce

-- | A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
putSession_activeContexts :: Lens.Lens' PutSession (Core.Maybe [ActiveContext])
putSession_activeContexts = Lens.lens (\PutSession' {activeContexts} -> activeContexts) (\s@PutSession' {} a -> s {activeContexts = a} :: PutSession) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The name of the bot that contains the session data.
putSession_botName :: Lens.Lens' PutSession Core.Text
putSession_botName = Lens.lens (\PutSession' {botName} -> botName) (\s@PutSession' {} a -> s {botName = a} :: PutSession)

-- | The alias in use for the bot that contains the session data.
putSession_botAlias :: Lens.Lens' PutSession Core.Text
putSession_botAlias = Lens.lens (\PutSession' {botAlias} -> botAlias) (\s@PutSession' {} a -> s {botAlias = a} :: PutSession)

-- | The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
putSession_userId :: Lens.Lens' PutSession Core.Text
putSession_userId = Lens.lens (\PutSession' {userId} -> userId) (\s@PutSession' {} a -> s {userId = a} :: PutSession)

instance Core.AWSRequest PutSession where
  type AWSResponse PutSession = PutSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          PutSessionResponse'
            Core.<$> (h Core..#? "x-amz-lex-dialog-state")
            Core.<*> (h Core..#? "x-amz-lex-session-attributes")
            Core.<*> (h Core..#? "Content-Type")
            Core.<*> (h Core..#? "x-amz-lex-message")
            Core.<*> (h Core..#? "x-amz-lex-session-id")
            Core.<*> (h Core..#? "x-amz-lex-intent-name")
            Core.<*> (h Core..#? "x-amz-lex-message-format")
            Core.<*> (h Core..#? "x-amz-lex-slots")
            Core.<*> (h Core..#? "x-amz-lex-slot-to-elicit")
            Core.<*> (h Core..#? "x-amz-lex-active-contexts")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.pure x)
      )

instance Core.Hashable PutSession

instance Core.NFData PutSession

instance Core.ToHeaders PutSession where
  toHeaders PutSession' {..} =
    Core.mconcat
      [ "Accept" Core.=# accept,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON PutSession where
  toJSON PutSession' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sessionAttributes" Core..=)
              Core.<$> sessionAttributes,
            ("dialogAction" Core..=) Core.<$> dialogAction,
            ("recentIntentSummaryView" Core..=)
              Core.<$> recentIntentSummaryView,
            ("activeContexts" Core..=) Core.<$> activeContexts
          ]
      )

instance Core.ToPath PutSession where
  toPath PutSession' {..} =
    Core.mconcat
      [ "/bot/",
        Core.toBS botName,
        "/alias/",
        Core.toBS botAlias,
        "/user/",
        Core.toBS userId,
        "/session"
      ]

instance Core.ToQuery PutSession where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutSessionResponse' smart constructor.
data PutSessionResponse = PutSessionResponse'
  { -- | -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
    --     response to confirm the intent before fulfilling an intent.
    --
    -- -   @ElicitIntent@ - Amazon Lex wants to elicit the user\'s intent.
    --
    -- -   @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the
    --     current intent.
    --
    -- -   @Failed@ - Conveys that the conversation with the user has failed.
    --     This can happen for various reasons, including the user does not
    --     provide an appropriate response to prompts from the service, or if
    --     the Lambda function fails to fulfill the intent.
    --
    -- -   @Fulfilled@ - Conveys that the Lambda function has sucessfully
    --     fulfilled the intent.
    --
    -- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
    --     intent.
    dialogState :: Core.Maybe DialogState,
    -- | Map of key\/value pairs representing session-specific context
    -- information.
    sessionAttributes :: Core.Maybe Core.Text,
    -- | Content type as specified in the @Accept@ HTTP header in the request.
    contentType :: Core.Maybe Core.Text,
    -- | The next message that should be presented to the user.
    message :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A unique identifier for the session.
    sessionId :: Core.Maybe Core.Text,
    -- | The name of the current intent.
    intentName :: Core.Maybe Core.Text,
    -- | The format of the response message. One of the following values:
    --
    -- -   @PlainText@ - The message contains plain UTF-8 text.
    --
    -- -   @CustomPayload@ - The message is a custom format for the client.
    --
    -- -   @SSML@ - The message contains text formatted for voice output.
    --
    -- -   @Composite@ - The message contains an escaped JSON object containing
    --     one or more messages from the groups that messages were assigned to
    --     when the intent was created.
    messageFormat :: Core.Maybe MessageFormatType,
    -- | Map of zero or more intent slots Amazon Lex detected from the user input
    -- during the conversation.
    --
    -- Amazon Lex creates a resolution list containing likely values for a
    -- slot. The value that it returns is determined by the
    -- @valueSelectionStrategy@ selected when the slot type was created or
    -- updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@, the
    -- value provided by the user is returned, if the user value is similar to
    -- the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@
    -- Amazon Lex returns the first value in the resolution list or, if there
    -- is no resolution list, null. If you don\'t specify a
    -- @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@.
    slots :: Core.Maybe Core.Text,
    -- | If the @dialogState@ is @ElicitSlot@, returns the name of the slot for
    -- which Amazon Lex is eliciting a value.
    slotToElicit :: Core.Maybe Core.Text,
    -- | A list of active contexts for the session.
    activeContexts :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The audio version of the message to convey to the user.
    audioStream :: Core.ResponseBody
  }
  deriving (Core.Show, Core.Generic)

-- |
-- Create a value of 'PutSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialogState', 'putSessionResponse_dialogState' - -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
--     response to confirm the intent before fulfilling an intent.
--
-- -   @ElicitIntent@ - Amazon Lex wants to elicit the user\'s intent.
--
-- -   @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the
--     current intent.
--
-- -   @Failed@ - Conveys that the conversation with the user has failed.
--     This can happen for various reasons, including the user does not
--     provide an appropriate response to prompts from the service, or if
--     the Lambda function fails to fulfill the intent.
--
-- -   @Fulfilled@ - Conveys that the Lambda function has sucessfully
--     fulfilled the intent.
--
-- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
--     intent.
--
-- 'sessionAttributes', 'putSessionResponse_sessionAttributes' - Map of key\/value pairs representing session-specific context
-- information.
--
-- 'contentType', 'putSessionResponse_contentType' - Content type as specified in the @Accept@ HTTP header in the request.
--
-- 'message', 'putSessionResponse_message' - The next message that should be presented to the user.
--
-- 'sessionId', 'putSessionResponse_sessionId' - A unique identifier for the session.
--
-- 'intentName', 'putSessionResponse_intentName' - The name of the current intent.
--
-- 'messageFormat', 'putSessionResponse_messageFormat' - The format of the response message. One of the following values:
--
-- -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format for the client.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages from the groups that messages were assigned to
--     when the intent was created.
--
-- 'slots', 'putSessionResponse_slots' - Map of zero or more intent slots Amazon Lex detected from the user input
-- during the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a
-- slot. The value that it returns is determined by the
-- @valueSelectionStrategy@ selected when the slot type was created or
-- updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@, the
-- value provided by the user is returned, if the user value is similar to
-- the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@
-- Amazon Lex returns the first value in the resolution list or, if there
-- is no resolution list, null. If you don\'t specify a
-- @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@.
--
-- 'slotToElicit', 'putSessionResponse_slotToElicit' - If the @dialogState@ is @ElicitSlot@, returns the name of the slot for
-- which Amazon Lex is eliciting a value.
--
-- 'activeContexts', 'putSessionResponse_activeContexts' - A list of active contexts for the session.
--
-- 'httpStatus', 'putSessionResponse_httpStatus' - The response's http status code.
--
-- 'audioStream', 'putSessionResponse_audioStream' - The audio version of the message to convey to the user.
newPutSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'audioStream'
  Core.ResponseBody ->
  PutSessionResponse
newPutSessionResponse pHttpStatus_ pAudioStream_ =
  PutSessionResponse'
    { dialogState = Core.Nothing,
      sessionAttributes = Core.Nothing,
      contentType = Core.Nothing,
      message = Core.Nothing,
      sessionId = Core.Nothing,
      intentName = Core.Nothing,
      messageFormat = Core.Nothing,
      slots = Core.Nothing,
      slotToElicit = Core.Nothing,
      activeContexts = Core.Nothing,
      httpStatus = pHttpStatus_,
      audioStream = pAudioStream_
    }

-- | -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
--     response to confirm the intent before fulfilling an intent.
--
-- -   @ElicitIntent@ - Amazon Lex wants to elicit the user\'s intent.
--
-- -   @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the
--     current intent.
--
-- -   @Failed@ - Conveys that the conversation with the user has failed.
--     This can happen for various reasons, including the user does not
--     provide an appropriate response to prompts from the service, or if
--     the Lambda function fails to fulfill the intent.
--
-- -   @Fulfilled@ - Conveys that the Lambda function has sucessfully
--     fulfilled the intent.
--
-- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
--     intent.
putSessionResponse_dialogState :: Lens.Lens' PutSessionResponse (Core.Maybe DialogState)
putSessionResponse_dialogState = Lens.lens (\PutSessionResponse' {dialogState} -> dialogState) (\s@PutSessionResponse' {} a -> s {dialogState = a} :: PutSessionResponse)

-- | Map of key\/value pairs representing session-specific context
-- information.
putSessionResponse_sessionAttributes :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_sessionAttributes = Lens.lens (\PutSessionResponse' {sessionAttributes} -> sessionAttributes) (\s@PutSessionResponse' {} a -> s {sessionAttributes = a} :: PutSessionResponse)

-- | Content type as specified in the @Accept@ HTTP header in the request.
putSessionResponse_contentType :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_contentType = Lens.lens (\PutSessionResponse' {contentType} -> contentType) (\s@PutSessionResponse' {} a -> s {contentType = a} :: PutSessionResponse)

-- | The next message that should be presented to the user.
putSessionResponse_message :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_message = Lens.lens (\PutSessionResponse' {message} -> message) (\s@PutSessionResponse' {} a -> s {message = a} :: PutSessionResponse) Core.. Lens.mapping Core._Sensitive

-- | A unique identifier for the session.
putSessionResponse_sessionId :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_sessionId = Lens.lens (\PutSessionResponse' {sessionId} -> sessionId) (\s@PutSessionResponse' {} a -> s {sessionId = a} :: PutSessionResponse)

-- | The name of the current intent.
putSessionResponse_intentName :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_intentName = Lens.lens (\PutSessionResponse' {intentName} -> intentName) (\s@PutSessionResponse' {} a -> s {intentName = a} :: PutSessionResponse)

-- | The format of the response message. One of the following values:
--
-- -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format for the client.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages from the groups that messages were assigned to
--     when the intent was created.
putSessionResponse_messageFormat :: Lens.Lens' PutSessionResponse (Core.Maybe MessageFormatType)
putSessionResponse_messageFormat = Lens.lens (\PutSessionResponse' {messageFormat} -> messageFormat) (\s@PutSessionResponse' {} a -> s {messageFormat = a} :: PutSessionResponse)

-- | Map of zero or more intent slots Amazon Lex detected from the user input
-- during the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a
-- slot. The value that it returns is determined by the
-- @valueSelectionStrategy@ selected when the slot type was created or
-- updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@, the
-- value provided by the user is returned, if the user value is similar to
-- the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@
-- Amazon Lex returns the first value in the resolution list or, if there
-- is no resolution list, null. If you don\'t specify a
-- @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@.
putSessionResponse_slots :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_slots = Lens.lens (\PutSessionResponse' {slots} -> slots) (\s@PutSessionResponse' {} a -> s {slots = a} :: PutSessionResponse)

-- | If the @dialogState@ is @ElicitSlot@, returns the name of the slot for
-- which Amazon Lex is eliciting a value.
putSessionResponse_slotToElicit :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_slotToElicit = Lens.lens (\PutSessionResponse' {slotToElicit} -> slotToElicit) (\s@PutSessionResponse' {} a -> s {slotToElicit = a} :: PutSessionResponse)

-- | A list of active contexts for the session.
putSessionResponse_activeContexts :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
putSessionResponse_activeContexts = Lens.lens (\PutSessionResponse' {activeContexts} -> activeContexts) (\s@PutSessionResponse' {} a -> s {activeContexts = a} :: PutSessionResponse) Core.. Lens.mapping Core._Sensitive

-- | The response's http status code.
putSessionResponse_httpStatus :: Lens.Lens' PutSessionResponse Core.Int
putSessionResponse_httpStatus = Lens.lens (\PutSessionResponse' {httpStatus} -> httpStatus) (\s@PutSessionResponse' {} a -> s {httpStatus = a} :: PutSessionResponse)

-- | The audio version of the message to convey to the user.
putSessionResponse_audioStream :: Lens.Lens' PutSessionResponse Core.ResponseBody
putSessionResponse_audioStream = Lens.lens (\PutSessionResponse' {audioStream} -> audioStream) (\s@PutSessionResponse' {} a -> s {audioStream = a} :: PutSessionResponse)
