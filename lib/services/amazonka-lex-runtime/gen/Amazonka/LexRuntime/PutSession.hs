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
-- Module      : Amazonka.LexRuntime.PutSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new session or modifies an existing session with an Amazon Lex
-- bot. Use this operation to enable your application to set the state of
-- the bot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/how-session-api.html Managing Sessions>.
module Amazonka.LexRuntime.PutSession
  ( -- * Creating a Request
    PutSession (..),
    newPutSession,

    -- * Request Lenses
    putSession_accept,
    putSession_activeContexts,
    putSession_dialogAction,
    putSession_recentIntentSummaryView,
    putSession_sessionAttributes,
    putSession_botName,
    putSession_botAlias,
    putSession_userId,

    -- * Destructuring the Response
    PutSessionResponse (..),
    newPutSessionResponse,

    -- * Response Lenses
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
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexRuntime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSession' smart constructor.
data PutSession = PutSession'
  { -- | The message that Amazon Lex returns in the response can be either text
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
    accept :: Prelude.Maybe Prelude.Text,
    -- | A list of contexts active for the request. A context can be activated
    -- when a previous intent is fulfilled, or by including the context in the
    -- request,
    --
    -- If you don\'t specify a list of contexts, Amazon Lex will use the
    -- current list of contexts for the session. If you specify an empty list,
    -- all contexts for the session are cleared.
    activeContexts :: Prelude.Maybe (Data.Sensitive [ActiveContext]),
    -- | Sets the next action that the bot should take to fulfill the
    -- conversation.
    dialogAction :: Prelude.Maybe DialogAction,
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
    recentIntentSummaryView :: Prelude.Maybe [IntentSummary],
    -- | Map of key\/value pairs representing the session-specific context
    -- information. It contains application information passed between Amazon
    -- Lex and a client application.
    sessionAttributes :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the bot that contains the session data.
    botName :: Prelude.Text,
    -- | The alias in use for the bot that contains the session data.
    botAlias :: Prelude.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify
    -- a user\'s conversation with your bot.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'activeContexts', 'putSession_activeContexts' - A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
--
-- 'dialogAction', 'putSession_dialogAction' - Sets the next action that the bot should take to fulfill the
-- conversation.
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
-- 'sessionAttributes', 'putSession_sessionAttributes' - Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
--
-- 'botName', 'putSession_botName' - The name of the bot that contains the session data.
--
-- 'botAlias', 'putSession_botAlias' - The alias in use for the bot that contains the session data.
--
-- 'userId', 'putSession_userId' - The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
newPutSession ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'botAlias'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  PutSession
newPutSession pBotName_ pBotAlias_ pUserId_ =
  PutSession'
    { accept = Prelude.Nothing,
      activeContexts = Prelude.Nothing,
      dialogAction = Prelude.Nothing,
      recentIntentSummaryView = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_
    }

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
putSession_accept :: Lens.Lens' PutSession (Prelude.Maybe Prelude.Text)
putSession_accept = Lens.lens (\PutSession' {accept} -> accept) (\s@PutSession' {} a -> s {accept = a} :: PutSession)

-- | A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
putSession_activeContexts :: Lens.Lens' PutSession (Prelude.Maybe [ActiveContext])
putSession_activeContexts = Lens.lens (\PutSession' {activeContexts} -> activeContexts) (\s@PutSession' {} a -> s {activeContexts = a} :: PutSession) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Sets the next action that the bot should take to fulfill the
-- conversation.
putSession_dialogAction :: Lens.Lens' PutSession (Prelude.Maybe DialogAction)
putSession_dialogAction = Lens.lens (\PutSession' {dialogAction} -> dialogAction) (\s@PutSession' {} a -> s {dialogAction = a} :: PutSession)

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
putSession_recentIntentSummaryView :: Lens.Lens' PutSession (Prelude.Maybe [IntentSummary])
putSession_recentIntentSummaryView = Lens.lens (\PutSession' {recentIntentSummaryView} -> recentIntentSummaryView) (\s@PutSession' {} a -> s {recentIntentSummaryView = a} :: PutSession) Prelude.. Lens.mapping Lens.coerced

-- | Map of key\/value pairs representing the session-specific context
-- information. It contains application information passed between Amazon
-- Lex and a client application.
putSession_sessionAttributes :: Lens.Lens' PutSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putSession_sessionAttributes = Lens.lens (\PutSession' {sessionAttributes} -> sessionAttributes) (\s@PutSession' {} a -> s {sessionAttributes = a} :: PutSession) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The name of the bot that contains the session data.
putSession_botName :: Lens.Lens' PutSession Prelude.Text
putSession_botName = Lens.lens (\PutSession' {botName} -> botName) (\s@PutSession' {} a -> s {botName = a} :: PutSession)

-- | The alias in use for the bot that contains the session data.
putSession_botAlias :: Lens.Lens' PutSession Prelude.Text
putSession_botAlias = Lens.lens (\PutSession' {botAlias} -> botAlias) (\s@PutSession' {} a -> s {botAlias = a} :: PutSession)

-- | The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot.
putSession_userId :: Lens.Lens' PutSession Prelude.Text
putSession_userId = Lens.lens (\PutSession' {userId} -> userId) (\s@PutSession' {} a -> s {userId = a} :: PutSession)

instance Core.AWSRequest PutSession where
  type AWSResponse PutSession = PutSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          PutSessionResponse'
            Prelude.<$> (h Data..#? "x-amz-lex-active-contexts")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "x-amz-lex-dialog-state")
            Prelude.<*> (h Data..#? "x-amz-lex-encoded-message")
            Prelude.<*> (h Data..#? "x-amz-lex-intent-name")
            Prelude.<*> (h Data..#? "x-amz-lex-message")
            Prelude.<*> (h Data..#? "x-amz-lex-message-format")
            Prelude.<*> (h Data..#? "x-amz-lex-session-attributes")
            Prelude.<*> (h Data..#? "x-amz-lex-session-id")
            Prelude.<*> (h Data..#? "x-amz-lex-slot-to-elicit")
            Prelude.<*> (h Data..#? "x-amz-lex-slots")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable PutSession where
  hashWithSalt _salt PutSession' {..} =
    _salt
      `Prelude.hashWithSalt` accept
      `Prelude.hashWithSalt` activeContexts
      `Prelude.hashWithSalt` dialogAction
      `Prelude.hashWithSalt` recentIntentSummaryView
      `Prelude.hashWithSalt` sessionAttributes
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` botAlias
      `Prelude.hashWithSalt` userId

instance Prelude.NFData PutSession where
  rnf PutSession' {..} =
    Prelude.rnf accept `Prelude.seq`
      Prelude.rnf activeContexts `Prelude.seq`
        Prelude.rnf dialogAction `Prelude.seq`
          Prelude.rnf recentIntentSummaryView `Prelude.seq`
            Prelude.rnf sessionAttributes `Prelude.seq`
              Prelude.rnf botName `Prelude.seq`
                Prelude.rnf botAlias `Prelude.seq`
                  Prelude.rnf userId

instance Data.ToHeaders PutSession where
  toHeaders PutSession' {..} =
    Prelude.mconcat
      [ "Accept" Data.=# accept,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON PutSession where
  toJSON PutSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("activeContexts" Data..=)
              Prelude.<$> activeContexts,
            ("dialogAction" Data..=) Prelude.<$> dialogAction,
            ("recentIntentSummaryView" Data..=)
              Prelude.<$> recentIntentSummaryView,
            ("sessionAttributes" Data..=)
              Prelude.<$> sessionAttributes
          ]
      )

instance Data.ToPath PutSession where
  toPath PutSession' {..} =
    Prelude.mconcat
      [ "/bot/",
        Data.toBS botName,
        "/alias/",
        Data.toBS botAlias,
        "/user/",
        Data.toBS userId,
        "/session"
      ]

instance Data.ToQuery PutSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSessionResponse' smart constructor.
data PutSessionResponse = PutSessionResponse'
  { -- | A list of active contexts for the session.
    activeContexts :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Content type as specified in the @Accept@ HTTP header in the request.
    contentType :: Prelude.Maybe Prelude.Text,
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
    dialogState :: Prelude.Maybe DialogState,
    -- | The next message that should be presented to the user.
    --
    -- The @encodedMessage@ field is base-64 encoded. You must decode the field
    -- before you can use the value.
    encodedMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the current intent.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | The next message that should be presented to the user.
    --
    -- You can only use this field in the de-DE, en-AU, en-GB, en-US, es-419,
    -- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
    -- @message@ field is null. You should use the @encodedMessage@ field
    -- instead.
    message :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
    messageFormat :: Prelude.Maybe MessageFormatType,
    -- | Map of key\/value pairs representing session-specific context
    -- information.
    sessionAttributes :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | If the @dialogState@ is @ElicitSlot@, returns the name of the slot for
    -- which Amazon Lex is eliciting a value.
    slotToElicit :: Prelude.Maybe Prelude.Text,
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
    slots :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The audio version of the message to convey to the user.
    audioStream :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeContexts', 'putSessionResponse_activeContexts' - A list of active contexts for the session.
--
-- 'contentType', 'putSessionResponse_contentType' - Content type as specified in the @Accept@ HTTP header in the request.
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
-- 'encodedMessage', 'putSessionResponse_encodedMessage' - The next message that should be presented to the user.
--
-- The @encodedMessage@ field is base-64 encoded. You must decode the field
-- before you can use the value.
--
-- 'intentName', 'putSessionResponse_intentName' - The name of the current intent.
--
-- 'message', 'putSessionResponse_message' - The next message that should be presented to the user.
--
-- You can only use this field in the de-DE, en-AU, en-GB, en-US, es-419,
-- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
-- @message@ field is null. You should use the @encodedMessage@ field
-- instead.
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
-- 'sessionAttributes', 'putSessionResponse_sessionAttributes' - Map of key\/value pairs representing session-specific context
-- information.
--
-- 'sessionId', 'putSessionResponse_sessionId' - A unique identifier for the session.
--
-- 'slotToElicit', 'putSessionResponse_slotToElicit' - If the @dialogState@ is @ElicitSlot@, returns the name of the slot for
-- which Amazon Lex is eliciting a value.
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
-- 'httpStatus', 'putSessionResponse_httpStatus' - The response's http status code.
--
-- 'audioStream', 'putSessionResponse_audioStream' - The audio version of the message to convey to the user.
newPutSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'audioStream'
  Data.ResponseBody ->
  PutSessionResponse
newPutSessionResponse pHttpStatus_ pAudioStream_ =
  PutSessionResponse'
    { activeContexts =
        Prelude.Nothing,
      contentType = Prelude.Nothing,
      dialogState = Prelude.Nothing,
      encodedMessage = Prelude.Nothing,
      intentName = Prelude.Nothing,
      message = Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      slotToElicit = Prelude.Nothing,
      slots = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      audioStream = pAudioStream_
    }

-- | A list of active contexts for the session.
putSessionResponse_activeContexts :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_activeContexts = Lens.lens (\PutSessionResponse' {activeContexts} -> activeContexts) (\s@PutSessionResponse' {} a -> s {activeContexts = a} :: PutSessionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Content type as specified in the @Accept@ HTTP header in the request.
putSessionResponse_contentType :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_contentType = Lens.lens (\PutSessionResponse' {contentType} -> contentType) (\s@PutSessionResponse' {} a -> s {contentType = a} :: PutSessionResponse)

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
putSessionResponse_dialogState :: Lens.Lens' PutSessionResponse (Prelude.Maybe DialogState)
putSessionResponse_dialogState = Lens.lens (\PutSessionResponse' {dialogState} -> dialogState) (\s@PutSessionResponse' {} a -> s {dialogState = a} :: PutSessionResponse)

-- | The next message that should be presented to the user.
--
-- The @encodedMessage@ field is base-64 encoded. You must decode the field
-- before you can use the value.
putSessionResponse_encodedMessage :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_encodedMessage = Lens.lens (\PutSessionResponse' {encodedMessage} -> encodedMessage) (\s@PutSessionResponse' {} a -> s {encodedMessage = a} :: PutSessionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the current intent.
putSessionResponse_intentName :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_intentName = Lens.lens (\PutSessionResponse' {intentName} -> intentName) (\s@PutSessionResponse' {} a -> s {intentName = a} :: PutSessionResponse)

-- | The next message that should be presented to the user.
--
-- You can only use this field in the de-DE, en-AU, en-GB, en-US, es-419,
-- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
-- @message@ field is null. You should use the @encodedMessage@ field
-- instead.
putSessionResponse_message :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_message = Lens.lens (\PutSessionResponse' {message} -> message) (\s@PutSessionResponse' {} a -> s {message = a} :: PutSessionResponse) Prelude.. Lens.mapping Data._Sensitive

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
putSessionResponse_messageFormat :: Lens.Lens' PutSessionResponse (Prelude.Maybe MessageFormatType)
putSessionResponse_messageFormat = Lens.lens (\PutSessionResponse' {messageFormat} -> messageFormat) (\s@PutSessionResponse' {} a -> s {messageFormat = a} :: PutSessionResponse)

-- | Map of key\/value pairs representing session-specific context
-- information.
putSessionResponse_sessionAttributes :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_sessionAttributes = Lens.lens (\PutSessionResponse' {sessionAttributes} -> sessionAttributes) (\s@PutSessionResponse' {} a -> s {sessionAttributes = a} :: PutSessionResponse)

-- | A unique identifier for the session.
putSessionResponse_sessionId :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_sessionId = Lens.lens (\PutSessionResponse' {sessionId} -> sessionId) (\s@PutSessionResponse' {} a -> s {sessionId = a} :: PutSessionResponse)

-- | If the @dialogState@ is @ElicitSlot@, returns the name of the slot for
-- which Amazon Lex is eliciting a value.
putSessionResponse_slotToElicit :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_slotToElicit = Lens.lens (\PutSessionResponse' {slotToElicit} -> slotToElicit) (\s@PutSessionResponse' {} a -> s {slotToElicit = a} :: PutSessionResponse)

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
putSessionResponse_slots :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_slots = Lens.lens (\PutSessionResponse' {slots} -> slots) (\s@PutSessionResponse' {} a -> s {slots = a} :: PutSessionResponse)

-- | The response's http status code.
putSessionResponse_httpStatus :: Lens.Lens' PutSessionResponse Prelude.Int
putSessionResponse_httpStatus = Lens.lens (\PutSessionResponse' {httpStatus} -> httpStatus) (\s@PutSessionResponse' {} a -> s {httpStatus = a} :: PutSessionResponse)

-- | The audio version of the message to convey to the user.
putSessionResponse_audioStream :: Lens.Lens' PutSessionResponse Data.ResponseBody
putSessionResponse_audioStream = Lens.lens (\PutSessionResponse' {audioStream} -> audioStream) (\s@PutSessionResponse' {} a -> s {audioStream = a} :: PutSessionResponse)
