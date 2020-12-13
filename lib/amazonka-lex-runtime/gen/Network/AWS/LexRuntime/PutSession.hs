{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.PutSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new session or modifies an existing session with an Amazon Lex bot. Use this operation to enable your application to set the state of the bot.
--
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-session-api.html Managing Sessions> .
module Network.AWS.LexRuntime.PutSession
  ( -- * Creating a request
    PutSession (..),
    mkPutSession,

    -- ** Request lenses
    psBotAlias,
    psAccept,
    psBotName,
    psUserId,
    psActiveContexts,
    psRecentIntentSummaryView,
    psDialogAction,
    psSessionAttributes,

    -- * Destructuring the response
    PutSessionResponse (..),
    mkPutSessionResponse,

    -- ** Response lenses
    psrsSlots,
    psrsAudioStream,
    psrsIntentName,
    psrsDialogState,
    psrsActiveContexts,
    psrsMessageFormat,
    psrsMessage,
    psrsSessionId,
    psrsSlotToElicit,
    psrsContentType,
    psrsSessionAttributes,
    psrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutSession' smart constructor.
data PutSession = PutSession'
  { -- | The alias in use for the bot that contains the session data.
    botAlias :: Lude.Text,
    -- | The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.
    --
    --
    --     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.
    --
    --
    --     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
    --
    --
    --     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.
    --
    --
    --     * The following are the accepted values:
    --
    --     * @audio/mpeg@
    --
    --
    --     * @audio/ogg@
    --
    --
    --     * @audio/pcm@
    --
    --
    --     * @audio/*@ (defaults to mpeg)
    --
    --
    --     * @text/plain; charset=utf-8@
    accept :: Lude.Maybe Lude.Text,
    -- | The name of the bot that contains the session data.
    botName :: Lude.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
    userId :: Lude.Text,
    -- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
    --
    -- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
    activeContexts :: Lude.Maybe [ActiveContext],
    -- | A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list.
    --
    -- An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:
    --
    --     * @intentName@
    --
    --
    --     * slot names
    --
    --
    --     * @slotToElict@
    --
    --
    -- If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
    recentIntentSummaryView :: Lude.Maybe [IntentSummary],
    -- | Sets the next action that the bot should take to fulfill the conversation.
    dialogAction :: Lude.Maybe DialogAction,
    -- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
    sessionAttributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutSession' with the minimum fields required to make a request.
--
-- * 'botAlias' - The alias in use for the bot that contains the session data.
-- * 'accept' - The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.
--
--
--     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.
--
--
--     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
--
--
--     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.
--
--
--     * The following are the accepted values:
--
--     * @audio/mpeg@
--
--
--     * @audio/ogg@
--
--
--     * @audio/pcm@
--
--
--     * @audio/*@ (defaults to mpeg)
--
--
--     * @text/plain; charset=utf-8@
--
--
--
--
-- * 'botName' - The name of the bot that contains the session data.
-- * 'userId' - The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
-- * 'activeContexts' - A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
-- * 'recentIntentSummaryView' - A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list.
--
-- An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:
--
--     * @intentName@
--
--
--     * slot names
--
--
--     * @slotToElict@
--
--
-- If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
-- * 'dialogAction' - Sets the next action that the bot should take to fulfill the conversation.
-- * 'sessionAttributes' - Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
mkPutSession ::
  -- | 'botAlias'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  PutSession
mkPutSession pBotAlias_ pBotName_ pUserId_ =
  PutSession'
    { botAlias = pBotAlias_,
      accept = Lude.Nothing,
      botName = pBotName_,
      userId = pUserId_,
      activeContexts = Lude.Nothing,
      recentIntentSummaryView = Lude.Nothing,
      dialogAction = Lude.Nothing,
      sessionAttributes = Lude.Nothing
    }

-- | The alias in use for the bot that contains the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBotAlias :: Lens.Lens' PutSession Lude.Text
psBotAlias = Lens.lens (botAlias :: PutSession -> Lude.Text) (\s a -> s {botAlias = a} :: PutSession)
{-# DEPRECATED psBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.
--
--
--     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.
--
--
--     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
--
--
--     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.
--
--
--     * The following are the accepted values:
--
--     * @audio/mpeg@
--
--
--     * @audio/ogg@
--
--
--     * @audio/pcm@
--
--
--     * @audio/*@ (defaults to mpeg)
--
--
--     * @text/plain; charset=utf-8@
--
--
--
--
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAccept :: Lens.Lens' PutSession (Lude.Maybe Lude.Text)
psAccept = Lens.lens (accept :: PutSession -> Lude.Maybe Lude.Text) (\s a -> s {accept = a} :: PutSession)
{-# DEPRECATED psAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

-- | The name of the bot that contains the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBotName :: Lens.Lens' PutSession Lude.Text
psBotName = Lens.lens (botName :: PutSession -> Lude.Text) (\s a -> s {botName = a} :: PutSession)
{-# DEPRECATED psBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psUserId :: Lens.Lens' PutSession Lude.Text
psUserId = Lens.lens (userId :: PutSession -> Lude.Text) (\s a -> s {userId = a} :: PutSession)
{-# DEPRECATED psUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psActiveContexts :: Lens.Lens' PutSession (Lude.Maybe [ActiveContext])
psActiveContexts = Lens.lens (activeContexts :: PutSession -> Lude.Maybe [ActiveContext]) (\s a -> s {activeContexts = a} :: PutSession)
{-# DEPRECATED psActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list.
--
-- An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:
--
--     * @intentName@
--
--
--     * slot names
--
--
--     * @slotToElict@
--
--
-- If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
--
-- /Note:/ Consider using 'recentIntentSummaryView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psRecentIntentSummaryView :: Lens.Lens' PutSession (Lude.Maybe [IntentSummary])
psRecentIntentSummaryView = Lens.lens (recentIntentSummaryView :: PutSession -> Lude.Maybe [IntentSummary]) (\s a -> s {recentIntentSummaryView = a} :: PutSession)
{-# DEPRECATED psRecentIntentSummaryView "Use generic-lens or generic-optics with 'recentIntentSummaryView' instead." #-}

-- | Sets the next action that the bot should take to fulfill the conversation.
--
-- /Note:/ Consider using 'dialogAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDialogAction :: Lens.Lens' PutSession (Lude.Maybe DialogAction)
psDialogAction = Lens.lens (dialogAction :: PutSession -> Lude.Maybe DialogAction) (\s a -> s {dialogAction = a} :: PutSession)
{-# DEPRECATED psDialogAction "Use generic-lens or generic-optics with 'dialogAction' instead." #-}

-- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSessionAttributes :: Lens.Lens' PutSession (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
psSessionAttributes = Lens.lens (sessionAttributes :: PutSession -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {sessionAttributes = a} :: PutSession)
{-# DEPRECATED psSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

instance Lude.AWSRequest PutSession where
  type Rs PutSession = PutSessionResponse
  request = Req.postJSON lexRuntimeService
  response =
    Res.receiveBody
      ( \s h x ->
          PutSessionResponse'
            Lude.<$> (h Lude..#? "x-amz-lex-slots")
            Lude.<*> (Lude.pure x)
            Lude.<*> (h Lude..#? "x-amz-lex-intent-name")
            Lude.<*> (h Lude..#? "x-amz-lex-dialog-state")
            Lude.<*> (h Lude..#? "x-amz-lex-active-contexts")
            Lude.<*> (h Lude..#? "x-amz-lex-message-format")
            Lude.<*> (h Lude..#? "x-amz-lex-message")
            Lude.<*> (h Lude..#? "x-amz-lex-session-id")
            Lude.<*> (h Lude..#? "x-amz-lex-slot-to-elicit")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (h Lude..#? "x-amz-lex-session-attributes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutSession where
  toHeaders PutSession' {..} =
    Lude.mconcat
      [ "Accept" Lude.=# accept,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON PutSession where
  toJSON PutSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("activeContexts" Lude..=) Lude.<$> activeContexts,
            ("recentIntentSummaryView" Lude..=)
              Lude.<$> recentIntentSummaryView,
            ("dialogAction" Lude..=) Lude.<$> dialogAction,
            ("sessionAttributes" Lude..=) Lude.<$> sessionAttributes
          ]
      )

instance Lude.ToPath PutSession where
  toPath PutSession' {..} =
    Lude.mconcat
      [ "/bot/",
        Lude.toBS botName,
        "/alias/",
        Lude.toBS botAlias,
        "/user/",
        Lude.toBS userId,
        "/session"
      ]

instance Lude.ToQuery PutSession where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutSessionResponse' smart constructor.
data PutSessionResponse = PutSessionResponse'
  { -- | Map of zero or more intent slots Amazon Lex detected from the user input during the conversation.
    --
    -- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ .
    slots :: Lude.Maybe Lude.Text,
    -- | The audio version of the message to convey to the user.
    audioStream :: Lude.RsBody,
    -- | The name of the current intent.
    intentName :: Lude.Maybe Lude.Text,
    -- |
    --
    --
    --     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.
    --
    --
    --     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.
    --
    --
    --     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.
    --
    --
    --     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.
    --
    --
    --     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.
    --
    --
    --     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
    dialogState :: Lude.Maybe DialogState,
    -- | A list of active contexts for the session.
    activeContexts :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The format of the response message. One of the following values:
    --
    --
    --     * @PlainText@ - The message contains plain UTF-8 text.
    --
    --
    --     * @CustomPayload@ - The message is a custom format for the client.
    --
    --
    --     * @SSML@ - The message contains text formatted for voice output.
    --
    --
    --     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
    messageFormat :: Lude.Maybe MessageFormatType,
    -- | The next message that should be presented to the user.
    message :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | A unique identifier for the session.
    sessionId :: Lude.Maybe Lude.Text,
    -- | If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
    slotToElicit :: Lude.Maybe Lude.Text,
    -- | Content type as specified in the @Accept@ HTTP header in the request.
    contentType :: Lude.Maybe Lude.Text,
    -- | Map of key/value pairs representing session-specific context information.
    sessionAttributes :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'PutSessionResponse' with the minimum fields required to make a request.
--
-- * 'slots' - Map of zero or more intent slots Amazon Lex detected from the user input during the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ .
-- * 'audioStream' - The audio version of the message to convey to the user.
-- * 'intentName' - The name of the current intent.
-- * 'dialogState' -
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.
--
--
--     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.
--
--
--     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
--
-- * 'activeContexts' - A list of active contexts for the session.
-- * 'messageFormat' - The format of the response message. One of the following values:
--
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format for the client.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
--
-- * 'message' - The next message that should be presented to the user.
-- * 'sessionId' - A unique identifier for the session.
-- * 'slotToElicit' - If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
-- * 'contentType' - Content type as specified in the @Accept@ HTTP header in the request.
-- * 'sessionAttributes' - Map of key/value pairs representing session-specific context information.
-- * 'responseStatus' - The response status code.
mkPutSessionResponse ::
  -- | 'audioStream'
  Lude.RsBody ->
  -- | 'responseStatus'
  Lude.Int ->
  PutSessionResponse
mkPutSessionResponse pAudioStream_ pResponseStatus_ =
  PutSessionResponse'
    { slots = Lude.Nothing,
      audioStream = pAudioStream_,
      intentName = Lude.Nothing,
      dialogState = Lude.Nothing,
      activeContexts = Lude.Nothing,
      messageFormat = Lude.Nothing,
      message = Lude.Nothing,
      sessionId = Lude.Nothing,
      slotToElicit = Lude.Nothing,
      contentType = Lude.Nothing,
      sessionAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Map of zero or more intent slots Amazon Lex detected from the user input during the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ .
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsSlots :: Lens.Lens' PutSessionResponse (Lude.Maybe Lude.Text)
psrsSlots = Lens.lens (slots :: PutSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {slots = a} :: PutSessionResponse)
{-# DEPRECATED psrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The audio version of the message to convey to the user.
--
-- /Note:/ Consider using 'audioStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsAudioStream :: Lens.Lens' PutSessionResponse Lude.RsBody
psrsAudioStream = Lens.lens (audioStream :: PutSessionResponse -> Lude.RsBody) (\s a -> s {audioStream = a} :: PutSessionResponse)
{-# DEPRECATED psrsAudioStream "Use generic-lens or generic-optics with 'audioStream' instead." #-}

-- | The name of the current intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsIntentName :: Lens.Lens' PutSessionResponse (Lude.Maybe Lude.Text)
psrsIntentName = Lens.lens (intentName :: PutSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {intentName = a} :: PutSessionResponse)
{-# DEPRECATED psrsIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- |
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.
--
--
--     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.
--
--
--     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
--
--
-- /Note:/ Consider using 'dialogState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsDialogState :: Lens.Lens' PutSessionResponse (Lude.Maybe DialogState)
psrsDialogState = Lens.lens (dialogState :: PutSessionResponse -> Lude.Maybe DialogState) (\s a -> s {dialogState = a} :: PutSessionResponse)
{-# DEPRECATED psrsDialogState "Use generic-lens or generic-optics with 'dialogState' instead." #-}

-- | A list of active contexts for the session.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsActiveContexts :: Lens.Lens' PutSessionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
psrsActiveContexts = Lens.lens (activeContexts :: PutSessionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {activeContexts = a} :: PutSessionResponse)
{-# DEPRECATED psrsActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | The format of the response message. One of the following values:
--
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format for the client.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
--
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsMessageFormat :: Lens.Lens' PutSessionResponse (Lude.Maybe MessageFormatType)
psrsMessageFormat = Lens.lens (messageFormat :: PutSessionResponse -> Lude.Maybe MessageFormatType) (\s a -> s {messageFormat = a} :: PutSessionResponse)
{-# DEPRECATED psrsMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The next message that should be presented to the user.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsMessage :: Lens.Lens' PutSessionResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
psrsMessage = Lens.lens (message :: PutSessionResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {message = a} :: PutSessionResponse)
{-# DEPRECATED psrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsSessionId :: Lens.Lens' PutSessionResponse (Lude.Maybe Lude.Text)
psrsSessionId = Lens.lens (sessionId :: PutSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: PutSessionResponse)
{-# DEPRECATED psrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsSlotToElicit :: Lens.Lens' PutSessionResponse (Lude.Maybe Lude.Text)
psrsSlotToElicit = Lens.lens (slotToElicit :: PutSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {slotToElicit = a} :: PutSessionResponse)
{-# DEPRECATED psrsSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

-- | Content type as specified in the @Accept@ HTTP header in the request.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsContentType :: Lens.Lens' PutSessionResponse (Lude.Maybe Lude.Text)
psrsContentType = Lens.lens (contentType :: PutSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: PutSessionResponse)
{-# DEPRECATED psrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Map of key/value pairs representing session-specific context information.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsSessionAttributes :: Lens.Lens' PutSessionResponse (Lude.Maybe Lude.Text)
psrsSessionAttributes = Lens.lens (sessionAttributes :: PutSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionAttributes = a} :: PutSessionResponse)
{-# DEPRECATED psrsSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrsResponseStatus :: Lens.Lens' PutSessionResponse Lude.Int
psrsResponseStatus = Lens.lens (responseStatus :: PutSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutSessionResponse)
{-# DEPRECATED psrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
