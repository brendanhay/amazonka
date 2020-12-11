{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.PostText
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input to Amazon Lex. Client applications can use this API to send requests to Amazon Lex at runtime. Amazon Lex then interprets the user input using the machine learning model it built for the bot.
--
-- In response, Amazon Lex returns the next @message@ to convey to the user an optional @responseCard@ to display. Consider the following example messages:
--
--     * For a user input "I would like a pizza", Amazon Lex might return a response with a message eliciting slot data (for example, PizzaSize): "What size pizza would you like?"
--
--
--     * After the user provides all of the pizza order information, Amazon Lex might return a response with a message to obtain user confirmation "Proceed with the pizza order?".
--
--
--     * After the user replies to a confirmation prompt with a "yes", Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.".
--
--
-- Not all Amazon Lex messages require a user response. For example, a conclusion statement does not require a response. Some messages require only a "yes" or "no" user response. In addition to the @message@ , Amazon Lex provides additional context about the message in the response that you might use to enhance client behavior, for example, to display the appropriate client user interface. These are the @slotToElicit@ , @dialogState@ , @intentName@ , and @slots@ fields in the response. Consider the following examples:
--
--     * If the message is to elicit slot data, Amazon Lex returns the following context information:
--
--     * @dialogState@ set to ElicitSlot
--
--
--     * @intentName@ set to the intent name in the current context
--
--
--     * @slotToElicit@ set to the slot name for which the @message@ is eliciting information
--
--
--     * @slots@ set to a map of slots, configured for the intent, with currently known values
--
--
--
--
--     * If the message is a confirmation prompt, the @dialogState@ is set to ConfirmIntent and @SlotToElicit@ is set to null.
--
--
--     * If the message is a clarification prompt (configured for the intent) that indicates that user intent is not understood, the @dialogState@ is set to ElicitIntent and @slotToElicit@ is set to null.
--
--
-- In addition, Amazon Lex also returns your application-specific @sessionAttributes@ . For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context> .
module Network.AWS.LexRuntime.PostText
  ( -- * Creating a request
    PostText (..),
    mkPostText,

    -- ** Request lenses
    ptActiveContexts,
    ptRequestAttributes,
    ptSessionAttributes,
    ptBotName,
    ptBotAlias,
    ptUserId,
    ptInputText,

    -- * Destructuring the response
    PostTextResponse (..),
    mkPostTextResponse,

    -- ** Response lenses
    ptrsSentimentResponse,
    ptrsNluIntentConfidence,
    ptrsSlots,
    ptrsResponseCard,
    ptrsIntentName,
    ptrsBotVersion,
    ptrsDialogState,
    ptrsActiveContexts,
    ptrsAlternativeIntents,
    ptrsMessageFormat,
    ptrsMessage,
    ptrsSessionId,
    ptrsSlotToElicit,
    ptrsSessionAttributes,
    ptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPostText' smart constructor.
data PostText = PostText'
  { activeContexts ::
      Lude.Maybe [ActiveContext],
    requestAttributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    sessionAttributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    botName :: Lude.Text,
    botAlias :: Lude.Text,
    userId :: Lude.Text,
    inputText :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostText' with the minimum fields required to make a request.
--
-- * 'activeContexts' - A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
-- * 'botAlias' - The alias of the Amazon Lex bot.
-- * 'botName' - The name of the Amazon Lex bot.
-- * 'inputText' - The text that the user entered (Amazon Lex interprets this text).
-- * 'requestAttributes' - Request-specific information passed between Amazon Lex and a client application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
-- * 'sessionAttributes' - Application-specific information passed between Amazon Lex and a client application.
--
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
-- * 'userId' - The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. At runtime, each request must contain the @userID@ field.
--
-- To decide the user ID to use for your application, consider the following factors.
--
--     * The @userID@ field must not contain any personally identifiable information of the user, for example, name, personal identification numbers, or other end user personal information.
--
--
--     * If you want a user to start a conversation on one device and continue on another device, use a user-specific identifier.
--
--
--     * If you want the same user to be able to have two independent conversations on two different devices, choose a device-specific identifier.
--
--
--     * A user can't have two independent conversations with two different versions of the same bot. For example, a user can't have a conversation with the PROD and BETA versions of the same bot. If you anticipate that a user will need to have conversation with two different versions, for example, while testing, include the bot alias in the user ID to separate the two conversations.
mkPostText ::
  -- | 'botName'
  Lude.Text ->
  -- | 'botAlias'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'inputText'
  Lude.Sensitive Lude.Text ->
  PostText
mkPostText pBotName_ pBotAlias_ pUserId_ pInputText_ =
  PostText'
    { activeContexts = Lude.Nothing,
      requestAttributes = Lude.Nothing,
      sessionAttributes = Lude.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_,
      inputText = pInputText_
    }

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptActiveContexts :: Lens.Lens' PostText (Lude.Maybe [ActiveContext])
ptActiveContexts = Lens.lens (activeContexts :: PostText -> Lude.Maybe [ActiveContext]) (\s a -> s {activeContexts = a} :: PostText)
{-# DEPRECATED ptActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | Request-specific information passed between Amazon Lex and a client application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
--
-- /Note:/ Consider using 'requestAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptRequestAttributes :: Lens.Lens' PostText (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ptRequestAttributes = Lens.lens (requestAttributes :: PostText -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {requestAttributes = a} :: PostText)
{-# DEPRECATED ptRequestAttributes "Use generic-lens or generic-optics with 'requestAttributes' instead." #-}

-- | Application-specific information passed between Amazon Lex and a client application.
--
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptSessionAttributes :: Lens.Lens' PostText (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ptSessionAttributes = Lens.lens (sessionAttributes :: PostText -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {sessionAttributes = a} :: PostText)
{-# DEPRECATED ptSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptBotName :: Lens.Lens' PostText Lude.Text
ptBotName = Lens.lens (botName :: PostText -> Lude.Text) (\s a -> s {botName = a} :: PostText)
{-# DEPRECATED ptBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The alias of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptBotAlias :: Lens.Lens' PostText Lude.Text
ptBotAlias = Lens.lens (botAlias :: PostText -> Lude.Text) (\s a -> s {botAlias = a} :: PostText)
{-# DEPRECATED ptBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. At runtime, each request must contain the @userID@ field.
--
-- To decide the user ID to use for your application, consider the following factors.
--
--     * The @userID@ field must not contain any personally identifiable information of the user, for example, name, personal identification numbers, or other end user personal information.
--
--
--     * If you want a user to start a conversation on one device and continue on another device, use a user-specific identifier.
--
--
--     * If you want the same user to be able to have two independent conversations on two different devices, choose a device-specific identifier.
--
--
--     * A user can't have two independent conversations with two different versions of the same bot. For example, a user can't have a conversation with the PROD and BETA versions of the same bot. If you anticipate that a user will need to have conversation with two different versions, for example, while testing, include the bot alias in the user ID to separate the two conversations.
--
--
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptUserId :: Lens.Lens' PostText Lude.Text
ptUserId = Lens.lens (userId :: PostText -> Lude.Text) (\s a -> s {userId = a} :: PostText)
{-# DEPRECATED ptUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The text that the user entered (Amazon Lex interprets this text).
--
-- /Note:/ Consider using 'inputText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptInputText :: Lens.Lens' PostText (Lude.Sensitive Lude.Text)
ptInputText = Lens.lens (inputText :: PostText -> Lude.Sensitive Lude.Text) (\s a -> s {inputText = a} :: PostText)
{-# DEPRECATED ptInputText "Use generic-lens or generic-optics with 'inputText' instead." #-}

instance Lude.AWSRequest PostText where
  type Rs PostText = PostTextResponse
  request = Req.postJSON lexRuntimeService
  response =
    Res.receiveJSON
      ( \s h x ->
          PostTextResponse'
            Lude.<$> (x Lude..?> "sentimentResponse")
            Lude.<*> (x Lude..?> "nluIntentConfidence")
            Lude.<*> (x Lude..?> "slots" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "responseCard")
            Lude.<*> (x Lude..?> "intentName")
            Lude.<*> (x Lude..?> "botVersion")
            Lude.<*> (x Lude..?> "dialogState")
            Lude.<*> (x Lude..?> "activeContexts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "alternativeIntents" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "messageFormat")
            Lude.<*> (x Lude..?> "message")
            Lude.<*> (x Lude..?> "sessionId")
            Lude.<*> (x Lude..?> "slotToElicit")
            Lude.<*> (x Lude..?> "sessionAttributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PostText where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PostText where
  toJSON PostText' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("activeContexts" Lude..=) Lude.<$> activeContexts,
            ("requestAttributes" Lude..=) Lude.<$> requestAttributes,
            ("sessionAttributes" Lude..=) Lude.<$> sessionAttributes,
            Lude.Just ("inputText" Lude..= inputText)
          ]
      )

instance Lude.ToPath PostText where
  toPath PostText' {..} =
    Lude.mconcat
      [ "/bot/",
        Lude.toBS botName,
        "/alias/",
        Lude.toBS botAlias,
        "/user/",
        Lude.toBS userId,
        "/text"
      ]

instance Lude.ToQuery PostText where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPostTextResponse' smart constructor.
data PostTextResponse = PostTextResponse'
  { sentimentResponse ::
      Lude.Maybe SentimentResponse,
    nluIntentConfidence :: Lude.Maybe IntentConfidence,
    slots :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    responseCard :: Lude.Maybe ResponseCard,
    intentName :: Lude.Maybe Lude.Text,
    botVersion :: Lude.Maybe Lude.Text,
    dialogState :: Lude.Maybe DialogState,
    activeContexts :: Lude.Maybe [ActiveContext],
    alternativeIntents :: Lude.Maybe [PredictedIntent],
    messageFormat :: Lude.Maybe MessageFormatType,
    message :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sessionId :: Lude.Maybe Lude.Text,
    slotToElicit :: Lude.Maybe Lude.Text,
    sessionAttributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PostTextResponse' with the minimum fields required to make a request.
--
-- * 'activeContexts' - A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
-- * 'alternativeIntents' - One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
-- * 'botVersion' - The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
-- * 'dialogState' - Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit user intent.
-- For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialogState.
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response.
-- For example, Amazon Lex wants user confirmation before fulfilling an intent.
-- Instead of a simple "yes" or "no," a user might respond with additional information. For example, "yes, but make it thick crust pizza" or "no, I want to order a drink". Amazon Lex can process such additional information (in these examples, update the crust type slot value, or change intent from OrderPizza to OrderDrink).
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting a slot value for the current intent.
-- For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.
--
--
--     * @Fulfilled@ - Conveys that the Lambda function configured for the intent has successfully fulfilled the intent.
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
--
--     * @Failed@ - Conveys that the conversation with the user failed.
-- This can happen for various reasons including that the user did not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or the Lambda function failed to fulfill the intent.
--
--
-- * 'intentName' - The current user intent that Amazon Lex is aware of.
-- * 'message' - The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
-- * 'messageFormat' - The format of the response message. One of the following values:
--
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format defined by the Lambda function.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
--
-- * 'nluIntentConfidence' - Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> .
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
-- * 'responseCard' - Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
-- * 'responseStatus' - The response status code.
-- * 'sentimentResponse' - The sentiment expressed in and utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
-- * 'sessionAttributes' - A map of key-value pairs representing the session-specific context information.
-- * 'sessionId' - A unique identifier for the session.
-- * 'slotToElicit' - If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
-- * 'slots' - The intent slots that Amazon Lex detected from the user input in the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
mkPostTextResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PostTextResponse
mkPostTextResponse pResponseStatus_ =
  PostTextResponse'
    { sentimentResponse = Lude.Nothing,
      nluIntentConfidence = Lude.Nothing,
      slots = Lude.Nothing,
      responseCard = Lude.Nothing,
      intentName = Lude.Nothing,
      botVersion = Lude.Nothing,
      dialogState = Lude.Nothing,
      activeContexts = Lude.Nothing,
      alternativeIntents = Lude.Nothing,
      messageFormat = Lude.Nothing,
      message = Lude.Nothing,
      sessionId = Lude.Nothing,
      slotToElicit = Lude.Nothing,
      sessionAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The sentiment expressed in and utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
--
-- /Note:/ Consider using 'sentimentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsSentimentResponse :: Lens.Lens' PostTextResponse (Lude.Maybe SentimentResponse)
ptrsSentimentResponse = Lens.lens (sentimentResponse :: PostTextResponse -> Lude.Maybe SentimentResponse) (\s a -> s {sentimentResponse = a} :: PostTextResponse)
{-# DEPRECATED ptrsSentimentResponse "Use generic-lens or generic-optics with 'sentimentResponse' instead." #-}

-- | Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> .
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
--
-- /Note:/ Consider using 'nluIntentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsNluIntentConfidence :: Lens.Lens' PostTextResponse (Lude.Maybe IntentConfidence)
ptrsNluIntentConfidence = Lens.lens (nluIntentConfidence :: PostTextResponse -> Lude.Maybe IntentConfidence) (\s a -> s {nluIntentConfidence = a} :: PostTextResponse)
{-# DEPRECATED ptrsNluIntentConfidence "Use generic-lens or generic-optics with 'nluIntentConfidence' instead." #-}

-- | The intent slots that Amazon Lex detected from the user input in the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsSlots :: Lens.Lens' PostTextResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ptrsSlots = Lens.lens (slots :: PostTextResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {slots = a} :: PostTextResponse)
{-# DEPRECATED ptrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsResponseCard :: Lens.Lens' PostTextResponse (Lude.Maybe ResponseCard)
ptrsResponseCard = Lens.lens (responseCard :: PostTextResponse -> Lude.Maybe ResponseCard) (\s a -> s {responseCard = a} :: PostTextResponse)
{-# DEPRECATED ptrsResponseCard "Use generic-lens or generic-optics with 'responseCard' instead." #-}

-- | The current user intent that Amazon Lex is aware of.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsIntentName :: Lens.Lens' PostTextResponse (Lude.Maybe Lude.Text)
ptrsIntentName = Lens.lens (intentName :: PostTextResponse -> Lude.Maybe Lude.Text) (\s a -> s {intentName = a} :: PostTextResponse)
{-# DEPRECATED ptrsIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsBotVersion :: Lens.Lens' PostTextResponse (Lude.Maybe Lude.Text)
ptrsBotVersion = Lens.lens (botVersion :: PostTextResponse -> Lude.Maybe Lude.Text) (\s a -> s {botVersion = a} :: PostTextResponse)
{-# DEPRECATED ptrsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit user intent.
-- For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialogState.
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response.
-- For example, Amazon Lex wants user confirmation before fulfilling an intent.
-- Instead of a simple "yes" or "no," a user might respond with additional information. For example, "yes, but make it thick crust pizza" or "no, I want to order a drink". Amazon Lex can process such additional information (in these examples, update the crust type slot value, or change intent from OrderPizza to OrderDrink).
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting a slot value for the current intent.
-- For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.
--
--
--     * @Fulfilled@ - Conveys that the Lambda function configured for the intent has successfully fulfilled the intent.
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
--
--     * @Failed@ - Conveys that the conversation with the user failed.
-- This can happen for various reasons including that the user did not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or the Lambda function failed to fulfill the intent.
--
--
--
-- /Note:/ Consider using 'dialogState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsDialogState :: Lens.Lens' PostTextResponse (Lude.Maybe DialogState)
ptrsDialogState = Lens.lens (dialogState :: PostTextResponse -> Lude.Maybe DialogState) (\s a -> s {dialogState = a} :: PostTextResponse)
{-# DEPRECATED ptrsDialogState "Use generic-lens or generic-optics with 'dialogState' instead." #-}

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsActiveContexts :: Lens.Lens' PostTextResponse (Lude.Maybe [ActiveContext])
ptrsActiveContexts = Lens.lens (activeContexts :: PostTextResponse -> Lude.Maybe [ActiveContext]) (\s a -> s {activeContexts = a} :: PostTextResponse)
{-# DEPRECATED ptrsActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
--
-- /Note:/ Consider using 'alternativeIntents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsAlternativeIntents :: Lens.Lens' PostTextResponse (Lude.Maybe [PredictedIntent])
ptrsAlternativeIntents = Lens.lens (alternativeIntents :: PostTextResponse -> Lude.Maybe [PredictedIntent]) (\s a -> s {alternativeIntents = a} :: PostTextResponse)
{-# DEPRECATED ptrsAlternativeIntents "Use generic-lens or generic-optics with 'alternativeIntents' instead." #-}

-- | The format of the response message. One of the following values:
--
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format defined by the Lambda function.
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
ptrsMessageFormat :: Lens.Lens' PostTextResponse (Lude.Maybe MessageFormatType)
ptrsMessageFormat = Lens.lens (messageFormat :: PostTextResponse -> Lude.Maybe MessageFormatType) (\s a -> s {messageFormat = a} :: PostTextResponse)
{-# DEPRECATED ptrsMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsMessage :: Lens.Lens' PostTextResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
ptrsMessage = Lens.lens (message :: PostTextResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {message = a} :: PostTextResponse)
{-# DEPRECATED ptrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsSessionId :: Lens.Lens' PostTextResponse (Lude.Maybe Lude.Text)
ptrsSessionId = Lens.lens (sessionId :: PostTextResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: PostTextResponse)
{-# DEPRECATED ptrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsSlotToElicit :: Lens.Lens' PostTextResponse (Lude.Maybe Lude.Text)
ptrsSlotToElicit = Lens.lens (slotToElicit :: PostTextResponse -> Lude.Maybe Lude.Text) (\s a -> s {slotToElicit = a} :: PostTextResponse)
{-# DEPRECATED ptrsSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

-- | A map of key-value pairs representing the session-specific context information.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsSessionAttributes :: Lens.Lens' PostTextResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ptrsSessionAttributes = Lens.lens (sessionAttributes :: PostTextResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {sessionAttributes = a} :: PostTextResponse)
{-# DEPRECATED ptrsSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrsResponseStatus :: Lens.Lens' PostTextResponse Lude.Int
ptrsResponseStatus = Lens.lens (responseStatus :: PostTextResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PostTextResponse)
{-# DEPRECATED ptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
