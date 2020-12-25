{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ptBotName,
    ptBotAlias,
    ptUserId,
    ptInputText,
    ptActiveContexts,
    ptRequestAttributes,
    ptSessionAttributes,

    -- * Destructuring the response
    PostTextResponse (..),
    mkPostTextResponse,

    -- ** Response lenses
    ptrrsActiveContexts,
    ptrrsAlternativeIntents,
    ptrrsBotVersion,
    ptrrsDialogState,
    ptrrsIntentName,
    ptrrsMessage,
    ptrrsMessageFormat,
    ptrrsNluIntentConfidence,
    ptrrsResponseCard,
    ptrrsSentimentResponse,
    ptrrsSessionAttributes,
    ptrrsSessionId,
    ptrrsSlotToElicit,
    ptrrsSlots,
    ptrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPostText' smart constructor.
data PostText = PostText'
  { -- | The name of the Amazon Lex bot.
    botName :: Types.BotName,
    -- | The alias of the Amazon Lex bot.
    botAlias :: Types.BotAlias,
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
    userId :: Types.UserId,
    -- | The text that the user entered (Amazon Lex interprets this text).
    inputText :: Types.InputText,
    -- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
    --
    -- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
    activeContexts :: Core.Maybe [Types.ActiveContext],
    -- | Request-specific information passed between Amazon Lex and a client application.
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
    -- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
    requestAttributes :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Application-specific information passed between Amazon Lex and a client application.
    --
    -- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
    sessionAttributes :: Core.Maybe (Core.HashMap Types.String Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostText' value with any optional fields omitted.
mkPostText ::
  -- | 'botName'
  Types.BotName ->
  -- | 'botAlias'
  Types.BotAlias ->
  -- | 'userId'
  Types.UserId ->
  -- | 'inputText'
  Types.InputText ->
  PostText
mkPostText botName botAlias userId inputText =
  PostText'
    { botName,
      botAlias,
      userId,
      inputText,
      activeContexts = Core.Nothing,
      requestAttributes = Core.Nothing,
      sessionAttributes = Core.Nothing
    }

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptBotName :: Lens.Lens' PostText Types.BotName
ptBotName = Lens.field @"botName"
{-# DEPRECATED ptBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The alias of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptBotAlias :: Lens.Lens' PostText Types.BotAlias
ptBotAlias = Lens.field @"botAlias"
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
ptUserId :: Lens.Lens' PostText Types.UserId
ptUserId = Lens.field @"userId"
{-# DEPRECATED ptUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The text that the user entered (Amazon Lex interprets this text).
--
-- /Note:/ Consider using 'inputText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptInputText :: Lens.Lens' PostText Types.InputText
ptInputText = Lens.field @"inputText"
{-# DEPRECATED ptInputText "Use generic-lens or generic-optics with 'inputText' instead." #-}

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptActiveContexts :: Lens.Lens' PostText (Core.Maybe [Types.ActiveContext])
ptActiveContexts = Lens.field @"activeContexts"
{-# DEPRECATED ptActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | Request-specific information passed between Amazon Lex and a client application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
--
-- /Note:/ Consider using 'requestAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptRequestAttributes :: Lens.Lens' PostText (Core.Maybe (Core.HashMap Types.String Types.String))
ptRequestAttributes = Lens.field @"requestAttributes"
{-# DEPRECATED ptRequestAttributes "Use generic-lens or generic-optics with 'requestAttributes' instead." #-}

-- | Application-specific information passed between Amazon Lex and a client application.
--
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptSessionAttributes :: Lens.Lens' PostText (Core.Maybe (Core.HashMap Types.String Types.String))
ptSessionAttributes = Lens.field @"sessionAttributes"
{-# DEPRECATED ptSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

instance Core.FromJSON PostText where
  toJSON PostText {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("inputText" Core..= inputText),
            ("activeContexts" Core..=) Core.<$> activeContexts,
            ("requestAttributes" Core..=) Core.<$> requestAttributes,
            ("sessionAttributes" Core..=) Core.<$> sessionAttributes
          ]
      )

instance Core.AWSRequest PostText where
  type Rs PostText = PostTextResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/bot/" Core.<> (Core.toText botName) Core.<> ("/alias/")
                Core.<> (Core.toText botAlias)
                Core.<> ("/user/")
                Core.<> (Core.toText userId)
                Core.<> ("/text")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PostTextResponse'
            Core.<$> (x Core..:? "activeContexts")
            Core.<*> (x Core..:? "alternativeIntents")
            Core.<*> (x Core..:? "botVersion")
            Core.<*> (x Core..:? "dialogState")
            Core.<*> (x Core..:? "intentName")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "messageFormat")
            Core.<*> (x Core..:? "nluIntentConfidence")
            Core.<*> (x Core..:? "responseCard")
            Core.<*> (x Core..:? "sentimentResponse")
            Core.<*> (x Core..:? "sessionAttributes")
            Core.<*> (x Core..:? "sessionId")
            Core.<*> (x Core..:? "slotToElicit")
            Core.<*> (x Core..:? "slots")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPostTextResponse' smart constructor.
data PostTextResponse = PostTextResponse'
  { -- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
    --
    -- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
    activeContexts :: Core.Maybe [Types.ActiveContext],
    -- | One to four alternative intents that may be applicable to the user's intent.
    --
    -- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
    alternativeIntents :: Core.Maybe [Types.PredictedIntent],
    -- | The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
    botVersion :: Core.Maybe Types.BotVersion,
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
    dialogState :: Core.Maybe Types.DialogState,
    -- | The current user intent that Amazon Lex is aware of.
    intentName :: Core.Maybe Types.IntentName,
    -- | The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
    --
    -- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
    -- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
    -- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
    message :: Core.Maybe Types.Text,
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
    messageFormat :: Core.Maybe Types.MessageFormatType,
    -- | Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> .
    --
    -- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
    nluIntentConfidence :: Core.Maybe Types.IntentConfidence,
    -- | Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
    responseCard :: Core.Maybe Types.ResponseCard,
    -- | The sentiment expressed in and utterance.
    --
    -- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
    sentimentResponse :: Core.Maybe Types.SentimentResponse,
    -- | A map of key-value pairs representing the session-specific context information.
    sessionAttributes :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | A unique identifier for the session.
    sessionId :: Core.Maybe Types.String,
    -- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
    slotToElicit :: Core.Maybe Types.String,
    -- | The intent slots that Amazon Lex detected from the user input in the conversation.
    --
    -- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
    slots :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PostTextResponse' value with any optional fields omitted.
mkPostTextResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PostTextResponse
mkPostTextResponse responseStatus =
  PostTextResponse'
    { activeContexts = Core.Nothing,
      alternativeIntents = Core.Nothing,
      botVersion = Core.Nothing,
      dialogState = Core.Nothing,
      intentName = Core.Nothing,
      message = Core.Nothing,
      messageFormat = Core.Nothing,
      nluIntentConfidence = Core.Nothing,
      responseCard = Core.Nothing,
      sentimentResponse = Core.Nothing,
      sessionAttributes = Core.Nothing,
      sessionId = Core.Nothing,
      slotToElicit = Core.Nothing,
      slots = Core.Nothing,
      responseStatus
    }

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsActiveContexts :: Lens.Lens' PostTextResponse (Core.Maybe [Types.ActiveContext])
ptrrsActiveContexts = Lens.field @"activeContexts"
{-# DEPRECATED ptrrsActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
--
-- /Note:/ Consider using 'alternativeIntents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsAlternativeIntents :: Lens.Lens' PostTextResponse (Core.Maybe [Types.PredictedIntent])
ptrrsAlternativeIntents = Lens.field @"alternativeIntents"
{-# DEPRECATED ptrrsAlternativeIntents "Use generic-lens or generic-optics with 'alternativeIntents' instead." #-}

-- | The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsBotVersion :: Lens.Lens' PostTextResponse (Core.Maybe Types.BotVersion)
ptrrsBotVersion = Lens.field @"botVersion"
{-# DEPRECATED ptrrsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

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
ptrrsDialogState :: Lens.Lens' PostTextResponse (Core.Maybe Types.DialogState)
ptrrsDialogState = Lens.field @"dialogState"
{-# DEPRECATED ptrrsDialogState "Use generic-lens or generic-optics with 'dialogState' instead." #-}

-- | The current user intent that Amazon Lex is aware of.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsIntentName :: Lens.Lens' PostTextResponse (Core.Maybe Types.IntentName)
ptrrsIntentName = Lens.field @"intentName"
{-# DEPRECATED ptrrsIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsMessage :: Lens.Lens' PostTextResponse (Core.Maybe Types.Text)
ptrrsMessage = Lens.field @"message"
{-# DEPRECATED ptrrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

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
ptrrsMessageFormat :: Lens.Lens' PostTextResponse (Core.Maybe Types.MessageFormatType)
ptrrsMessageFormat = Lens.field @"messageFormat"
{-# DEPRECATED ptrrsMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> .
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
--
-- /Note:/ Consider using 'nluIntentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsNluIntentConfidence :: Lens.Lens' PostTextResponse (Core.Maybe Types.IntentConfidence)
ptrrsNluIntentConfidence = Lens.field @"nluIntentConfidence"
{-# DEPRECATED ptrrsNluIntentConfidence "Use generic-lens or generic-optics with 'nluIntentConfidence' instead." #-}

-- | Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsResponseCard :: Lens.Lens' PostTextResponse (Core.Maybe Types.ResponseCard)
ptrrsResponseCard = Lens.field @"responseCard"
{-# DEPRECATED ptrrsResponseCard "Use generic-lens or generic-optics with 'responseCard' instead." #-}

-- | The sentiment expressed in and utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
--
-- /Note:/ Consider using 'sentimentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsSentimentResponse :: Lens.Lens' PostTextResponse (Core.Maybe Types.SentimentResponse)
ptrrsSentimentResponse = Lens.field @"sentimentResponse"
{-# DEPRECATED ptrrsSentimentResponse "Use generic-lens or generic-optics with 'sentimentResponse' instead." #-}

-- | A map of key-value pairs representing the session-specific context information.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsSessionAttributes :: Lens.Lens' PostTextResponse (Core.Maybe (Core.HashMap Types.String Types.String))
ptrrsSessionAttributes = Lens.field @"sessionAttributes"
{-# DEPRECATED ptrrsSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | A unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsSessionId :: Lens.Lens' PostTextResponse (Core.Maybe Types.String)
ptrrsSessionId = Lens.field @"sessionId"
{-# DEPRECATED ptrrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsSlotToElicit :: Lens.Lens' PostTextResponse (Core.Maybe Types.String)
ptrrsSlotToElicit = Lens.field @"slotToElicit"
{-# DEPRECATED ptrrsSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

-- | The intent slots that Amazon Lex detected from the user input in the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsSlots :: Lens.Lens' PostTextResponse (Core.Maybe (Core.HashMap Types.String Types.String))
ptrrsSlots = Lens.field @"slots"
{-# DEPRECATED ptrrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptrrsResponseStatus :: Lens.Lens' PostTextResponse Core.Int
ptrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ptrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
