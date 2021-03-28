{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.PostContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input (text or speech) to Amazon Lex. Clients use this API to send text and audio requests to Amazon Lex at runtime. Amazon Lex interprets the user input using the machine learning model that it built for the bot. 
--
-- The @PostContent@ operation supports audio input at 8kHz and 16kHz. You can use 8kHz audio to achieve higher speech recognition accuracy in telephone audio applications. 
-- In response, Amazon Lex returns the next message to convey to the user. Consider the following example messages: 
--
--     * For a user input "I would like a pizza," Amazon Lex might return a response with a message eliciting slot data (for example, @PizzaSize@ ): "What size pizza would you like?". 
--
--
--     * After the user provides all of the pizza order information, Amazon Lex might return a response with a message to get user confirmation: "Order the pizza?". 
--
--
--     * After the user replies "Yes" to the confirmation prompt, Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". 
--
--
-- Not all Amazon Lex messages require a response from the user. For example, conclusion statements do not require a response. Some messages require only a yes or no response. In addition to the @message@ , Amazon Lex provides additional context about the message in the response that you can use to enhance client behavior, such as displaying the appropriate client user interface. Consider the following examples: 
--
--     * If the message is to elicit slot data, Amazon Lex returns the following context information: 
--
--     * @x-amz-lex-dialog-state@ header set to @ElicitSlot@ 
--
--
--     * @x-amz-lex-intent-name@ header set to the intent name in the current context 
--
--
--     * @x-amz-lex-slot-to-elicit@ header set to the slot name for which the @message@ is eliciting information 
--
--
--     * @x-amz-lex-slots@ header set to a map of slots configured for the intent with their current values 
--
--
--
--
--     * If the message is a confirmation prompt, the @x-amz-lex-dialog-state@ header is set to @Confirmation@ and the @x-amz-lex-slot-to-elicit@ header is omitted. 
--
--
--     * If the message is a clarification prompt configured for the intent, indicating that the user intent is not understood, the @x-amz-dialog-state@ header is set to @ElicitIntent@ and the @x-amz-slot-to-elicit@ header is omitted. 
--
--
-- In addition, Amazon Lex also returns your application-specific @sessionAttributes@ . For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context> . 
module Network.AWS.LexRuntime.PostContent
    (
    -- * Creating a request
      PostContent (..)
    , mkPostContent
    -- ** Request lenses
    , pcBotName
    , pcBotAlias
    , pcUserId
    , pcContentType
    , pcInputStream
    , pcAccept
    , pcActiveContexts
    , pcRequestAttributes
    , pcSessionAttributes

    -- * Destructuring the response
    , PostContentResponse (..)
    , mkPostContentResponse
    -- ** Response lenses
    , pcrrsActiveContexts
    , pcrrsAlternativeIntents
    , pcrrsAudioStream
    , pcrrsBotVersion
    , pcrrsContentType
    , pcrrsDialogState
    , pcrrsInputTranscript
    , pcrrsIntentName
    , pcrrsMessage
    , pcrrsMessageFormat
    , pcrrsNluIntentConfidence
    , pcrrsSentimentResponse
    , pcrrsSessionAttributes
    , pcrrsSessionId
    , pcrrsSlotToElicit
    , pcrrsSlots
    , pcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPostContent' smart constructor.
data PostContent = PostContent'
  { botName :: Types.BotName
    -- ^ Name of the Amazon Lex bot.
  , botAlias :: Types.BotAlias
    -- ^ Alias of the Amazon Lex bot.
  , userId :: Types.UserId
    -- ^ The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. At runtime, each request must contain the @userID@ field.
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
  , contentType :: Types.HttpContentType
    -- ^ You pass this value as the @Content-Type@ HTTP header. 
--
-- Indicates the audio format or text. The header value must start with one of the following prefixes: 
--
--     * PCM format, audio data must be in little-endian byte order.
--
--     * audio/l16; rate=16000; channels=1
--
--
--     * audio/x-l16; sample-rate=16000; channel-count=1
--
--
--     * audio/lpcm; sample-rate=8000; sample-size-bits=16; channel-count=1; is-big-endian=false 
--
--
--
--
--     * Opus format
--
--     * audio/x-cbr-opus-with-preamble; preamble-size=0; bit-rate=256000; frame-size-milliseconds=4
--
--
--
--
--     * Text format
--
--     * text/plain; charset=utf-8
--
--
--
--
  , inputStream :: Core.HashedBody
    -- ^ User input in PCM or Opus audio format or text format as described in the @Content-Type@ HTTP header. 
--
-- You can stream audio data to Amazon Lex or you can create a local buffer that captures all of the audio data before sending. In general, you get better performance if you stream audio data rather than buffering the data locally.
  , accept :: Core.Maybe Types.Accept
    -- ^ You pass this value as the @Accept@ HTTP header. 
--
-- The message Amazon Lex returns in the response can be either text or speech based on the @Accept@ HTTP header value in the request. 
--
--     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response. 
--
--
--     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech (using the configuration you specified in the @Accept@ header). For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
--
--
--     * If the value is @audio/pcm@ , the speech returned is @audio/pcm@ in 16-bit, little endian format. 
--
--
--     * The following are the accepted values:
--
--     * audio/mpeg
--
--
--     * audio/ogg
--
--
--     * audio/pcm
--
--
--     * text/plain; charset=utf-8
--
--
--     * audio/* (defaults to mpeg)
--
--
--
--
  , activeContexts :: Core.Maybe Types.ActiveContexts
    -- ^ A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
  , requestAttributes :: Core.Maybe Types.RequestAttributes
    -- ^ You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
--
-- Request-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @requestAttributes@ and @sessionAttributes@ headers is limited to 12 KB.
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
  , sessionAttributes :: Core.Maybe Types.SessionAttributes
    -- ^ You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
--
-- Application-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @sessionAttributes@ and @requestAttributes@ headers is limited to 12 KB.
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'PostContent' value with any optional fields omitted.
mkPostContent
    :: Types.BotName -- ^ 'botName'
    -> Types.BotAlias -- ^ 'botAlias'
    -> Types.UserId -- ^ 'userId'
    -> Types.HttpContentType -- ^ 'contentType'
    -> Core.HashedBody -- ^ 'inputStream'
    -> PostContent
mkPostContent botName botAlias userId contentType inputStream
  = PostContent'{botName, botAlias, userId, contentType, inputStream,
                 accept = Core.Nothing, activeContexts = Core.Nothing,
                 requestAttributes = Core.Nothing, sessionAttributes = Core.Nothing}

-- | Name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcBotName :: Lens.Lens' PostContent Types.BotName
pcBotName = Lens.field @"botName"
{-# INLINEABLE pcBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | Alias of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcBotAlias :: Lens.Lens' PostContent Types.BotAlias
pcBotAlias = Lens.field @"botAlias"
{-# INLINEABLE pcBotAlias #-}
{-# DEPRECATED botAlias "Use generic-lens or generic-optics with 'botAlias' instead"  #-}

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
pcUserId :: Lens.Lens' PostContent Types.UserId
pcUserId = Lens.field @"userId"
{-# INLINEABLE pcUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | You pass this value as the @Content-Type@ HTTP header. 
--
-- Indicates the audio format or text. The header value must start with one of the following prefixes: 
--
--     * PCM format, audio data must be in little-endian byte order.
--
--     * audio/l16; rate=16000; channels=1
--
--
--     * audio/x-l16; sample-rate=16000; channel-count=1
--
--
--     * audio/lpcm; sample-rate=8000; sample-size-bits=16; channel-count=1; is-big-endian=false 
--
--
--
--
--     * Opus format
--
--     * audio/x-cbr-opus-with-preamble; preamble-size=0; bit-rate=256000; frame-size-milliseconds=4
--
--
--
--
--     * Text format
--
--     * text/plain; charset=utf-8
--
--
--
--
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcContentType :: Lens.Lens' PostContent Types.HttpContentType
pcContentType = Lens.field @"contentType"
{-# INLINEABLE pcContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | User input in PCM or Opus audio format or text format as described in the @Content-Type@ HTTP header. 
--
-- You can stream audio data to Amazon Lex or you can create a local buffer that captures all of the audio data before sending. In general, you get better performance if you stream audio data rather than buffering the data locally.
--
-- /Note:/ Consider using 'inputStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcInputStream :: Lens.Lens' PostContent Core.HashedBody
pcInputStream = Lens.field @"inputStream"
{-# INLINEABLE pcInputStream #-}
{-# DEPRECATED inputStream "Use generic-lens or generic-optics with 'inputStream' instead"  #-}

-- | You pass this value as the @Accept@ HTTP header. 
--
-- The message Amazon Lex returns in the response can be either text or speech based on the @Accept@ HTTP header value in the request. 
--
--     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response. 
--
--
--     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech (using the configuration you specified in the @Accept@ header). For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
--
--
--     * If the value is @audio/pcm@ , the speech returned is @audio/pcm@ in 16-bit, little endian format. 
--
--
--     * The following are the accepted values:
--
--     * audio/mpeg
--
--
--     * audio/ogg
--
--
--     * audio/pcm
--
--
--     * text/plain; charset=utf-8
--
--
--     * audio/* (defaults to mpeg)
--
--
--
--
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAccept :: Lens.Lens' PostContent (Core.Maybe Types.Accept)
pcAccept = Lens.field @"accept"
{-# INLINEABLE pcAccept #-}
{-# DEPRECATED accept "Use generic-lens or generic-optics with 'accept' instead"  #-}

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcActiveContexts :: Lens.Lens' PostContent (Core.Maybe Types.ActiveContexts)
pcActiveContexts = Lens.field @"activeContexts"
{-# INLINEABLE pcActiveContexts #-}
{-# DEPRECATED activeContexts "Use generic-lens or generic-optics with 'activeContexts' instead"  #-}

-- | You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
--
-- Request-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @requestAttributes@ and @sessionAttributes@ headers is limited to 12 KB.
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
--
-- /Note:/ Consider using 'requestAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcRequestAttributes :: Lens.Lens' PostContent (Core.Maybe Types.RequestAttributes)
pcRequestAttributes = Lens.field @"requestAttributes"
{-# INLINEABLE pcRequestAttributes #-}
{-# DEPRECATED requestAttributes "Use generic-lens or generic-optics with 'requestAttributes' instead"  #-}

-- | You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
--
-- Application-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @sessionAttributes@ and @requestAttributes@ headers is limited to 12 KB.
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcSessionAttributes :: Lens.Lens' PostContent (Core.Maybe Types.SessionAttributes)
pcSessionAttributes = Lens.field @"sessionAttributes"
{-# INLINEABLE pcSessionAttributes #-}
{-# DEPRECATED sessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead"  #-}

instance Core.ToQuery PostContent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PostContent where
        toHeaders PostContent{..}
          = Core.toHeaders "Content-Type" contentType Core.<>
              Core.toHeaders "Accept" accept
              Core.<> Core.toHeaders "x-amz-lex-active-contexts" activeContexts
              Core.<>
              Core.toHeaders "x-amz-lex-request-attributes" requestAttributes
              Core.<>
              Core.toHeaders "x-amz-lex-session-attributes" sessionAttributes

instance Core.AWSRequest PostContent where
        type Rs PostContent = PostContentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/bot/" Core.<> Core.toText botName Core.<> "/alias/" Core.<>
                             Core.toText botAlias
                             Core.<> "/user/"
                             Core.<> Core.toText userId
                             Core.<> "/content",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody inputStream}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBody
              (\ s h x ->
                 PostContentResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-lex-active-contexts" h) Core.<*>
                     Core.parseHeaderMaybe "x-amz-lex-alternative-intents" h
                     Core.<*> Core.pure x
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-bot-version" h
                     Core.<*> Core.parseHeaderMaybe "Content-Type" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-dialog-state" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-input-transcript" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-intent-name" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-message" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-message-format" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-nlu-intent-confidence" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-sentiment" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-session-attributes" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-session-id" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-slot-to-elicit" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-slots" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPostContentResponse' smart constructor.
data PostContentResponse = PostContentResponse'
  { activeContexts :: Core.Maybe Types.ActiveContexts
    -- ^ A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
  , alternativeIntents :: Core.Maybe Core.Text
    -- ^ One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
  , audioStream :: Core.RsBody
    -- ^ The prompt (or statement) to convey to the user. This is based on the bot configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the bot. If the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Another example: Suppose that the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. Then Amazon Lex sends that message in the response. 
  , botVersion :: Core.Maybe Types.BotVersion
    -- ^ The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
  , contentType :: Core.Maybe Types.HttpContentType
    -- ^ Content type as specified in the @Accept@ HTTP header in the request.
  , dialogState :: Core.Maybe Types.DialogState
    -- ^ Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface. 
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent. Consider the following examples: 
-- For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialog state. 
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response. 
-- For example, Amazon Lex wants user confirmation before fulfilling an intent. Instead of a simple "yes" or "no" response, a user might respond with additional information. For example, "yes, but make it a thick crust pizza" or "no, I want to order a drink." Amazon Lex can process such additional information (in these examples, update the crust type slot or change the intent from OrderPizza to OrderDrink). 
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent. 
-- For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately. 
--
--
--     * @Fulfilled@ - Conveys that the Lambda function has successfully fulfilled the intent. 
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the request. 
--
--
--     * @Failed@ - Conveys that the conversation with the user failed. 
-- This can happen for various reasons, including that the user does not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or if the Lambda function fails to fulfill the intent. 
--
--
  , inputTranscript :: Core.Maybe Core.Text
    -- ^ The text used to process the request.
--
-- If the input was an audio stream, the @inputTranscript@ field contains the text extracted from the audio stream. This is the text that is actually processed to recognize intents and slot values. You can use this information to determine if Amazon Lex is correctly processing the audio that you send.
  , intentName :: Core.Maybe Types.IntentName
    -- ^ Current user intent that Amazon Lex is aware of.
  , message :: Core.Maybe Types.Message
    -- ^ The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ in its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
  , messageFormat :: Core.Maybe Types.MessageFormatType
    -- ^ The format of the response message. One of the following values:
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
  , nluIntentConfidence :: Core.Maybe Core.Text
    -- ^ Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0.
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex. 
  , sentimentResponse :: Core.Maybe Core.Text
    -- ^ The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
  , sessionAttributes :: Core.Maybe Core.Text
    -- ^ Map of key/value pairs representing the session-specific context information. 
  , sessionId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the session.
  , slotToElicit :: Core.Maybe Core.Text
    -- ^ If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value. 
  , slots :: Core.Maybe Core.Text
    -- ^ Map of zero or more intent slots (name/value pairs) Amazon Lex detected from the user input during the conversation. The field is base-64 encoded.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'PostContentResponse' value with any optional fields omitted.
mkPostContentResponse
    :: Core.RsBody -- ^ 'audioStream'
    -> Core.Int -- ^ 'responseStatus'
    -> PostContentResponse
mkPostContentResponse audioStream responseStatus
  = PostContentResponse'{activeContexts = Core.Nothing,
                         alternativeIntents = Core.Nothing, audioStream,
                         botVersion = Core.Nothing, contentType = Core.Nothing,
                         dialogState = Core.Nothing, inputTranscript = Core.Nothing,
                         intentName = Core.Nothing, message = Core.Nothing,
                         messageFormat = Core.Nothing, nluIntentConfidence = Core.Nothing,
                         sentimentResponse = Core.Nothing, sessionAttributes = Core.Nothing,
                         sessionId = Core.Nothing, slotToElicit = Core.Nothing,
                         slots = Core.Nothing, responseStatus}

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsActiveContexts :: Lens.Lens' PostContentResponse (Core.Maybe Types.ActiveContexts)
pcrrsActiveContexts = Lens.field @"activeContexts"
{-# INLINEABLE pcrrsActiveContexts #-}
{-# DEPRECATED activeContexts "Use generic-lens or generic-optics with 'activeContexts' instead"  #-}

-- | One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
--
-- /Note:/ Consider using 'alternativeIntents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsAlternativeIntents :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsAlternativeIntents = Lens.field @"alternativeIntents"
{-# INLINEABLE pcrrsAlternativeIntents #-}
{-# DEPRECATED alternativeIntents "Use generic-lens or generic-optics with 'alternativeIntents' instead"  #-}

-- | The prompt (or statement) to convey to the user. This is based on the bot configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the bot. If the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Another example: Suppose that the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. Then Amazon Lex sends that message in the response. 
--
-- /Note:/ Consider using 'audioStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsAudioStream :: Lens.Lens' PostContentResponse Core.RsBody
pcrrsAudioStream = Lens.field @"audioStream"
{-# INLINEABLE pcrrsAudioStream #-}
{-# DEPRECATED audioStream "Use generic-lens or generic-optics with 'audioStream' instead"  #-}

-- | The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsBotVersion :: Lens.Lens' PostContentResponse (Core.Maybe Types.BotVersion)
pcrrsBotVersion = Lens.field @"botVersion"
{-# INLINEABLE pcrrsBotVersion #-}
{-# DEPRECATED botVersion "Use generic-lens or generic-optics with 'botVersion' instead"  #-}

-- | Content type as specified in the @Accept@ HTTP header in the request.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsContentType :: Lens.Lens' PostContentResponse (Core.Maybe Types.HttpContentType)
pcrrsContentType = Lens.field @"contentType"
{-# INLINEABLE pcrrsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface. 
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent. Consider the following examples: 
-- For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialog state. 
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response. 
-- For example, Amazon Lex wants user confirmation before fulfilling an intent. Instead of a simple "yes" or "no" response, a user might respond with additional information. For example, "yes, but make it a thick crust pizza" or "no, I want to order a drink." Amazon Lex can process such additional information (in these examples, update the crust type slot or change the intent from OrderPizza to OrderDrink). 
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent. 
-- For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately. 
--
--
--     * @Fulfilled@ - Conveys that the Lambda function has successfully fulfilled the intent. 
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the request. 
--
--
--     * @Failed@ - Conveys that the conversation with the user failed. 
-- This can happen for various reasons, including that the user does not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or if the Lambda function fails to fulfill the intent. 
--
--
--
-- /Note:/ Consider using 'dialogState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsDialogState :: Lens.Lens' PostContentResponse (Core.Maybe Types.DialogState)
pcrrsDialogState = Lens.field @"dialogState"
{-# INLINEABLE pcrrsDialogState #-}
{-# DEPRECATED dialogState "Use generic-lens or generic-optics with 'dialogState' instead"  #-}

-- | The text used to process the request.
--
-- If the input was an audio stream, the @inputTranscript@ field contains the text extracted from the audio stream. This is the text that is actually processed to recognize intents and slot values. You can use this information to determine if Amazon Lex is correctly processing the audio that you send.
--
-- /Note:/ Consider using 'inputTranscript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsInputTranscript :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsInputTranscript = Lens.field @"inputTranscript"
{-# INLINEABLE pcrrsInputTranscript #-}
{-# DEPRECATED inputTranscript "Use generic-lens or generic-optics with 'inputTranscript' instead"  #-}

-- | Current user intent that Amazon Lex is aware of.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsIntentName :: Lens.Lens' PostContentResponse (Core.Maybe Types.IntentName)
pcrrsIntentName = Lens.field @"intentName"
{-# INLINEABLE pcrrsIntentName #-}
{-# DEPRECATED intentName "Use generic-lens or generic-optics with 'intentName' instead"  #-}

-- | The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ in its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsMessage :: Lens.Lens' PostContentResponse (Core.Maybe Types.Message)
pcrrsMessage = Lens.field @"message"
{-# INLINEABLE pcrrsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

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
pcrrsMessageFormat :: Lens.Lens' PostContentResponse (Core.Maybe Types.MessageFormatType)
pcrrsMessageFormat = Lens.field @"messageFormat"
{-# INLINEABLE pcrrsMessageFormat #-}
{-# DEPRECATED messageFormat "Use generic-lens or generic-optics with 'messageFormat' instead"  #-}

-- | Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0.
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex. 
--
-- /Note:/ Consider using 'nluIntentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsNluIntentConfidence :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsNluIntentConfidence = Lens.field @"nluIntentConfidence"
{-# INLINEABLE pcrrsNluIntentConfidence #-}
{-# DEPRECATED nluIntentConfidence "Use generic-lens or generic-optics with 'nluIntentConfidence' instead"  #-}

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
--
-- /Note:/ Consider using 'sentimentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsSentimentResponse :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsSentimentResponse = Lens.field @"sentimentResponse"
{-# INLINEABLE pcrrsSentimentResponse #-}
{-# DEPRECATED sentimentResponse "Use generic-lens or generic-optics with 'sentimentResponse' instead"  #-}

-- | Map of key/value pairs representing the session-specific context information. 
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsSessionAttributes :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsSessionAttributes = Lens.field @"sessionAttributes"
{-# INLINEABLE pcrrsSessionAttributes #-}
{-# DEPRECATED sessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead"  #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsSessionId :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsSessionId = Lens.field @"sessionId"
{-# INLINEABLE pcrrsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value. 
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsSlotToElicit :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsSlotToElicit = Lens.field @"slotToElicit"
{-# INLINEABLE pcrrsSlotToElicit #-}
{-# DEPRECATED slotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead"  #-}

-- | Map of zero or more intent slots (name/value pairs) Amazon Lex detected from the user input during the conversation. The field is base-64 encoded.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsSlots :: Lens.Lens' PostContentResponse (Core.Maybe Core.Text)
pcrrsSlots = Lens.field @"slots"
{-# INLINEABLE pcrrsSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsResponseStatus :: Lens.Lens' PostContentResponse Core.Int
pcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
