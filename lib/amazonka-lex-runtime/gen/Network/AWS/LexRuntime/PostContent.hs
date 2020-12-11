{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PostContent (..),
    mkPostContent,

    -- ** Request lenses
    pcAccept,
    pcActiveContexts,
    pcRequestAttributes,
    pcSessionAttributes,
    pcBotName,
    pcBotAlias,
    pcUserId,
    pcContentType,
    pcInputStream,

    -- * Destructuring the response
    PostContentResponse (..),
    mkPostContentResponse,

    -- ** Response lenses
    pcrsSentimentResponse,
    pcrsNluIntentConfidence,
    pcrsSlots,
    pcrsIntentName,
    pcrsBotVersion,
    pcrsDialogState,
    pcrsActiveContexts,
    pcrsAlternativeIntents,
    pcrsInputTranscript,
    pcrsMessageFormat,
    pcrsMessage,
    pcrsSessionId,
    pcrsSlotToElicit,
    pcrsContentType,
    pcrsSessionAttributes,
    pcrsResponseStatus,
    pcrsAudioStream,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPostContent' smart constructor.
data PostContent = PostContent'
  { accept :: Lude.Maybe Lude.Text,
    activeContexts :: Lude.Maybe (Lude.Sensitive Lude.Text),
    requestAttributes :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sessionAttributes :: Lude.Maybe (Lude.Sensitive Lude.Text),
    botName :: Lude.Text,
    botAlias :: Lude.Text,
    userId :: Lude.Text,
    contentType :: Lude.Text,
    inputStream :: Lude.HashedBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'PostContent' with the minimum fields required to make a request.
--
-- * 'accept' - You pass this value as the @Accept@ HTTP header.
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
-- * 'activeContexts' - A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
-- * 'botAlias' - Alias of the Amazon Lex bot.
-- * 'botName' - Name of the Amazon Lex bot.
-- * 'contentType' - You pass this value as the @Content-Type@ HTTP header.
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
-- * 'inputStream' - User input in PCM or Opus audio format or text format as described in the @Content-Type@ HTTP header.
--
-- You can stream audio data to Amazon Lex or you can create a local buffer that captures all of the audio data before sending. In general, you get better performance if you stream audio data rather than buffering the data locally.
-- * 'requestAttributes' - You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
--
-- Request-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @requestAttributes@ and @sessionAttributes@ headers is limited to 12 KB.
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
-- * 'sessionAttributes' - You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
--
-- Application-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @sessionAttributes@ and @requestAttributes@ headers is limited to 12 KB.
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
mkPostContent ::
  -- | 'botName'
  Lude.Text ->
  -- | 'botAlias'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'contentType'
  Lude.Text ->
  -- | 'inputStream'
  Lude.HashedBody ->
  PostContent
mkPostContent
  pBotName_
  pBotAlias_
  pUserId_
  pContentType_
  pInputStream_ =
    PostContent'
      { accept = Lude.Nothing,
        activeContexts = Lude.Nothing,
        requestAttributes = Lude.Nothing,
        sessionAttributes = Lude.Nothing,
        botName = pBotName_,
        botAlias = pBotAlias_,
        userId = pUserId_,
        contentType = pContentType_,
        inputStream = pInputStream_
      }

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
pcAccept :: Lens.Lens' PostContent (Lude.Maybe Lude.Text)
pcAccept = Lens.lens (accept :: PostContent -> Lude.Maybe Lude.Text) (\s a -> s {accept = a} :: PostContent)
{-# DEPRECATED pcAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcActiveContexts :: Lens.Lens' PostContent (Lude.Maybe (Lude.Sensitive Lude.Text))
pcActiveContexts = Lens.lens (activeContexts :: PostContent -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {activeContexts = a} :: PostContent)
{-# DEPRECATED pcActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
--
-- Request-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @requestAttributes@ and @sessionAttributes@ headers is limited to 12 KB.
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ .
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
--
-- /Note:/ Consider using 'requestAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcRequestAttributes :: Lens.Lens' PostContent (Lude.Maybe (Lude.Sensitive Lude.Text))
pcRequestAttributes = Lens.lens (requestAttributes :: PostContent -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {requestAttributes = a} :: PostContent)
{-# DEPRECATED pcRequestAttributes "Use generic-lens or generic-optics with 'requestAttributes' instead." #-}

-- | You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
--
-- Application-specific information passed between Amazon Lex and a client application. The value must be a JSON serialized and base64 encoded map with string keys and values. The total size of the @sessionAttributes@ and @requestAttributes@ headers is limited to 12 KB.
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcSessionAttributes :: Lens.Lens' PostContent (Lude.Maybe (Lude.Sensitive Lude.Text))
pcSessionAttributes = Lens.lens (sessionAttributes :: PostContent -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {sessionAttributes = a} :: PostContent)
{-# DEPRECATED pcSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | Name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcBotName :: Lens.Lens' PostContent Lude.Text
pcBotName = Lens.lens (botName :: PostContent -> Lude.Text) (\s a -> s {botName = a} :: PostContent)
{-# DEPRECATED pcBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | Alias of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcBotAlias :: Lens.Lens' PostContent Lude.Text
pcBotAlias = Lens.lens (botAlias :: PostContent -> Lude.Text) (\s a -> s {botAlias = a} :: PostContent)
{-# DEPRECATED pcBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

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
pcUserId :: Lens.Lens' PostContent Lude.Text
pcUserId = Lens.lens (userId :: PostContent -> Lude.Text) (\s a -> s {userId = a} :: PostContent)
{-# DEPRECATED pcUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

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
pcContentType :: Lens.Lens' PostContent Lude.Text
pcContentType = Lens.lens (contentType :: PostContent -> Lude.Text) (\s a -> s {contentType = a} :: PostContent)
{-# DEPRECATED pcContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | User input in PCM or Opus audio format or text format as described in the @Content-Type@ HTTP header.
--
-- You can stream audio data to Amazon Lex or you can create a local buffer that captures all of the audio data before sending. In general, you get better performance if you stream audio data rather than buffering the data locally.
--
-- /Note:/ Consider using 'inputStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcInputStream :: Lens.Lens' PostContent Lude.HashedBody
pcInputStream = Lens.lens (inputStream :: PostContent -> Lude.HashedBody) (\s a -> s {inputStream = a} :: PostContent)
{-# DEPRECATED pcInputStream "Use generic-lens or generic-optics with 'inputStream' instead." #-}

instance Lude.AWSRequest PostContent where
  type Rs PostContent = PostContentResponse
  request = Req.postBody lexRuntimeService
  response =
    Res.receiveBody
      ( \s h x ->
          PostContentResponse'
            Lude.<$> (h Lude..#? "x-amz-lex-sentiment")
            Lude.<*> (h Lude..#? "x-amz-lex-nlu-intent-confidence")
            Lude.<*> (h Lude..#? "x-amz-lex-slots")
            Lude.<*> (h Lude..#? "x-amz-lex-intent-name")
            Lude.<*> (h Lude..#? "x-amz-lex-bot-version")
            Lude.<*> (h Lude..#? "x-amz-lex-dialog-state")
            Lude.<*> (h Lude..#? "x-amz-lex-active-contexts")
            Lude.<*> (h Lude..#? "x-amz-lex-alternative-intents")
            Lude.<*> (h Lude..#? "x-amz-lex-input-transcript")
            Lude.<*> (h Lude..#? "x-amz-lex-message-format")
            Lude.<*> (h Lude..#? "x-amz-lex-message")
            Lude.<*> (h Lude..#? "x-amz-lex-session-id")
            Lude.<*> (h Lude..#? "x-amz-lex-slot-to-elicit")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (h Lude..#? "x-amz-lex-session-attributes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (Lude.pure x)
      )

instance Lude.ToBody PostContent where
  toBody = Lude.toBody Lude.. inputStream

instance Lude.ToHeaders PostContent where
  toHeaders PostContent' {..} =
    Lude.mconcat
      [ "Accept" Lude.=# accept,
        "x-amz-lex-active-contexts" Lude.=# activeContexts,
        "x-amz-lex-request-attributes" Lude.=# requestAttributes,
        "x-amz-lex-session-attributes" Lude.=# sessionAttributes,
        "Content-Type" Lude.=# contentType
      ]

instance Lude.ToPath PostContent where
  toPath PostContent' {..} =
    Lude.mconcat
      [ "/bot/",
        Lude.toBS botName,
        "/alias/",
        Lude.toBS botAlias,
        "/user/",
        Lude.toBS userId,
        "/content"
      ]

instance Lude.ToQuery PostContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPostContentResponse' smart constructor.
data PostContentResponse = PostContentResponse'
  { sentimentResponse ::
      Lude.Maybe Lude.Text,
    nluIntentConfidence :: Lude.Maybe Lude.Text,
    slots :: Lude.Maybe Lude.Text,
    intentName :: Lude.Maybe Lude.Text,
    botVersion :: Lude.Maybe Lude.Text,
    dialogState :: Lude.Maybe DialogState,
    activeContexts ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    alternativeIntents :: Lude.Maybe Lude.Text,
    inputTranscript :: Lude.Maybe Lude.Text,
    messageFormat :: Lude.Maybe MessageFormatType,
    message :: Lude.Maybe (Lude.Sensitive Lude.Text),
    sessionId :: Lude.Maybe Lude.Text,
    slotToElicit :: Lude.Maybe Lude.Text,
    contentType :: Lude.Maybe Lude.Text,
    sessionAttributes :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    audioStream :: Lude.RsBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'PostContentResponse' with the minimum fields required to make a request.
--
-- * 'activeContexts' - A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
-- * 'alternativeIntents' - One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
-- * 'audioStream' - The prompt (or statement) to convey to the user. This is based on the bot configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the bot. If the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Another example: Suppose that the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. Then Amazon Lex sends that message in the response.
-- * 'botVersion' - The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
-- * 'contentType' - Content type as specified in the @Accept@ HTTP header in the request.
-- * 'dialogState' - Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.
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
-- * 'inputTranscript' - The text used to process the request.
--
-- If the input was an audio stream, the @inputTranscript@ field contains the text extracted from the audio stream. This is the text that is actually processed to recognize intents and slot values. You can use this information to determine if Amazon Lex is correctly processing the audio that you send.
-- * 'intentName' - Current user intent that Amazon Lex is aware of.
-- * 'message' - The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ in its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
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
-- * 'nluIntentConfidence' - Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0.
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
-- * 'responseStatus' - The response status code.
-- * 'sentimentResponse' - The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
-- * 'sessionAttributes' - Map of key/value pairs representing the session-specific context information.
-- * 'sessionId' - The unique identifier for the session.
-- * 'slotToElicit' - If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
-- * 'slots' - Map of zero or more intent slots (name/value pairs) Amazon Lex detected from the user input during the conversation. The field is base-64 encoded.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
mkPostContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'audioStream'
  Lude.RsBody ->
  PostContentResponse
mkPostContentResponse pResponseStatus_ pAudioStream_ =
  PostContentResponse'
    { sentimentResponse = Lude.Nothing,
      nluIntentConfidence = Lude.Nothing,
      slots = Lude.Nothing,
      intentName = Lude.Nothing,
      botVersion = Lude.Nothing,
      dialogState = Lude.Nothing,
      activeContexts = Lude.Nothing,
      alternativeIntents = Lude.Nothing,
      inputTranscript = Lude.Nothing,
      messageFormat = Lude.Nothing,
      message = Lude.Nothing,
      sessionId = Lude.Nothing,
      slotToElicit = Lude.Nothing,
      contentType = Lude.Nothing,
      sessionAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_,
      audioStream = pAudioStream_
    }

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
--
-- /Note:/ Consider using 'sentimentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsSentimentResponse :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsSentimentResponse = Lens.lens (sentimentResponse :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {sentimentResponse = a} :: PostContentResponse)
{-# DEPRECATED pcrsSentimentResponse "Use generic-lens or generic-optics with 'sentimentResponse' instead." #-}

-- | Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0.
--
-- The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
--
-- /Note:/ Consider using 'nluIntentConfidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsNluIntentConfidence :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsNluIntentConfidence = Lens.lens (nluIntentConfidence :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {nluIntentConfidence = a} :: PostContentResponse)
{-# DEPRECATED pcrsNluIntentConfidence "Use generic-lens or generic-optics with 'nluIntentConfidence' instead." #-}

-- | Map of zero or more intent slots (name/value pairs) Amazon Lex detected from the user input during the conversation. The field is base-64 encoded.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsSlots :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsSlots = Lens.lens (slots :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {slots = a} :: PostContentResponse)
{-# DEPRECATED pcrsSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | Current user intent that Amazon Lex is aware of.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsIntentName :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsIntentName = Lens.lens (intentName :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {intentName = a} :: PostContentResponse)
{-# DEPRECATED pcrsIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsBotVersion :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsBotVersion = Lens.lens (botVersion :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {botVersion = a} :: PostContentResponse)
{-# DEPRECATED pcrsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

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
pcrsDialogState :: Lens.Lens' PostContentResponse (Lude.Maybe DialogState)
pcrsDialogState = Lens.lens (dialogState :: PostContentResponse -> Lude.Maybe DialogState) (\s a -> s {dialogState = a} :: PostContentResponse)
{-# DEPRECATED pcrsDialogState "Use generic-lens or generic-optics with 'dialogState' instead." #-}

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsActiveContexts :: Lens.Lens' PostContentResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
pcrsActiveContexts = Lens.lens (activeContexts :: PostContentResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {activeContexts = a} :: PostContentResponse)
{-# DEPRECATED pcrsActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | One to four alternative intents that may be applicable to the user's intent.
--
-- Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
--
-- /Note:/ Consider using 'alternativeIntents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsAlternativeIntents :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsAlternativeIntents = Lens.lens (alternativeIntents :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {alternativeIntents = a} :: PostContentResponse)
{-# DEPRECATED pcrsAlternativeIntents "Use generic-lens or generic-optics with 'alternativeIntents' instead." #-}

-- | The text used to process the request.
--
-- If the input was an audio stream, the @inputTranscript@ field contains the text extracted from the audio stream. This is the text that is actually processed to recognize intents and slot values. You can use this information to determine if Amazon Lex is correctly processing the audio that you send.
--
-- /Note:/ Consider using 'inputTranscript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsInputTranscript :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsInputTranscript = Lens.lens (inputTranscript :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {inputTranscript = a} :: PostContentResponse)
{-# DEPRECATED pcrsInputTranscript "Use generic-lens or generic-optics with 'inputTranscript' instead." #-}

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
pcrsMessageFormat :: Lens.Lens' PostContentResponse (Lude.Maybe MessageFormatType)
pcrsMessageFormat = Lens.lens (messageFormat :: PostContentResponse -> Lude.Maybe MessageFormatType) (\s a -> s {messageFormat = a} :: PostContentResponse)
{-# DEPRECATED pcrsMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The message to convey to the user. The message can come from the bot's configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ in its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message.
-- When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' .
-- If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsMessage :: Lens.Lens' PostContentResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
pcrsMessage = Lens.lens (message :: PostContentResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {message = a} :: PostContentResponse)
{-# DEPRECATED pcrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsSessionId :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsSessionId = Lens.lens (sessionId :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: PostContentResponse)
{-# DEPRECATED pcrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsSlotToElicit :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsSlotToElicit = Lens.lens (slotToElicit :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {slotToElicit = a} :: PostContentResponse)
{-# DEPRECATED pcrsSlotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead." #-}

-- | Content type as specified in the @Accept@ HTTP header in the request.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsContentType :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsContentType = Lens.lens (contentType :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: PostContentResponse)
{-# DEPRECATED pcrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | Map of key/value pairs representing the session-specific context information.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsSessionAttributes :: Lens.Lens' PostContentResponse (Lude.Maybe Lude.Text)
pcrsSessionAttributes = Lens.lens (sessionAttributes :: PostContentResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionAttributes = a} :: PostContentResponse)
{-# DEPRECATED pcrsSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsResponseStatus :: Lens.Lens' PostContentResponse Lude.Int
pcrsResponseStatus = Lens.lens (responseStatus :: PostContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PostContentResponse)
{-# DEPRECATED pcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The prompt (or statement) to convey to the user. This is based on the bot configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the bot. If the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Another example: Suppose that the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. Then Amazon Lex sends that message in the response.
--
-- /Note:/ Consider using 'audioStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrsAudioStream :: Lens.Lens' PostContentResponse Lude.RsBody
pcrsAudioStream = Lens.lens (audioStream :: PostContentResponse -> Lude.RsBody) (\s a -> s {audioStream = a} :: PostContentResponse)
{-# DEPRECATED pcrsAudioStream "Use generic-lens or generic-optics with 'audioStream' instead." #-}
