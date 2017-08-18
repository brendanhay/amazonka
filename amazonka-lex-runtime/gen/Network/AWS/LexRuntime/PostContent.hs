{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.PostContent
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input (text or speech) to Amazon Lex. Clients use this API to send requests to Amazon Lex at runtime. Amazon Lex interprets the user input using the machine learning model that it built for the bot.
--
--
-- In response, Amazon Lex returns the next message to convey to the user. Consider the following example messages:
--
--     * For a user input "I would like a pizza," Amazon Lex might return a response with a message eliciting slot data (for example, @PizzaSize@ ): "What size pizza would you like?".
--
--     * After the user provides all of the pizza order information, Amazon Lex might return a response with a message to get user confirmation: "Order the pizza?".
--
--     * After the user replies "Yes" to the confirmation prompt, Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.".
--
--
--
-- Not all Amazon Lex messages require a response from the user. For example, conclusion statements do not require a response. Some messages require only a yes or no response. In addition to the @message@ , Amazon Lex provides additional context about the message in the response that you can use to enhance client behavior, such as displaying the appropriate client user interface. Consider the following examples:
--
--     * If the message is to elicit slot data, Amazon Lex returns the following context information:
--
--     * @x-amz-lex-dialog-state@ header set to @ElicitSlot@
--
--     * @x-amz-lex-intent-name@ header set to the intent name in the current context
--
--     * @x-amz-lex-slot-to-elicit@ header set to the slot name for which the @message@ is eliciting information
--
--     * @x-amz-lex-slots@ header set to a map of slots configured for the intent with their current values
--
--
--
--     * If the message is a confirmation prompt, the @x-amz-lex-dialog-state@ header is set to @Confirmation@ and the @x-amz-lex-slot-to-elicit@ header is omitted.
--
--     * If the message is a clarification prompt configured for the intent, indicating that the user intent is not understood, the @x-amz-dialog-state@ header is set to @ElicitIntent@ and the @x-amz-slot-to-elicit@ header is omitted.
--
--
--
-- In addition, Amazon Lex also returns your application-specific @sessionAttributes@ . For more information, see <http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context> .
--
module Network.AWS.LexRuntime.PostContent
    (
    -- * Creating a Request
      postContent
    , PostContent
    -- * Request Lenses
    , pcAccept
    , pcSessionAttributes
    , pcBotName
    , pcBotAlias
    , pcUserId
    , pcContentType
    , pcInputStream

    -- * Destructuring the Response
    , postContentResponse
    , PostContentResponse
    -- * Response Lenses
    , pcrsSlots
    , pcrsIntentName
    , pcrsDialogState
    , pcrsInputTranscript
    , pcrsMessage
    , pcrsSlotToElicit
    , pcrsContentType
    , pcrsSessionAttributes
    , pcrsResponseStatus
    , pcrsAudioStream
    ) where

import           Network.AWS.Lens
import           Network.AWS.LexRuntime.Types
import           Network.AWS.LexRuntime.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'postContent' smart constructor.
data PostContent = PostContent'
    { _pcAccept            :: !(Maybe Text)
    , _pcSessionAttributes :: !(Maybe Text)
    , _pcBotName           :: !Text
    , _pcBotAlias          :: !Text
    , _pcUserId            :: !Text
    , _pcContentType       :: !Text
    , _pcInputStream       :: !HashedBody
    } deriving (Show,Generic)

-- | Creates a value of 'PostContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcAccept' - You pass this value as the @Accept@ HTTP header.  The message Amazon Lex returns in the response can be either text or speech based on the @Accept@ HTTP header value in the request.      * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.      * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech (using the configuration you specified in the @Accept@ header). For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format. The following are the accepted values:     * audio/mpeg     * audio/ogg     * audio/pcm     * text/plain; charset=utf-8     * audio/* (defaults to mpeg)
--
-- * 'pcSessionAttributes' - You pass this value in the @x-amz-lex-session-attributes@ HTTP header. The value must be map (keys and values must be strings) that is JSON serialized and then base64 encoded. A session represents dialog between a user and Amazon Lex. At runtime, a client application can pass contextual information, in the request to Amazon Lex. For example,      * You might use session attributes to track the requestID of user requests.     * In Getting Started Exercise 1, the example bot uses the price session attribute to maintain the price of flowers ordered (for example, "price":25). The code hook (Lambda function) sets this attribute based on the type of flowers ordered. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/gs-bp-details-after-lambda.html Review the Details of Information Flow> .      * In the BookTrip bot exercise, the bot uses the @currentReservation@ session attribute to maintains the slot data during the in-progress conversation to book a hotel or book a car. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/book-trip-detail-flow.html Details of Information Flow> .  Amazon Lex passes these session attributes to the Lambda functions configured for the intent In the your Lambda function, you can use the session attributes for initialization and customization (prompts). Some examples are:      * Initialization - In a pizza ordering bot, if you pass user location (for example, @"Location : 111 Maple Street"@ ), then your Lambda function might use this information to determine the closest pizzeria to place the order (and perhaps set the storeAddress slot value as well).  Personalized prompts - For example, you can configure prompts to refer to the user by name (for example, "Hey [firstName], what toppings would you like?"). You can pass the user's name as a session attribute ("firstName": "Joe") so that Amazon Lex can substitute the placeholder to provide a personalized prompt to the user ("Hey Joe, what toppings would you like?").
--
-- * 'pcBotName' - Name of the Amazon Lex bot.
--
-- * 'pcBotAlias' - Alias of the Amazon Lex bot.
--
-- * 'pcUserId' - ID of the client application user. Typically, each of your application users should have a unique ID. The application developer decides the user IDs. At runtime, each request must include the user ID. Note the following considerations:     * If you want a user to start conversation on one device and continue the conversation on another device, you might choose a user-specific identifier, such as the user's login, or Amazon Cognito user ID (assuming your application is using Amazon Cognito).      * If you want the same user to be able to have two independent conversations on two different devices, you might choose device-specific identifier, such as device ID, or some globally unique identifier.
--
-- * 'pcContentType' - You pass this values as the @Content-Type@ HTTP header.  Indicates the audio format or text. The header value must start with one of the following prefixes:      * PCM format     * audio/l16; rate=16000; channels=1     * audio/x-l16; sample-rate=16000; channel-count=1     * Opus format     * audio/x-cbr-opus-with-preamble; preamble-size=0; bit-rate=1; frame-size-milliseconds=1.1     * Text format     * text/plain; charset=utf-8
--
-- * 'pcInputStream' - User input in PCM or Opus audio format or text format as described in the @Content-Type@ HTTP header.
postContent
    :: Text -- ^ 'pcBotName'
    -> Text -- ^ 'pcBotAlias'
    -> Text -- ^ 'pcUserId'
    -> Text -- ^ 'pcContentType'
    -> HashedBody -- ^ 'pcInputStream'
    -> PostContent
postContent pBotName_ pBotAlias_ pUserId_ pContentType_ pInputStream_ =
    PostContent'
    { _pcAccept = Nothing
    , _pcSessionAttributes = Nothing
    , _pcBotName = pBotName_
    , _pcBotAlias = pBotAlias_
    , _pcUserId = pUserId_
    , _pcContentType = pContentType_
    , _pcInputStream = pInputStream_
    }

-- | You pass this value as the @Accept@ HTTP header.  The message Amazon Lex returns in the response can be either text or speech based on the @Accept@ HTTP header value in the request.      * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.      * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech (using the configuration you specified in the @Accept@ header). For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format. The following are the accepted values:     * audio/mpeg     * audio/ogg     * audio/pcm     * text/plain; charset=utf-8     * audio/* (defaults to mpeg)
pcAccept :: Lens' PostContent (Maybe Text)
pcAccept = lens _pcAccept (\ s a -> s{_pcAccept = a});

-- | You pass this value in the @x-amz-lex-session-attributes@ HTTP header. The value must be map (keys and values must be strings) that is JSON serialized and then base64 encoded. A session represents dialog between a user and Amazon Lex. At runtime, a client application can pass contextual information, in the request to Amazon Lex. For example,      * You might use session attributes to track the requestID of user requests.     * In Getting Started Exercise 1, the example bot uses the price session attribute to maintain the price of flowers ordered (for example, "price":25). The code hook (Lambda function) sets this attribute based on the type of flowers ordered. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/gs-bp-details-after-lambda.html Review the Details of Information Flow> .      * In the BookTrip bot exercise, the bot uses the @currentReservation@ session attribute to maintains the slot data during the in-progress conversation to book a hotel or book a car. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/book-trip-detail-flow.html Details of Information Flow> .  Amazon Lex passes these session attributes to the Lambda functions configured for the intent In the your Lambda function, you can use the session attributes for initialization and customization (prompts). Some examples are:      * Initialization - In a pizza ordering bot, if you pass user location (for example, @"Location : 111 Maple Street"@ ), then your Lambda function might use this information to determine the closest pizzeria to place the order (and perhaps set the storeAddress slot value as well).  Personalized prompts - For example, you can configure prompts to refer to the user by name (for example, "Hey [firstName], what toppings would you like?"). You can pass the user's name as a session attribute ("firstName": "Joe") so that Amazon Lex can substitute the placeholder to provide a personalized prompt to the user ("Hey Joe, what toppings would you like?").
pcSessionAttributes :: Lens' PostContent (Maybe Text)
pcSessionAttributes = lens _pcSessionAttributes (\ s a -> s{_pcSessionAttributes = a});

-- | Name of the Amazon Lex bot.
pcBotName :: Lens' PostContent Text
pcBotName = lens _pcBotName (\ s a -> s{_pcBotName = a});

-- | Alias of the Amazon Lex bot.
pcBotAlias :: Lens' PostContent Text
pcBotAlias = lens _pcBotAlias (\ s a -> s{_pcBotAlias = a});

-- | ID of the client application user. Typically, each of your application users should have a unique ID. The application developer decides the user IDs. At runtime, each request must include the user ID. Note the following considerations:     * If you want a user to start conversation on one device and continue the conversation on another device, you might choose a user-specific identifier, such as the user's login, or Amazon Cognito user ID (assuming your application is using Amazon Cognito).      * If you want the same user to be able to have two independent conversations on two different devices, you might choose device-specific identifier, such as device ID, or some globally unique identifier.
pcUserId :: Lens' PostContent Text
pcUserId = lens _pcUserId (\ s a -> s{_pcUserId = a});

-- | You pass this values as the @Content-Type@ HTTP header.  Indicates the audio format or text. The header value must start with one of the following prefixes:      * PCM format     * audio/l16; rate=16000; channels=1     * audio/x-l16; sample-rate=16000; channel-count=1     * Opus format     * audio/x-cbr-opus-with-preamble; preamble-size=0; bit-rate=1; frame-size-milliseconds=1.1     * Text format     * text/plain; charset=utf-8
pcContentType :: Lens' PostContent Text
pcContentType = lens _pcContentType (\ s a -> s{_pcContentType = a});

-- | User input in PCM or Opus audio format or text format as described in the @Content-Type@ HTTP header.
pcInputStream :: Lens' PostContent HashedBody
pcInputStream = lens _pcInputStream (\ s a -> s{_pcInputStream = a});

instance AWSRequest PostContent where
        type Rs PostContent = PostContentResponse
        request = postBody lexRuntime
        response
          = receiveBody
              (\ s h x ->
                 PostContentResponse' <$>
                   (h .#? "x-amz-lex-slots") <*>
                     (h .#? "x-amz-lex-intent-name")
                     <*> (h .#? "x-amz-lex-dialog-state")
                     <*> (h .#? "x-amz-lex-input-transcript")
                     <*> (h .#? "x-amz-lex-message")
                     <*> (h .#? "x-amz-lex-slot-to-elicit")
                     <*> (h .#? "Content-Type")
                     <*> (h .#? "x-amz-lex-session-attributes")
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance ToBody PostContent where
        toBody = toBody . _pcInputStream

instance ToHeaders PostContent where
        toHeaders PostContent'{..}
          = mconcat
              ["Accept" =# _pcAccept,
               "x-amz-lex-session-attributes" =#
                 _pcSessionAttributes,
               "Content-Type" =# _pcContentType,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath PostContent where
        toPath PostContent'{..}
          = mconcat
              ["/bot/", toBS _pcBotName, "/alias/",
               toBS _pcBotAlias, "/user/", toBS _pcUserId,
               "/content"]

instance ToQuery PostContent where
        toQuery = const mempty

-- | /See:/ 'postContentResponse' smart constructor.
data PostContentResponse = PostContentResponse'
    { _pcrsSlots             :: !(Maybe Text)
    , _pcrsIntentName        :: !(Maybe Text)
    , _pcrsDialogState       :: !(Maybe DialogState)
    , _pcrsInputTranscript   :: !(Maybe Text)
    , _pcrsMessage           :: !(Maybe Text)
    , _pcrsSlotToElicit      :: !(Maybe Text)
    , _pcrsContentType       :: !(Maybe Text)
    , _pcrsSessionAttributes :: !(Maybe Text)
    , _pcrsResponseStatus    :: !Int
    , _pcrsAudioStream       :: !RsBody
    } deriving (Show,Generic)

-- | Creates a value of 'PostContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrsSlots' - Map of zero or more intent slots (name/value pairs) Amazon Lex detected from the user input during the conversation.
--
-- * 'pcrsIntentName' - Current user intent that Amazon Lex is aware of.
--
-- * 'pcrsDialogState' - Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.      * @ElicitIntent@ – Amazon Lex wants to elicit the user's intent. Consider the following examples:  For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialog state.      * @ConfirmIntent@ – Amazon Lex is expecting a "yes" or "no" response.  For example, Amazon Lex wants user confirmation before fulfilling an intent. Instead of a simple "yes" or "no" response, a user might respond with additional information. For example, "yes, but make it a thick crust pizza" or "no, I want to order a drink." Amazon Lex can process such additional information (in these examples, update the crust type slot or change the intent from OrderPizza to OrderDrink).      * @ElicitSlot@ – Amazon Lex is expecting the value of a slot for the current intent.  For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.      * @Fulfilled@ – Conveys that the Lambda function has successfully fulfilled the intent.      * @ReadyForFulfillment@ – Conveys that the client has to fullfill the request.      * @Failed@ – Conveys that the conversation with the user failed.  This can happen for various reasons, including that the user does not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or if the Lambda function fails to fulfill the intent.
--
-- * 'pcrsInputTranscript' - Transcript of the voice input to the operation.
--
-- * 'pcrsMessage' - Message to convey to the user. It can come from the bot's configuration or a code hook (Lambda function). If the current intent is not configured with a code hook or if the code hook returned @Delegate@ as the @dialogAction.type@ in its response, then Amazon Lex decides the next course of action and selects an appropriate message from the bot configuration based on the current user interaction context. For example, if Amazon Lex is not able to understand the user input, it uses a clarification prompt message (For more information, see the Error Handling section in the Amazon Lex console). Another example: if the intent requires confirmation before fulfillment, then Amazon Lex uses the confirmation prompt message in the intent configuration. If the code hook returns a message, Amazon Lex passes it as-is in its response to the client.
--
-- * 'pcrsSlotToElicit' - If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- * 'pcrsContentType' - Content type as specified in the @Accept@ HTTP header in the request.
--
-- * 'pcrsSessionAttributes' - Map of key/value pairs representing the session-specific context information.
--
-- * 'pcrsResponseStatus' - -- | The response status code.
--
-- * 'pcrsAudioStream' - The prompt (or statement) to convey to the user. This is based on the bot configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the bot. If the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Another example: Suppose that the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. Then Amazon Lex sends that message in the response.
postContentResponse
    :: Int -- ^ 'pcrsResponseStatus'
    -> RsBody -- ^ 'pcrsAudioStream'
    -> PostContentResponse
postContentResponse pResponseStatus_ pAudioStream_ =
    PostContentResponse'
    { _pcrsSlots = Nothing
    , _pcrsIntentName = Nothing
    , _pcrsDialogState = Nothing
    , _pcrsInputTranscript = Nothing
    , _pcrsMessage = Nothing
    , _pcrsSlotToElicit = Nothing
    , _pcrsContentType = Nothing
    , _pcrsSessionAttributes = Nothing
    , _pcrsResponseStatus = pResponseStatus_
    , _pcrsAudioStream = pAudioStream_
    }

-- | Map of zero or more intent slots (name/value pairs) Amazon Lex detected from the user input during the conversation.
pcrsSlots :: Lens' PostContentResponse (Maybe Text)
pcrsSlots = lens _pcrsSlots (\ s a -> s{_pcrsSlots = a});

-- | Current user intent that Amazon Lex is aware of.
pcrsIntentName :: Lens' PostContentResponse (Maybe Text)
pcrsIntentName = lens _pcrsIntentName (\ s a -> s{_pcrsIntentName = a});

-- | Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.      * @ElicitIntent@ – Amazon Lex wants to elicit the user's intent. Consider the following examples:  For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialog state.      * @ConfirmIntent@ – Amazon Lex is expecting a "yes" or "no" response.  For example, Amazon Lex wants user confirmation before fulfilling an intent. Instead of a simple "yes" or "no" response, a user might respond with additional information. For example, "yes, but make it a thick crust pizza" or "no, I want to order a drink." Amazon Lex can process such additional information (in these examples, update the crust type slot or change the intent from OrderPizza to OrderDrink).      * @ElicitSlot@ – Amazon Lex is expecting the value of a slot for the current intent.  For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.      * @Fulfilled@ – Conveys that the Lambda function has successfully fulfilled the intent.      * @ReadyForFulfillment@ – Conveys that the client has to fullfill the request.      * @Failed@ – Conveys that the conversation with the user failed.  This can happen for various reasons, including that the user does not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or if the Lambda function fails to fulfill the intent.
pcrsDialogState :: Lens' PostContentResponse (Maybe DialogState)
pcrsDialogState = lens _pcrsDialogState (\ s a -> s{_pcrsDialogState = a});

-- | Transcript of the voice input to the operation.
pcrsInputTranscript :: Lens' PostContentResponse (Maybe Text)
pcrsInputTranscript = lens _pcrsInputTranscript (\ s a -> s{_pcrsInputTranscript = a});

-- | Message to convey to the user. It can come from the bot's configuration or a code hook (Lambda function). If the current intent is not configured with a code hook or if the code hook returned @Delegate@ as the @dialogAction.type@ in its response, then Amazon Lex decides the next course of action and selects an appropriate message from the bot configuration based on the current user interaction context. For example, if Amazon Lex is not able to understand the user input, it uses a clarification prompt message (For more information, see the Error Handling section in the Amazon Lex console). Another example: if the intent requires confirmation before fulfillment, then Amazon Lex uses the confirmation prompt message in the intent configuration. If the code hook returns a message, Amazon Lex passes it as-is in its response to the client.
pcrsMessage :: Lens' PostContentResponse (Maybe Text)
pcrsMessage = lens _pcrsMessage (\ s a -> s{_pcrsMessage = a});

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
pcrsSlotToElicit :: Lens' PostContentResponse (Maybe Text)
pcrsSlotToElicit = lens _pcrsSlotToElicit (\ s a -> s{_pcrsSlotToElicit = a});

-- | Content type as specified in the @Accept@ HTTP header in the request.
pcrsContentType :: Lens' PostContentResponse (Maybe Text)
pcrsContentType = lens _pcrsContentType (\ s a -> s{_pcrsContentType = a});

-- | Map of key/value pairs representing the session-specific context information.
pcrsSessionAttributes :: Lens' PostContentResponse (Maybe Text)
pcrsSessionAttributes = lens _pcrsSessionAttributes (\ s a -> s{_pcrsSessionAttributes = a});

-- | -- | The response status code.
pcrsResponseStatus :: Lens' PostContentResponse Int
pcrsResponseStatus = lens _pcrsResponseStatus (\ s a -> s{_pcrsResponseStatus = a});

-- | The prompt (or statement) to convey to the user. This is based on the bot configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the bot. If the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Another example: Suppose that the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. Then Amazon Lex sends that message in the response.
pcrsAudioStream :: Lens' PostContentResponse RsBody
pcrsAudioStream = lens _pcrsAudioStream (\ s a -> s{_pcrsAudioStream = a});
