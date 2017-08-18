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
-- Module      : Network.AWS.LexRuntime.PostText
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input (text-only) to Amazon Lex. Client applications can use this API to send requests to Amazon Lex at runtime. Amazon Lex then interprets the user input using the machine learning model it built for the bot.
--
--
-- In response, Amazon Lex returns the next @message@ to convey to the user an optional @responseCard@ to display. Consider the following example messages:
--
--     * For a user input "I would like a pizza", Amazon Lex might return a response with a message eliciting slot data (for example, PizzaSize): "What size pizza would you like?"
--
--     * After the user provides all of the pizza order information, Amazon Lex might return a response with a message to obtain user confirmation "Proceed with the pizza order?".
--
--     * After the user replies to a confirmation prompt with a "yes", Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.".
--
--
--
-- Not all Amazon Lex messages require a user response. For example, a conclusion statement does not require a response. Some messages require only a "yes" or "no" user response. In addition to the @message@ , Amazon Lex provides additional context about the message in the response that you might use to enhance client behavior, for example, to display the appropriate client user interface. These are the @slotToElicit@ , @dialogState@ , @intentName@ , and @slots@ fields in the response. Consider the following examples:
--
--     * If the message is to elicit slot data, Amazon Lex returns the following context information:
--
--     * @dialogState@ set to ElicitSlot
--
--     * @intentName@ set to the intent name in the current context
--
--     * @slotToElicit@ set to the slot name for which the @message@ is eliciting information
--
--     * @slots@ set to a map of slots, configured for the intent, with currently known values
--
--
--
--     * If the message is a confirmation prompt, the @dialogState@ is set to ConfirmIntent and @SlotToElicit@ is set to null.
--
--     * If the message is a clarification prompt (configured for the intent) that indicates that user intent is not understood, the @dialogState@ is set to ElicitIntent and @slotToElicit@ is set to null.
--
--
--
-- In addition, Amazon Lex also returns your application-specific @sessionAttributes@ . For more information, see <http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context> .
--
module Network.AWS.LexRuntime.PostText
    (
    -- * Creating a Request
      postText
    , PostText
    -- * Request Lenses
    , ptSessionAttributes
    , ptBotName
    , ptBotAlias
    , ptUserId
    , ptInputText

    -- * Destructuring the Response
    , postTextResponse
    , PostTextResponse
    -- * Response Lenses
    , ptrsSlots
    , ptrsResponseCard
    , ptrsIntentName
    , ptrsDialogState
    , ptrsMessage
    , ptrsSlotToElicit
    , ptrsSessionAttributes
    , ptrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.LexRuntime.Types
import           Network.AWS.LexRuntime.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'postText' smart constructor.
data PostText = PostText'
    { _ptSessionAttributes :: !(Maybe (Map Text Text))
    , _ptBotName           :: !Text
    , _ptBotAlias          :: !Text
    , _ptUserId            :: !Text
    , _ptInputText         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PostText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptSessionAttributes' - By using session attributes, a client application can pass contextual information in the request to Amazon Lex For example,      * In Getting Started Exercise 1, the example bot uses the @price@ session attribute to maintain the price of the flowers ordered (for example, "Price":25). The code hook (the Lambda function) sets this attribute based on the type of flowers ordered. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/gs-bp-details-after-lambda.html Review the Details of Information Flow> .      * In the BookTrip bot exercise, the bot uses the @currentReservation@ session attribute to maintain slot data during the in-progress conversation to book a hotel or book a car. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/book-trip-detail-flow.html Details of Information Flow> .      * You might use the session attributes (key, value pairs) to track the requestID of user requests. Amazon Lex simply passes these session attributes to the Lambda functions configured for the intent. In your Lambda function, you can also use the session attributes for initialization and customization (prompts and response cards). Some examples are:     * Initialization - In a pizza ordering bot, if you can pass the user location as a session attribute (for example, @"Location" : "111 Maple street"@ ), then your Lambda function might use this information to determine the closest pizzeria to place the order (perhaps to set the storeAddress slot value).      * Personalize prompts - For example, you can configure prompts to refer to the user name. (For example, "Hey [FirstName], what toppings would you like?"). You can pass the user name as a session attribute (@"FirstName" : "Joe"@ ) so that Amazon Lex can substitute the placeholder to provide a personalize prompt to the user ("Hey Joe, what toppings would you like?").
--
-- * 'ptBotName' - The name of the Amazon Lex bot.
--
-- * 'ptBotAlias' - The alias of the Amazon Lex bot.
--
-- * 'ptUserId' - The ID of the client application user. The application developer decides the user IDs. At runtime, each request must include the user ID. Typically, each of your application users should have a unique ID. Note the following considerations:      * If you want a user to start a conversation on one device and continue the conversation on another device, you might choose a user-specific identifier, such as a login or Amazon Cognito user ID (assuming your application is using Amazon Cognito).      * If you want the same user to be able to have two independent conversations on two different devices, you might choose a device-specific identifier, such as device ID, or some globally unique identifier.
--
-- * 'ptInputText' - The text that the user entered (Amazon Lex interprets this text).
postText
    :: Text -- ^ 'ptBotName'
    -> Text -- ^ 'ptBotAlias'
    -> Text -- ^ 'ptUserId'
    -> Text -- ^ 'ptInputText'
    -> PostText
postText pBotName_ pBotAlias_ pUserId_ pInputText_ =
    PostText'
    { _ptSessionAttributes = Nothing
    , _ptBotName = pBotName_
    , _ptBotAlias = pBotAlias_
    , _ptUserId = pUserId_
    , _ptInputText = pInputText_
    }

-- | By using session attributes, a client application can pass contextual information in the request to Amazon Lex For example,      * In Getting Started Exercise 1, the example bot uses the @price@ session attribute to maintain the price of the flowers ordered (for example, "Price":25). The code hook (the Lambda function) sets this attribute based on the type of flowers ordered. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/gs-bp-details-after-lambda.html Review the Details of Information Flow> .      * In the BookTrip bot exercise, the bot uses the @currentReservation@ session attribute to maintain slot data during the in-progress conversation to book a hotel or book a car. For more information, see <http://docs.aws.amazon.com/lex/latest/dg/book-trip-detail-flow.html Details of Information Flow> .      * You might use the session attributes (key, value pairs) to track the requestID of user requests. Amazon Lex simply passes these session attributes to the Lambda functions configured for the intent. In your Lambda function, you can also use the session attributes for initialization and customization (prompts and response cards). Some examples are:     * Initialization - In a pizza ordering bot, if you can pass the user location as a session attribute (for example, @"Location" : "111 Maple street"@ ), then your Lambda function might use this information to determine the closest pizzeria to place the order (perhaps to set the storeAddress slot value).      * Personalize prompts - For example, you can configure prompts to refer to the user name. (For example, "Hey [FirstName], what toppings would you like?"). You can pass the user name as a session attribute (@"FirstName" : "Joe"@ ) so that Amazon Lex can substitute the placeholder to provide a personalize prompt to the user ("Hey Joe, what toppings would you like?").
ptSessionAttributes :: Lens' PostText (HashMap Text Text)
ptSessionAttributes = lens _ptSessionAttributes (\ s a -> s{_ptSessionAttributes = a}) . _Default . _Map;

-- | The name of the Amazon Lex bot.
ptBotName :: Lens' PostText Text
ptBotName = lens _ptBotName (\ s a -> s{_ptBotName = a});

-- | The alias of the Amazon Lex bot.
ptBotAlias :: Lens' PostText Text
ptBotAlias = lens _ptBotAlias (\ s a -> s{_ptBotAlias = a});

-- | The ID of the client application user. The application developer decides the user IDs. At runtime, each request must include the user ID. Typically, each of your application users should have a unique ID. Note the following considerations:      * If you want a user to start a conversation on one device and continue the conversation on another device, you might choose a user-specific identifier, such as a login or Amazon Cognito user ID (assuming your application is using Amazon Cognito).      * If you want the same user to be able to have two independent conversations on two different devices, you might choose a device-specific identifier, such as device ID, or some globally unique identifier.
ptUserId :: Lens' PostText Text
ptUserId = lens _ptUserId (\ s a -> s{_ptUserId = a});

-- | The text that the user entered (Amazon Lex interprets this text).
ptInputText :: Lens' PostText Text
ptInputText = lens _ptInputText (\ s a -> s{_ptInputText = a});

instance AWSRequest PostText where
        type Rs PostText = PostTextResponse
        request = postJSON lexRuntime
        response
          = receiveJSON
              (\ s h x ->
                 PostTextResponse' <$>
                   (x .?> "slots" .!@ mempty) <*> (x .?> "responseCard")
                     <*> (x .?> "intentName")
                     <*> (x .?> "dialogState")
                     <*> (x .?> "message")
                     <*> (x .?> "slotToElicit")
                     <*> (x .?> "sessionAttributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable PostText

instance NFData PostText

instance ToHeaders PostText where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PostText where
        toJSON PostText'{..}
          = object
              (catMaybes
                 [("sessionAttributes" .=) <$> _ptSessionAttributes,
                  Just ("inputText" .= _ptInputText)])

instance ToPath PostText where
        toPath PostText'{..}
          = mconcat
              ["/bot/", toBS _ptBotName, "/alias/",
               toBS _ptBotAlias, "/user/", toBS _ptUserId, "/text"]

instance ToQuery PostText where
        toQuery = const mempty

-- | /See:/ 'postTextResponse' smart constructor.
data PostTextResponse = PostTextResponse'
    { _ptrsSlots             :: !(Maybe (Map Text Text))
    , _ptrsResponseCard      :: !(Maybe ResponseCard)
    , _ptrsIntentName        :: !(Maybe Text)
    , _ptrsDialogState       :: !(Maybe DialogState)
    , _ptrsMessage           :: !(Maybe Text)
    , _ptrsSlotToElicit      :: !(Maybe Text)
    , _ptrsSessionAttributes :: !(Maybe (Map Text Text))
    , _ptrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PostTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrsSlots' - The intent slots (name/value pairs) that Amazon Lex detected so far from the user input in the conversation.
--
-- * 'ptrsResponseCard' - Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
--
-- * 'ptrsIntentName' - The current user intent that Amazon Lex is aware of.
--
-- * 'ptrsDialogState' - Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.      * @ElicitIntent@ – Amazon Lex wants to elicit user intent.  For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialogState.     * @ConfirmIntent@ – Amazon Lex is expecting a "yes" or "no" response.  For example, Amazon Lex wants user confirmation before fulfilling an intent.  Instead of a simple "yes" or "no," a user might respond with additional information. For example, "yes, but make it thick crust pizza" or "no, I want to order a drink". Amazon Lex can process such additional information (in these examples, update the crust type slot value, or change intent from OrderPizza to OrderDrink).     * @ElicitSlot@ – Amazon Lex is expecting a slot value for the current intent.  For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.      * @Fulfilled@ – Conveys that the Lambda function configured for the intent has successfully fulfilled the intent.      * @ReadyForFulfillment@ – Conveys that the client has to fulfill the intent.      * @Failed@ – Conveys that the conversation with the user failed.  This can happen for various reasons including that the user did not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or the Lambda function failed to fulfill the intent.
--
-- * 'ptrsMessage' - A message to convey to the user. It can come from the bot's configuration or a code hook (Lambda function). If the current intent is not configured with a code hook or the code hook returned @Delegate@ as the @dialogAction.type@ in its response, then Amazon Lex decides the next course of action and selects an appropriate message from the bot configuration based on the current user interaction context. For example, if Amazon Lex is not able to understand the user input, it uses a clarification prompt message (for more information, see the Error Handling section in the Amazon Lex console). Another example: if the intent requires confirmation before fulfillment, then Amazon Lex uses the confirmation prompt message in the intent configuration. If the code hook returns a message, Amazon Lex passes it as-is in its response to the client.
--
-- * 'ptrsSlotToElicit' - If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- * 'ptrsSessionAttributes' - A map of key-value pairs representing the session-specific context information.
--
-- * 'ptrsResponseStatus' - -- | The response status code.
postTextResponse
    :: Int -- ^ 'ptrsResponseStatus'
    -> PostTextResponse
postTextResponse pResponseStatus_ =
    PostTextResponse'
    { _ptrsSlots = Nothing
    , _ptrsResponseCard = Nothing
    , _ptrsIntentName = Nothing
    , _ptrsDialogState = Nothing
    , _ptrsMessage = Nothing
    , _ptrsSlotToElicit = Nothing
    , _ptrsSessionAttributes = Nothing
    , _ptrsResponseStatus = pResponseStatus_
    }

-- | The intent slots (name/value pairs) that Amazon Lex detected so far from the user input in the conversation.
ptrsSlots :: Lens' PostTextResponse (HashMap Text Text)
ptrsSlots = lens _ptrsSlots (\ s a -> s{_ptrsSlots = a}) . _Default . _Map;

-- | Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
ptrsResponseCard :: Lens' PostTextResponse (Maybe ResponseCard)
ptrsResponseCard = lens _ptrsResponseCard (\ s a -> s{_ptrsResponseCard = a});

-- | The current user intent that Amazon Lex is aware of.
ptrsIntentName :: Lens' PostTextResponse (Maybe Text)
ptrsIntentName = lens _ptrsIntentName (\ s a -> s{_ptrsIntentName = a});

-- | Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.      * @ElicitIntent@ – Amazon Lex wants to elicit user intent.  For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialogState.     * @ConfirmIntent@ – Amazon Lex is expecting a "yes" or "no" response.  For example, Amazon Lex wants user confirmation before fulfilling an intent.  Instead of a simple "yes" or "no," a user might respond with additional information. For example, "yes, but make it thick crust pizza" or "no, I want to order a drink". Amazon Lex can process such additional information (in these examples, update the crust type slot value, or change intent from OrderPizza to OrderDrink).     * @ElicitSlot@ – Amazon Lex is expecting a slot value for the current intent.  For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.      * @Fulfilled@ – Conveys that the Lambda function configured for the intent has successfully fulfilled the intent.      * @ReadyForFulfillment@ – Conveys that the client has to fulfill the intent.      * @Failed@ – Conveys that the conversation with the user failed.  This can happen for various reasons including that the user did not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or the Lambda function failed to fulfill the intent.
ptrsDialogState :: Lens' PostTextResponse (Maybe DialogState)
ptrsDialogState = lens _ptrsDialogState (\ s a -> s{_ptrsDialogState = a});

-- | A message to convey to the user. It can come from the bot's configuration or a code hook (Lambda function). If the current intent is not configured with a code hook or the code hook returned @Delegate@ as the @dialogAction.type@ in its response, then Amazon Lex decides the next course of action and selects an appropriate message from the bot configuration based on the current user interaction context. For example, if Amazon Lex is not able to understand the user input, it uses a clarification prompt message (for more information, see the Error Handling section in the Amazon Lex console). Another example: if the intent requires confirmation before fulfillment, then Amazon Lex uses the confirmation prompt message in the intent configuration. If the code hook returns a message, Amazon Lex passes it as-is in its response to the client.
ptrsMessage :: Lens' PostTextResponse (Maybe Text)
ptrsMessage = lens _ptrsMessage (\ s a -> s{_ptrsMessage = a});

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
ptrsSlotToElicit :: Lens' PostTextResponse (Maybe Text)
ptrsSlotToElicit = lens _ptrsSlotToElicit (\ s a -> s{_ptrsSlotToElicit = a});

-- | A map of key-value pairs representing the session-specific context information.
ptrsSessionAttributes :: Lens' PostTextResponse (HashMap Text Text)
ptrsSessionAttributes = lens _ptrsSessionAttributes (\ s a -> s{_ptrsSessionAttributes = a}) . _Default . _Map;

-- | -- | The response status code.
ptrsResponseStatus :: Lens' PostTextResponse Int
ptrsResponseStatus = lens _ptrsResponseStatus (\ s a -> s{_ptrsResponseStatus = a});

instance NFData PostTextResponse
