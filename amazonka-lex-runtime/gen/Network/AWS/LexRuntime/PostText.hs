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
-- Sends user input text to Amazon Lex at runtime. Amazon Lex uses the machine learning model that the service built for the application to interpret user input.
--
--
-- In response, Amazon Lex returns the next message to convey to the user (based on the context of the user interaction) and whether to expect a user response to the message (@dialogState@ ). For example, consider the following response messages:
--
--     * "What pizza toppings would you like?" – In this case, the @dialogState@ would be @ElicitSlot@ (that is, a user response is expected).
--
--     * "Your order has been placed." – In this case, Amazon Lex returns one of the following @dialogState@ values depending on how the intent fulfillment is configured (see @fulfillmentActivity@ in @CreateIntent@ ):
--
--     * @FulFilled@ – The intent fulfillment is configured through a Lambda function.
--
--     * @ReadyForFulfilment@ – The intent's @fulfillmentActivity@ is to simply return the intent data back to the client application.
--
--
--
--
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
-- * 'ptSessionAttributes' - A session represents the dialog between a user and Amazon Lex. At runtime, a client application can pass contextual information (session attributes) in the request. For example, @"FirstName" : "Joe"@ . Amazon Lex passes these session attributes to the AWS Lambda functions configured for the intent (see @dialogCodeHook@ and @fulfillmentActivity.codeHook@ in @CreateIntent@ ).  In your Lambda function, you can use the session attributes for customization. Some examples are:     * In a pizza ordering application, if you can pass user location as a session attribute (for example, @"Location" : "111 Maple street"@ ), your Lambda function might use this information to determine the closest pizzeria to place the order.      * Use session attributes to personalize prompts. For example, you pass in user name as a session attribute (@"FirstName" : "Joe"@ ), you might configure subsequent prompts to refer to this attribute, as @> session.FirstName"@ . At runtime, Amazon Lex substitutes a real value when it generates a prompt, such as "Hello Joe, what would you like to order?"
--
-- * 'ptBotName' - Name of the Amazon Lex bot.
--
-- * 'ptBotAlias' - Alias of the Amazon Lex bot.
--
-- * 'ptUserId' - User ID of your client application. Typically, each of your application users should have a unique ID. Note the following considerations:      * If you want a user to start a conversation on one mobile device and continue the conversation on another device, you might choose a user-specific identifier, such as a login or Amazon Cognito user ID (assuming your application is using Amazon Cognito).      * If you want the same user to be able to have two independent conversations on two different devices, you might choose a device-specific identifier, such as device ID, or some globally unique identifier.
--
-- * 'ptInputText' - Text user entered (Amazon Lex interprets this text).
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

-- | A session represents the dialog between a user and Amazon Lex. At runtime, a client application can pass contextual information (session attributes) in the request. For example, @"FirstName" : "Joe"@ . Amazon Lex passes these session attributes to the AWS Lambda functions configured for the intent (see @dialogCodeHook@ and @fulfillmentActivity.codeHook@ in @CreateIntent@ ).  In your Lambda function, you can use the session attributes for customization. Some examples are:     * In a pizza ordering application, if you can pass user location as a session attribute (for example, @"Location" : "111 Maple street"@ ), your Lambda function might use this information to determine the closest pizzeria to place the order.      * Use session attributes to personalize prompts. For example, you pass in user name as a session attribute (@"FirstName" : "Joe"@ ), you might configure subsequent prompts to refer to this attribute, as @> session.FirstName"@ . At runtime, Amazon Lex substitutes a real value when it generates a prompt, such as "Hello Joe, what would you like to order?"
ptSessionAttributes :: Lens' PostText (HashMap Text Text)
ptSessionAttributes = lens _ptSessionAttributes (\ s a -> s{_ptSessionAttributes = a}) . _Default . _Map;

-- | Name of the Amazon Lex bot.
ptBotName :: Lens' PostText Text
ptBotName = lens _ptBotName (\ s a -> s{_ptBotName = a});

-- | Alias of the Amazon Lex bot.
ptBotAlias :: Lens' PostText Text
ptBotAlias = lens _ptBotAlias (\ s a -> s{_ptBotAlias = a});

-- | User ID of your client application. Typically, each of your application users should have a unique ID. Note the following considerations:      * If you want a user to start a conversation on one mobile device and continue the conversation on another device, you might choose a user-specific identifier, such as a login or Amazon Cognito user ID (assuming your application is using Amazon Cognito).      * If you want the same user to be able to have two independent conversations on two different devices, you might choose a device-specific identifier, such as device ID, or some globally unique identifier.
ptUserId :: Lens' PostText Text
ptUserId = lens _ptUserId (\ s a -> s{_ptUserId = a});

-- | Text user entered (Amazon Lex interprets this text).
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
-- * 'ptrsSlots' - Intent slots (name/value pairs) Amazon Lex detected so far from the user input in the conversation.
--
-- * 'ptrsResponseCard' - Represents the options that the user has to respond to the current prompt. Amazon Lex sends this in the response only if the @dialogState@ value indicates that a user response is expected.
--
-- * 'ptrsIntentName' - Intent Amazon Lex inferred from the user input text. This is one of the intents configured for the bot.
--
-- * 'ptrsDialogState' - Represents the message type to be conveyed to the user. For example:      * @ElicitIntent@ – Amazon Lex wants to elicit user intent. For example, Amazon Lex did not understand the first utterances such as "I want to order pizza", which indicates the OrderPizza intent. If Amazon Lex doesn't understand the intent, it returns this @dialogState@ . Another example is when your intent is configured with a follow up prompt. For example, after OrderPizza intent is fulfilled, the intent might have a follow up prompt such as " Do you want to order a drink or desert?" In this case, Amazon Lex returns this @dialogState@ .      * @ConfirmIntent@ – Amazon Lex is expecting a yes/no response from the user indicating whether to go ahead and fulfill the intent (for example, OK to go ahead and order the pizza). In addition to a yes/no reply, the user might provide a response with additional slot information (either new slot information or changes to the existing slot values). For example, "Yes, but change to thick crust." Amazon Lex understands the additional information and updates the intent slots accordingly.  Consider another example. Before fulfilling an order, your application might prompt for confirmation such as "Do you want to place this pizza order?" A user might reply with "No, I want to order a drink." Amazon Lex recognizes the new OrderDrink intent.      * @ElicitSlot@ – Amazon Lex is expecting a value of a slot for the current intent. For example, suppose Amazon Lex asks, "What size pizza would you like?" A user might reply with "Medium pepperoni pizza." Amazon Lex recognizes the size and the topping as the two separate slot values.      * @Fulfilled@ – Conveys that the Lambda function has successfully fulfilled the intent. If Lambda function returns a statement/message to convey the fulfillment result, Amazon Lex passes this string to the client. If not, Amazon Lex looks for @conclusionStatement@ that you configured for the intent.  If both the Lambda function statement and the @conclusionStatement@ are missing, Amazon Lex throws a bad request exception.      * @ReadyForFulfillment@ – conveys that the client has to do the fulfillment work for the intent. This is the case when the current intent is configured with @ReturnIntent@ as the @fulfillmentActivity @ , where Amazon Lex returns this state to client.      * @Failed@ – Conversation with the user failed. Some of the reasons for this @dialogState@ are: after the configured number of attempts the user didn't provide an appropriate response, or the Lambda function failed to fulfill an intent.
--
-- * 'ptrsMessage' - Prompt (or statement) to convey to the user. This is based on the application configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the application. In another example, if the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Suppose the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. In that situation, Amazon Lex sends that message in the response.
--
-- * 'ptrsSlotToElicit' - If @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- * 'ptrsSessionAttributes' - Map of key value pairs representing the session specific context information.
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

-- | Intent slots (name/value pairs) Amazon Lex detected so far from the user input in the conversation.
ptrsSlots :: Lens' PostTextResponse (HashMap Text Text)
ptrsSlots = lens _ptrsSlots (\ s a -> s{_ptrsSlots = a}) . _Default . _Map;

-- | Represents the options that the user has to respond to the current prompt. Amazon Lex sends this in the response only if the @dialogState@ value indicates that a user response is expected.
ptrsResponseCard :: Lens' PostTextResponse (Maybe ResponseCard)
ptrsResponseCard = lens _ptrsResponseCard (\ s a -> s{_ptrsResponseCard = a});

-- | Intent Amazon Lex inferred from the user input text. This is one of the intents configured for the bot.
ptrsIntentName :: Lens' PostTextResponse (Maybe Text)
ptrsIntentName = lens _ptrsIntentName (\ s a -> s{_ptrsIntentName = a});

-- | Represents the message type to be conveyed to the user. For example:      * @ElicitIntent@ – Amazon Lex wants to elicit user intent. For example, Amazon Lex did not understand the first utterances such as "I want to order pizza", which indicates the OrderPizza intent. If Amazon Lex doesn't understand the intent, it returns this @dialogState@ . Another example is when your intent is configured with a follow up prompt. For example, after OrderPizza intent is fulfilled, the intent might have a follow up prompt such as " Do you want to order a drink or desert?" In this case, Amazon Lex returns this @dialogState@ .      * @ConfirmIntent@ – Amazon Lex is expecting a yes/no response from the user indicating whether to go ahead and fulfill the intent (for example, OK to go ahead and order the pizza). In addition to a yes/no reply, the user might provide a response with additional slot information (either new slot information or changes to the existing slot values). For example, "Yes, but change to thick crust." Amazon Lex understands the additional information and updates the intent slots accordingly.  Consider another example. Before fulfilling an order, your application might prompt for confirmation such as "Do you want to place this pizza order?" A user might reply with "No, I want to order a drink." Amazon Lex recognizes the new OrderDrink intent.      * @ElicitSlot@ – Amazon Lex is expecting a value of a slot for the current intent. For example, suppose Amazon Lex asks, "What size pizza would you like?" A user might reply with "Medium pepperoni pizza." Amazon Lex recognizes the size and the topping as the two separate slot values.      * @Fulfilled@ – Conveys that the Lambda function has successfully fulfilled the intent. If Lambda function returns a statement/message to convey the fulfillment result, Amazon Lex passes this string to the client. If not, Amazon Lex looks for @conclusionStatement@ that you configured for the intent.  If both the Lambda function statement and the @conclusionStatement@ are missing, Amazon Lex throws a bad request exception.      * @ReadyForFulfillment@ – conveys that the client has to do the fulfillment work for the intent. This is the case when the current intent is configured with @ReturnIntent@ as the @fulfillmentActivity @ , where Amazon Lex returns this state to client.      * @Failed@ – Conversation with the user failed. Some of the reasons for this @dialogState@ are: after the configured number of attempts the user didn't provide an appropriate response, or the Lambda function failed to fulfill an intent.
ptrsDialogState :: Lens' PostTextResponse (Maybe DialogState)
ptrsDialogState = lens _ptrsDialogState (\ s a -> s{_ptrsDialogState = a});

-- | Prompt (or statement) to convey to the user. This is based on the application configuration and context. For example, if Amazon Lex did not understand the user intent, it sends the @clarificationPrompt@ configured for the application. In another example, if the intent requires confirmation before taking the fulfillment action, it sends the @confirmationPrompt@ . Suppose the Lambda function successfully fulfilled the intent, and sent a message to convey to the user. In that situation, Amazon Lex sends that message in the response.
ptrsMessage :: Lens' PostTextResponse (Maybe Text)
ptrsMessage = lens _ptrsMessage (\ s a -> s{_ptrsMessage = a});

-- | If @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
ptrsSlotToElicit :: Lens' PostTextResponse (Maybe Text)
ptrsSlotToElicit = lens _ptrsSlotToElicit (\ s a -> s{_ptrsSlotToElicit = a});

-- | Map of key value pairs representing the session specific context information.
ptrsSessionAttributes :: Lens' PostTextResponse (HashMap Text Text)
ptrsSessionAttributes = lens _ptrsSessionAttributes (\ s a -> s{_ptrsSessionAttributes = a}) . _Default . _Map;

-- | -- | The response status code.
ptrsResponseStatus :: Lens' PostTextResponse Int
ptrsResponseStatus = lens _ptrsResponseStatus (\ s a -> s{_ptrsResponseStatus = a});

instance NFData PostTextResponse
