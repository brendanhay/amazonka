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
-- Module      : Network.AWS.LexModels.PutIntent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an intent or replaces an existing intent.
--
--
-- To define the interaction between the user and your bot, you use one or more intents. For a pizza ordering bot, for example, you would create an @OrderPizza@ intent.
--
-- To create an intent or replace an existing intent, you must provide the following:
--
--     * Intent name. For example, @OrderPizza@ .
--
--     * Sample utterances. For example, "Can I order a pizza, please." and "I want to order a pizza."
--
--     * Information to be gathered. You specify slot types for the information that your bot will request from the user. You can specify standard slot types, such as a date or a time, or custom slot types such as the size and crust of a pizza.
--
--     * How the intent will be fulfilled. You can provide a Lambda function or configure the intent to return the intent information to the client application. If you use a Lambda function, when all of the intent information is available, Amazon Lex invokes your Lambda function. If you configure your intent to return the intent information to the client application.
--
--
--
-- You can specify other optional information in the request, such as:
--
--     * A confirmation prompt to ask the user to confirm an intent. For example, "Shall I order your pizza?"
--
--     * A conclusion statement to send to the user after the intent has been fulfilled. For example, "I placed your pizza order."
--
--     * A follow-up prompt that asks the user for additional activity. For example, asking "Do you want to order a drink with your pizza?"
--
--
--
-- If you specify an existing intent name to update the intent, Amazon Lex replaces the values in the @> LATEST@ version of the intent with the values in the request. Amazon Lex removes fields that you don't provide in the request. If you don't specify the required fields, Amazon Lex throws an exception. When you update the @> LATEST@ version of an intent, the @status@ field of any bot that uses the @> LATEST@ version of the intent is set to @NOT_BUILT@ .
--
-- For more information, see 'how-it-works' .
--
-- This operation requires permissions for the @lex:PutIntent@ action.
--
module Network.AWS.LexModels.PutIntent
    (
    -- * Creating a Request
      putIntent
    , PutIntent
    -- * Request Lenses
    , piFulfillmentActivity
    , piSlots
    , piRejectionStatement
    , piChecksum
    , piConclusionStatement
    , piSampleUtterances
    , piParentIntentSignature
    , piFollowUpPrompt
    , piConfirmationPrompt
    , piCreateVersion
    , piDialogCodeHook
    , piDescription
    , piName

    -- * Destructuring the Response
    , putIntentResponse
    , PutIntentResponse
    -- * Response Lenses
    , pirsFulfillmentActivity
    , pirsSlots
    , pirsRejectionStatement
    , pirsChecksum
    , pirsConclusionStatement
    , pirsSampleUtterances
    , pirsParentIntentSignature
    , pirsCreatedDate
    , pirsName
    , pirsVersion
    , pirsFollowUpPrompt
    , pirsLastUpdatedDate
    , pirsConfirmationPrompt
    , pirsCreateVersion
    , pirsDialogCodeHook
    , pirsDescription
    , pirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putIntent' smart constructor.
data PutIntent = PutIntent'
  { _piFulfillmentActivity   :: !(Maybe FulfillmentActivity)
  , _piSlots                 :: !(Maybe [Slot])
  , _piRejectionStatement    :: !(Maybe Statement)
  , _piChecksum              :: !(Maybe Text)
  , _piConclusionStatement   :: !(Maybe Statement)
  , _piSampleUtterances      :: !(Maybe [Text])
  , _piParentIntentSignature :: !(Maybe Text)
  , _piFollowUpPrompt        :: !(Maybe FollowUpPrompt)
  , _piConfirmationPrompt    :: !(Maybe Prompt)
  , _piCreateVersion         :: !(Maybe Bool)
  , _piDialogCodeHook        :: !(Maybe CodeHook)
  , _piDescription           :: !(Maybe Text)
  , _piName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutIntent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piFulfillmentActivity' - Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store.  You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria).
--
-- * 'piSlots' - An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' .
--
-- * 'piRejectionStatement' - When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- * 'piChecksum' - Identifies a specific revision of the @> LATEST@ version. When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- * 'piConclusionStatement' - The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function.  This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
--
-- * 'piSampleUtterances' - An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas".  In each utterance, a slot name is enclosed in curly braces.
--
-- * 'piParentIntentSignature' - A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
--
-- * 'piFollowUpPrompt' - Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink. The action that Amazon Lex takes depends on the user's response, as follows:     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.     * If it doesn't recognize the utterance it repeats the follow-up prompt again. The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one.
--
-- * 'piConfirmationPrompt' - Prompts the user to confirm the intent. This question should have a yes or no answer. Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information.
--
-- * 'piCreateVersion' - Undocumented member.
--
-- * 'piDialogCodeHook' - Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction.  For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute.
--
-- * 'piDescription' - A description of the intent.
--
-- * 'piName' - The name of the intent. The name is /not/ case sensitive.  The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ . For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
putIntent
    :: Text -- ^ 'piName'
    -> PutIntent
putIntent pName_ =
  PutIntent'
    { _piFulfillmentActivity = Nothing
    , _piSlots = Nothing
    , _piRejectionStatement = Nothing
    , _piChecksum = Nothing
    , _piConclusionStatement = Nothing
    , _piSampleUtterances = Nothing
    , _piParentIntentSignature = Nothing
    , _piFollowUpPrompt = Nothing
    , _piConfirmationPrompt = Nothing
    , _piCreateVersion = Nothing
    , _piDialogCodeHook = Nothing
    , _piDescription = Nothing
    , _piName = pName_
    }


-- | Required. Describes how the intent is fulfilled. For example, after a user provides all of the information for a pizza order, @fulfillmentActivity@ defines how the bot places an order with a local pizza store.  You might configure Amazon Lex to return all of the intent information to the client application, or direct it to invoke a Lambda function that can process the intent (for example, place an order with a pizzeria).
piFulfillmentActivity :: Lens' PutIntent (Maybe FulfillmentActivity)
piFulfillmentActivity = lens _piFulfillmentActivity (\ s a -> s{_piFulfillmentActivity = a})

-- | An array of intent slots. At runtime, Amazon Lex elicits required slot values from the user using prompts defined in the slots. For more information, see 'how-it-works' .
piSlots :: Lens' PutIntent [Slot]
piSlots = lens _piSlots (\ s a -> s{_piSlots = a}) . _Default . _Coerce

-- | When the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
piRejectionStatement :: Lens' PutIntent (Maybe Statement)
piRejectionStatement = lens _piRejectionStatement (\ s a -> s{_piRejectionStatement = a})

-- | Identifies a specific revision of the @> LATEST@ version. When you create a new intent, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a intent, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
piChecksum :: Lens' PutIntent (Maybe Text)
piChecksum = lens _piChecksum (\ s a -> s{_piChecksum = a})

-- | The statement that you want Amazon Lex to convey to the user after the intent is successfully fulfilled by the Lambda function.  This element is relevant only if you provide a Lambda function in the @fulfillmentActivity@ . If you return the intent to the client application, you can't specify this element.
piConclusionStatement :: Lens' PutIntent (Maybe Statement)
piConclusionStatement = lens _piConclusionStatement (\ s a -> s{_piConclusionStatement = a})

-- | An array of utterances (strings) that a user might say to signal the intent. For example, "I want {PizzaSize} pizza", "Order {Quantity} {PizzaSize} pizzas".  In each utterance, a slot name is enclosed in curly braces.
piSampleUtterances :: Lens' PutIntent [Text]
piSampleUtterances = lens _piSampleUtterances (\ s a -> s{_piSampleUtterances = a}) . _Default . _Coerce

-- | A unique identifier for the built-in intent to base this intent on. To find the signature for an intent, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
piParentIntentSignature :: Lens' PutIntent (Maybe Text)
piParentIntentSignature = lens _piParentIntentSignature (\ s a -> s{_piParentIntentSignature = a})

-- | Amazon Lex uses this prompt to solicit additional activity after fulfilling an intent. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to order a drink. The action that Amazon Lex takes depends on the user's response, as follows:     * If the user says "Yes" it responds with the clarification prompt that is configured for the bot.     * if the user says "Yes" and continues with an utterance that triggers an intent it starts a conversation for the intent.     * If the user says "No" it responds with the rejection statement configured for the the follow-up prompt.     * If it doesn't recognize the utterance it repeats the follow-up prompt again. The @followUpPrompt@ field and the @conclusionStatement@ field are mutually exclusive. You can specify only one.
piFollowUpPrompt :: Lens' PutIntent (Maybe FollowUpPrompt)
piFollowUpPrompt = lens _piFollowUpPrompt (\ s a -> s{_piFollowUpPrompt = a})

-- | Prompts the user to confirm the intent. This question should have a yes or no answer. Amazon Lex uses this prompt to ensure that the user acknowledges that the intent is ready for fulfillment. For example, with the @OrderPizza@ intent, you might want to confirm that the order is correct before placing it. For other intents, such as intents that simply respond to user questions, you might not need to ask the user for confirmation before providing the information.
piConfirmationPrompt :: Lens' PutIntent (Maybe Prompt)
piConfirmationPrompt = lens _piConfirmationPrompt (\ s a -> s{_piConfirmationPrompt = a})

-- | Undocumented member.
piCreateVersion :: Lens' PutIntent (Maybe Bool)
piCreateVersion = lens _piCreateVersion (\ s a -> s{_piCreateVersion = a})

-- | Specifies a Lambda function to invoke for each user input. You can invoke this Lambda function to personalize user interaction.  For example, suppose your bot determines that the user is John. Your Lambda function might retrieve John's information from a backend database and prepopulate some of the values. For example, if you find that John is gluten intolerant, you might set the corresponding intent slot, @GlutenIntolerant@ , to true. You might find John's phone number and set the corresponding session attribute.
piDialogCodeHook :: Lens' PutIntent (Maybe CodeHook)
piDialogCodeHook = lens _piDialogCodeHook (\ s a -> s{_piDialogCodeHook = a})

-- | A description of the intent.
piDescription :: Lens' PutIntent (Maybe Text)
piDescription = lens _piDescription (\ s a -> s{_piDescription = a})

-- | The name of the intent. The name is /not/ case sensitive.  The name can't match a built-in intent name, or a built-in intent name with "AMAZON." removed. For example, because there is a built-in intent called @AMAZON.HelpIntent@ , you can't create a custom intent called @HelpIntent@ . For a list of built-in intents, see <https://developer.amazon.com/public/solutions/alexa/alexa-skills-kit/docs/built-in-intent-ref/standard-intents Standard Built-in Intents> in the /Alexa Skills Kit/ .
piName :: Lens' PutIntent Text
piName = lens _piName (\ s a -> s{_piName = a})

instance AWSRequest PutIntent where
        type Rs PutIntent = PutIntentResponse
        request = putJSON lexModels
        response
          = receiveJSON
              (\ s h x ->
                 PutIntentResponse' <$>
                   (x .?> "fulfillmentActivity") <*>
                     (x .?> "slots" .!@ mempty)
                     <*> (x .?> "rejectionStatement")
                     <*> (x .?> "checksum")
                     <*> (x .?> "conclusionStatement")
                     <*> (x .?> "sampleUtterances" .!@ mempty)
                     <*> (x .?> "parentIntentSignature")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "followUpPrompt")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "confirmationPrompt")
                     <*> (x .?> "createVersion")
                     <*> (x .?> "dialogCodeHook")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable PutIntent where

instance NFData PutIntent where

instance ToHeaders PutIntent where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutIntent where
        toJSON PutIntent'{..}
          = object
              (catMaybes
                 [("fulfillmentActivity" .=) <$>
                    _piFulfillmentActivity,
                  ("slots" .=) <$> _piSlots,
                  ("rejectionStatement" .=) <$> _piRejectionStatement,
                  ("checksum" .=) <$> _piChecksum,
                  ("conclusionStatement" .=) <$>
                    _piConclusionStatement,
                  ("sampleUtterances" .=) <$> _piSampleUtterances,
                  ("parentIntentSignature" .=) <$>
                    _piParentIntentSignature,
                  ("followUpPrompt" .=) <$> _piFollowUpPrompt,
                  ("confirmationPrompt" .=) <$> _piConfirmationPrompt,
                  ("createVersion" .=) <$> _piCreateVersion,
                  ("dialogCodeHook" .=) <$> _piDialogCodeHook,
                  ("description" .=) <$> _piDescription])

instance ToPath PutIntent where
        toPath PutIntent'{..}
          = mconcat
              ["/intents/", toBS _piName, "/versions/$LATEST"]

instance ToQuery PutIntent where
        toQuery = const mempty

-- | /See:/ 'putIntentResponse' smart constructor.
data PutIntentResponse = PutIntentResponse'
  { _pirsFulfillmentActivity   :: !(Maybe FulfillmentActivity)
  , _pirsSlots                 :: !(Maybe [Slot])
  , _pirsRejectionStatement    :: !(Maybe Statement)
  , _pirsChecksum              :: !(Maybe Text)
  , _pirsConclusionStatement   :: !(Maybe Statement)
  , _pirsSampleUtterances      :: !(Maybe [Text])
  , _pirsParentIntentSignature :: !(Maybe Text)
  , _pirsCreatedDate           :: !(Maybe POSIX)
  , _pirsName                  :: !(Maybe Text)
  , _pirsVersion               :: !(Maybe Text)
  , _pirsFollowUpPrompt        :: !(Maybe FollowUpPrompt)
  , _pirsLastUpdatedDate       :: !(Maybe POSIX)
  , _pirsConfirmationPrompt    :: !(Maybe Prompt)
  , _pirsCreateVersion         :: !(Maybe Bool)
  , _pirsDialogCodeHook        :: !(Maybe CodeHook)
  , _pirsDescription           :: !(Maybe Text)
  , _pirsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutIntentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pirsFulfillmentActivity' - If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
--
-- * 'pirsSlots' - An array of intent slots that are configured for the intent.
--
-- * 'pirsRejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- * 'pirsChecksum' - Checksum of the @> LATEST@ version of the intent created or updated.
--
-- * 'pirsConclusionStatement' - After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- * 'pirsSampleUtterances' - An array of sample utterances that are configured for the intent.
--
-- * 'pirsParentIntentSignature' - A unique identifier for the built-in intent that this intent is based on.
--
-- * 'pirsCreatedDate' - The date that the intent was created.
--
-- * 'pirsName' - The name of the intent.
--
-- * 'pirsVersion' - The version of the intent. For a new intent, the version is always @> LATEST@ .
--
-- * 'pirsFollowUpPrompt' - If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
--
-- * 'pirsLastUpdatedDate' - The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
--
-- * 'pirsConfirmationPrompt' - If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
--
-- * 'pirsCreateVersion' - Undocumented member.
--
-- * 'pirsDialogCodeHook' - If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
--
-- * 'pirsDescription' - A description of the intent.
--
-- * 'pirsResponseStatus' - -- | The response status code.
putIntentResponse
    :: Int -- ^ 'pirsResponseStatus'
    -> PutIntentResponse
putIntentResponse pResponseStatus_ =
  PutIntentResponse'
    { _pirsFulfillmentActivity = Nothing
    , _pirsSlots = Nothing
    , _pirsRejectionStatement = Nothing
    , _pirsChecksum = Nothing
    , _pirsConclusionStatement = Nothing
    , _pirsSampleUtterances = Nothing
    , _pirsParentIntentSignature = Nothing
    , _pirsCreatedDate = Nothing
    , _pirsName = Nothing
    , _pirsVersion = Nothing
    , _pirsFollowUpPrompt = Nothing
    , _pirsLastUpdatedDate = Nothing
    , _pirsConfirmationPrompt = Nothing
    , _pirsCreateVersion = Nothing
    , _pirsDialogCodeHook = Nothing
    , _pirsDescription = Nothing
    , _pirsResponseStatus = pResponseStatus_
    }


-- | If defined in the intent, Amazon Lex invokes this Lambda function to fulfill the intent after the user provides all of the information required by the intent.
pirsFulfillmentActivity :: Lens' PutIntentResponse (Maybe FulfillmentActivity)
pirsFulfillmentActivity = lens _pirsFulfillmentActivity (\ s a -> s{_pirsFulfillmentActivity = a})

-- | An array of intent slots that are configured for the intent.
pirsSlots :: Lens' PutIntentResponse [Slot]
pirsSlots = lens _pirsSlots (\ s a -> s{_pirsSlots = a}) . _Default . _Coerce

-- | If the user answers "no" to the question defined in @confirmationPrompt@ Amazon Lex responds with this statement to acknowledge that the intent was canceled.
pirsRejectionStatement :: Lens' PutIntentResponse (Maybe Statement)
pirsRejectionStatement = lens _pirsRejectionStatement (\ s a -> s{_pirsRejectionStatement = a})

-- | Checksum of the @> LATEST@ version of the intent created or updated.
pirsChecksum :: Lens' PutIntentResponse (Maybe Text)
pirsChecksum = lens _pirsChecksum (\ s a -> s{_pirsChecksum = a})

-- | After the Lambda function specified in the@fulfillmentActivity@ intent fulfills the intent, Amazon Lex conveys this statement to the user.
pirsConclusionStatement :: Lens' PutIntentResponse (Maybe Statement)
pirsConclusionStatement = lens _pirsConclusionStatement (\ s a -> s{_pirsConclusionStatement = a})

-- | An array of sample utterances that are configured for the intent.
pirsSampleUtterances :: Lens' PutIntentResponse [Text]
pirsSampleUtterances = lens _pirsSampleUtterances (\ s a -> s{_pirsSampleUtterances = a}) . _Default . _Coerce

-- | A unique identifier for the built-in intent that this intent is based on.
pirsParentIntentSignature :: Lens' PutIntentResponse (Maybe Text)
pirsParentIntentSignature = lens _pirsParentIntentSignature (\ s a -> s{_pirsParentIntentSignature = a})

-- | The date that the intent was created.
pirsCreatedDate :: Lens' PutIntentResponse (Maybe UTCTime)
pirsCreatedDate = lens _pirsCreatedDate (\ s a -> s{_pirsCreatedDate = a}) . mapping _Time

-- | The name of the intent.
pirsName :: Lens' PutIntentResponse (Maybe Text)
pirsName = lens _pirsName (\ s a -> s{_pirsName = a})

-- | The version of the intent. For a new intent, the version is always @> LATEST@ .
pirsVersion :: Lens' PutIntentResponse (Maybe Text)
pirsVersion = lens _pirsVersion (\ s a -> s{_pirsVersion = a})

-- | If defined in the intent, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
pirsFollowUpPrompt :: Lens' PutIntentResponse (Maybe FollowUpPrompt)
pirsFollowUpPrompt = lens _pirsFollowUpPrompt (\ s a -> s{_pirsFollowUpPrompt = a})

-- | The date that the intent was updated. When you create a resource, the creation date and last update dates are the same.
pirsLastUpdatedDate :: Lens' PutIntentResponse (Maybe UTCTime)
pirsLastUpdatedDate = lens _pirsLastUpdatedDate (\ s a -> s{_pirsLastUpdatedDate = a}) . mapping _Time

-- | If defined in the intent, Amazon Lex prompts the user to confirm the intent before fulfilling it.
pirsConfirmationPrompt :: Lens' PutIntentResponse (Maybe Prompt)
pirsConfirmationPrompt = lens _pirsConfirmationPrompt (\ s a -> s{_pirsConfirmationPrompt = a})

-- | Undocumented member.
pirsCreateVersion :: Lens' PutIntentResponse (Maybe Bool)
pirsCreateVersion = lens _pirsCreateVersion (\ s a -> s{_pirsCreateVersion = a})

-- | If defined in the intent, Amazon Lex invokes this Lambda function for each user input.
pirsDialogCodeHook :: Lens' PutIntentResponse (Maybe CodeHook)
pirsDialogCodeHook = lens _pirsDialogCodeHook (\ s a -> s{_pirsDialogCodeHook = a})

-- | A description of the intent.
pirsDescription :: Lens' PutIntentResponse (Maybe Text)
pirsDescription = lens _pirsDescription (\ s a -> s{_pirsDescription = a})

-- | -- | The response status code.
pirsResponseStatus :: Lens' PutIntentResponse Int
pirsResponseStatus = lens _pirsResponseStatus (\ s a -> s{_pirsResponseStatus = a})

instance NFData PutIntentResponse where
