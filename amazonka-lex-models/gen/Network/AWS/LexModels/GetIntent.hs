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
-- Module      : Network.AWS.LexModels.GetIntent
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an intent. In addition to the intent name, you must specify the intent version.
--
--
-- This operation requires permissions to perform the @lex:GetIntent@ action.
--
module Network.AWS.LexModels.GetIntent
    (
    -- * Creating a Request
      getIntent
    , GetIntent
    -- * Request Lenses
    , giName
    , giVersion

    -- * Destructuring the Response
    , getIntentResponse
    , GetIntentResponse
    -- * Response Lenses
    , girsFulfillmentActivity
    , girsSlots
    , girsRejectionStatement
    , girsChecksum
    , girsConclusionStatement
    , girsSampleUtterances
    , girsParentIntentSignature
    , girsCreatedDate
    , girsName
    , girsVersion
    , girsFollowUpPrompt
    , girsLastUpdatedDate
    , girsConfirmationPrompt
    , girsDialogCodeHook
    , girsDescription
    , girsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getIntent' smart constructor.
data GetIntent = GetIntent'
  { _giName    :: !Text
  , _giVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giName' - The name of the intent. The name is case sensitive.
--
-- * 'giVersion' - The version of the intent.
getIntent
    :: Text -- ^ 'giName'
    -> Text -- ^ 'giVersion'
    -> GetIntent
getIntent pName_ pVersion_ =
  GetIntent' {_giName = pName_, _giVersion = pVersion_}


-- | The name of the intent. The name is case sensitive.
giName :: Lens' GetIntent Text
giName = lens _giName (\ s a -> s{_giName = a});

-- | The version of the intent.
giVersion :: Lens' GetIntent Text
giVersion = lens _giVersion (\ s a -> s{_giVersion = a});

instance AWSRequest GetIntent where
        type Rs GetIntent = GetIntentResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetIntentResponse' <$>
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
                     <*> (x .?> "dialogCodeHook")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable GetIntent where

instance NFData GetIntent where

instance ToHeaders GetIntent where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetIntent where
        toPath GetIntent'{..}
          = mconcat
              ["/intents/", toBS _giName, "/versions/",
               toBS _giVersion]

instance ToQuery GetIntent where
        toQuery = const mempty

-- | /See:/ 'getIntentResponse' smart constructor.
data GetIntentResponse = GetIntentResponse'
  { _girsFulfillmentActivity   :: !(Maybe FulfillmentActivity)
  , _girsSlots                 :: !(Maybe [Slot])
  , _girsRejectionStatement    :: !(Maybe Statement)
  , _girsChecksum              :: !(Maybe Text)
  , _girsConclusionStatement   :: !(Maybe Statement)
  , _girsSampleUtterances      :: !(Maybe [Text])
  , _girsParentIntentSignature :: !(Maybe Text)
  , _girsCreatedDate           :: !(Maybe POSIX)
  , _girsName                  :: !(Maybe Text)
  , _girsVersion               :: !(Maybe Text)
  , _girsFollowUpPrompt        :: !(Maybe FollowUpPrompt)
  , _girsLastUpdatedDate       :: !(Maybe POSIX)
  , _girsConfirmationPrompt    :: !(Maybe Prompt)
  , _girsDialogCodeHook        :: !(Maybe CodeHook)
  , _girsDescription           :: !(Maybe Text)
  , _girsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsFulfillmentActivity' - Describes how the intent is fulfilled. For more information, see 'PutIntent' .
--
-- * 'girsSlots' - An array of intent slots configured for the intent.
--
-- * 'girsRejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- * 'girsChecksum' - Checksum of the intent.
--
-- * 'girsConclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- * 'girsSampleUtterances' - An array of sample utterances configured for the intent.
--
-- * 'girsParentIntentSignature' - A unique identifier for a built-in intent.
--
-- * 'girsCreatedDate' - The date that the intent was created.
--
-- * 'girsName' - The name of the intent.
--
-- * 'girsVersion' - The version of the intent.
--
-- * 'girsFollowUpPrompt' - If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
--
-- * 'girsLastUpdatedDate' - The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- * 'girsConfirmationPrompt' - If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
--
-- * 'girsDialogCodeHook' - If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
--
-- * 'girsDescription' - A description of the intent.
--
-- * 'girsResponseStatus' - -- | The response status code.
getIntentResponse
    :: Int -- ^ 'girsResponseStatus'
    -> GetIntentResponse
getIntentResponse pResponseStatus_ =
  GetIntentResponse'
  { _girsFulfillmentActivity = Nothing
  , _girsSlots = Nothing
  , _girsRejectionStatement = Nothing
  , _girsChecksum = Nothing
  , _girsConclusionStatement = Nothing
  , _girsSampleUtterances = Nothing
  , _girsParentIntentSignature = Nothing
  , _girsCreatedDate = Nothing
  , _girsName = Nothing
  , _girsVersion = Nothing
  , _girsFollowUpPrompt = Nothing
  , _girsLastUpdatedDate = Nothing
  , _girsConfirmationPrompt = Nothing
  , _girsDialogCodeHook = Nothing
  , _girsDescription = Nothing
  , _girsResponseStatus = pResponseStatus_
  }


-- | Describes how the intent is fulfilled. For more information, see 'PutIntent' .
girsFulfillmentActivity :: Lens' GetIntentResponse (Maybe FulfillmentActivity)
girsFulfillmentActivity = lens _girsFulfillmentActivity (\ s a -> s{_girsFulfillmentActivity = a});

-- | An array of intent slots configured for the intent.
girsSlots :: Lens' GetIntentResponse [Slot]
girsSlots = lens _girsSlots (\ s a -> s{_girsSlots = a}) . _Default . _Coerce;

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
girsRejectionStatement :: Lens' GetIntentResponse (Maybe Statement)
girsRejectionStatement = lens _girsRejectionStatement (\ s a -> s{_girsRejectionStatement = a});

-- | Checksum of the intent.
girsChecksum :: Lens' GetIntentResponse (Maybe Text)
girsChecksum = lens _girsChecksum (\ s a -> s{_girsChecksum = a});

-- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
girsConclusionStatement :: Lens' GetIntentResponse (Maybe Statement)
girsConclusionStatement = lens _girsConclusionStatement (\ s a -> s{_girsConclusionStatement = a});

-- | An array of sample utterances configured for the intent.
girsSampleUtterances :: Lens' GetIntentResponse [Text]
girsSampleUtterances = lens _girsSampleUtterances (\ s a -> s{_girsSampleUtterances = a}) . _Default . _Coerce;

-- | A unique identifier for a built-in intent.
girsParentIntentSignature :: Lens' GetIntentResponse (Maybe Text)
girsParentIntentSignature = lens _girsParentIntentSignature (\ s a -> s{_girsParentIntentSignature = a});

-- | The date that the intent was created.
girsCreatedDate :: Lens' GetIntentResponse (Maybe UTCTime)
girsCreatedDate = lens _girsCreatedDate (\ s a -> s{_girsCreatedDate = a}) . mapping _Time;

-- | The name of the intent.
girsName :: Lens' GetIntentResponse (Maybe Text)
girsName = lens _girsName (\ s a -> s{_girsName = a});

-- | The version of the intent.
girsVersion :: Lens' GetIntentResponse (Maybe Text)
girsVersion = lens _girsVersion (\ s a -> s{_girsVersion = a});

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
girsFollowUpPrompt :: Lens' GetIntentResponse (Maybe FollowUpPrompt)
girsFollowUpPrompt = lens _girsFollowUpPrompt (\ s a -> s{_girsFollowUpPrompt = a});

-- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
girsLastUpdatedDate :: Lens' GetIntentResponse (Maybe UTCTime)
girsLastUpdatedDate = lens _girsLastUpdatedDate (\ s a -> s{_girsLastUpdatedDate = a}) . mapping _Time;

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
girsConfirmationPrompt :: Lens' GetIntentResponse (Maybe Prompt)
girsConfirmationPrompt = lens _girsConfirmationPrompt (\ s a -> s{_girsConfirmationPrompt = a});

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
girsDialogCodeHook :: Lens' GetIntentResponse (Maybe CodeHook)
girsDialogCodeHook = lens _girsDialogCodeHook (\ s a -> s{_girsDialogCodeHook = a});

-- | A description of the intent.
girsDescription :: Lens' GetIntentResponse (Maybe Text)
girsDescription = lens _girsDescription (\ s a -> s{_girsDescription = a});

-- | -- | The response status code.
girsResponseStatus :: Lens' GetIntentResponse Int
girsResponseStatus = lens _girsResponseStatus (\ s a -> s{_girsResponseStatus = a});

instance NFData GetIntentResponse where
