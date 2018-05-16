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
-- Copyright   : (c) 2013-2018 Brendan Hay
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
    , getrsFulfillmentActivity
    , getrsSlots
    , getrsRejectionStatement
    , getrsChecksum
    , getrsConclusionStatement
    , getrsSampleUtterances
    , getrsParentIntentSignature
    , getrsCreatedDate
    , getrsName
    , getrsVersion
    , getrsFollowUpPrompt
    , getrsLastUpdatedDate
    , getrsConfirmationPrompt
    , getrsDialogCodeHook
    , getrsDescription
    , getrsResponseStatus
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
giName = lens _giName (\ s a -> s{_giName = a})

-- | The version of the intent.
giVersion :: Lens' GetIntent Text
giVersion = lens _giVersion (\ s a -> s{_giVersion = a})

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
  { _getrsFulfillmentActivity   :: !(Maybe FulfillmentActivity)
  , _getrsSlots                 :: !(Maybe [Slot])
  , _getrsRejectionStatement    :: !(Maybe Statement)
  , _getrsChecksum              :: !(Maybe Text)
  , _getrsConclusionStatement   :: !(Maybe Statement)
  , _getrsSampleUtterances      :: !(Maybe [Text])
  , _getrsParentIntentSignature :: !(Maybe Text)
  , _getrsCreatedDate           :: !(Maybe POSIX)
  , _getrsName                  :: !(Maybe Text)
  , _getrsVersion               :: !(Maybe Text)
  , _getrsFollowUpPrompt        :: !(Maybe FollowUpPrompt)
  , _getrsLastUpdatedDate       :: !(Maybe POSIX)
  , _getrsConfirmationPrompt    :: !(Maybe Prompt)
  , _getrsDialogCodeHook        :: !(Maybe CodeHook)
  , _getrsDescription           :: !(Maybe Text)
  , _getrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetIntentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsFulfillmentActivity' - Describes how the intent is fulfilled. For more information, see 'PutIntent' .
--
-- * 'getrsSlots' - An array of intent slots configured for the intent.
--
-- * 'getrsRejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- * 'getrsChecksum' - Checksum of the intent.
--
-- * 'getrsConclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- * 'getrsSampleUtterances' - An array of sample utterances configured for the intent.
--
-- * 'getrsParentIntentSignature' - A unique identifier for a built-in intent.
--
-- * 'getrsCreatedDate' - The date that the intent was created.
--
-- * 'getrsName' - The name of the intent.
--
-- * 'getrsVersion' - The version of the intent.
--
-- * 'getrsFollowUpPrompt' - If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
--
-- * 'getrsLastUpdatedDate' - The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- * 'getrsConfirmationPrompt' - If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
--
-- * 'getrsDialogCodeHook' - If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
--
-- * 'getrsDescription' - A description of the intent.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getIntentResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> GetIntentResponse
getIntentResponse pResponseStatus_ =
  GetIntentResponse'
    { _getrsFulfillmentActivity = Nothing
    , _getrsSlots = Nothing
    , _getrsRejectionStatement = Nothing
    , _getrsChecksum = Nothing
    , _getrsConclusionStatement = Nothing
    , _getrsSampleUtterances = Nothing
    , _getrsParentIntentSignature = Nothing
    , _getrsCreatedDate = Nothing
    , _getrsName = Nothing
    , _getrsVersion = Nothing
    , _getrsFollowUpPrompt = Nothing
    , _getrsLastUpdatedDate = Nothing
    , _getrsConfirmationPrompt = Nothing
    , _getrsDialogCodeHook = Nothing
    , _getrsDescription = Nothing
    , _getrsResponseStatus = pResponseStatus_
    }


-- | Describes how the intent is fulfilled. For more information, see 'PutIntent' .
getrsFulfillmentActivity :: Lens' GetIntentResponse (Maybe FulfillmentActivity)
getrsFulfillmentActivity = lens _getrsFulfillmentActivity (\ s a -> s{_getrsFulfillmentActivity = a})

-- | An array of intent slots configured for the intent.
getrsSlots :: Lens' GetIntentResponse [Slot]
getrsSlots = lens _getrsSlots (\ s a -> s{_getrsSlots = a}) . _Default . _Coerce

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
getrsRejectionStatement :: Lens' GetIntentResponse (Maybe Statement)
getrsRejectionStatement = lens _getrsRejectionStatement (\ s a -> s{_getrsRejectionStatement = a})

-- | Checksum of the intent.
getrsChecksum :: Lens' GetIntentResponse (Maybe Text)
getrsChecksum = lens _getrsChecksum (\ s a -> s{_getrsChecksum = a})

-- | After the Lambda function specified in the @fulfillmentActivity@ element fulfills the intent, Amazon Lex conveys this statement to the user.
getrsConclusionStatement :: Lens' GetIntentResponse (Maybe Statement)
getrsConclusionStatement = lens _getrsConclusionStatement (\ s a -> s{_getrsConclusionStatement = a})

-- | An array of sample utterances configured for the intent.
getrsSampleUtterances :: Lens' GetIntentResponse [Text]
getrsSampleUtterances = lens _getrsSampleUtterances (\ s a -> s{_getrsSampleUtterances = a}) . _Default . _Coerce

-- | A unique identifier for a built-in intent.
getrsParentIntentSignature :: Lens' GetIntentResponse (Maybe Text)
getrsParentIntentSignature = lens _getrsParentIntentSignature (\ s a -> s{_getrsParentIntentSignature = a})

-- | The date that the intent was created.
getrsCreatedDate :: Lens' GetIntentResponse (Maybe UTCTime)
getrsCreatedDate = lens _getrsCreatedDate (\ s a -> s{_getrsCreatedDate = a}) . mapping _Time

-- | The name of the intent.
getrsName :: Lens' GetIntentResponse (Maybe Text)
getrsName = lens _getrsName (\ s a -> s{_getrsName = a})

-- | The version of the intent.
getrsVersion :: Lens' GetIntentResponse (Maybe Text)
getrsVersion = lens _getrsVersion (\ s a -> s{_getrsVersion = a})

-- | If defined in the bot, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled. For more information, see 'PutIntent' .
getrsFollowUpPrompt :: Lens' GetIntentResponse (Maybe FollowUpPrompt)
getrsFollowUpPrompt = lens _getrsFollowUpPrompt (\ s a -> s{_getrsFollowUpPrompt = a})

-- | The date that the intent was updated. When you create a resource, the creation date and the last updated date are the same.
getrsLastUpdatedDate :: Lens' GetIntentResponse (Maybe UTCTime)
getrsLastUpdatedDate = lens _getrsLastUpdatedDate (\ s a -> s{_getrsLastUpdatedDate = a}) . mapping _Time

-- | If defined in the bot, Amazon Lex uses prompt to confirm the intent before fulfilling the user's request. For more information, see 'PutIntent' .
getrsConfirmationPrompt :: Lens' GetIntentResponse (Maybe Prompt)
getrsConfirmationPrompt = lens _getrsConfirmationPrompt (\ s a -> s{_getrsConfirmationPrompt = a})

-- | If defined in the bot, Amazon Amazon Lex invokes this Lambda function for each user input. For more information, see 'PutIntent' .
getrsDialogCodeHook :: Lens' GetIntentResponse (Maybe CodeHook)
getrsDialogCodeHook = lens _getrsDialogCodeHook (\ s a -> s{_getrsDialogCodeHook = a})

-- | A description of the intent.
getrsDescription :: Lens' GetIntentResponse (Maybe Text)
getrsDescription = lens _getrsDescription (\ s a -> s{_getrsDescription = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetIntentResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a})

instance NFData GetIntentResponse where
