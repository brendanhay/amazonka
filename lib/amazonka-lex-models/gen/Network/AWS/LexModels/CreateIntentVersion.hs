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
-- Module      : Network.AWS.LexModels.CreateIntentVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of an intent based on the @> LATEST@ version of the intent. If the @> LATEST@ version of this intent hasn't changed since you last updated it, Amazon Lex doesn't create a new version. It returns the last version you created.
--
--
-- When you create a version of an intent, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' .
--
-- This operation requires permissions to perform the @lex:CreateIntentVersion@ action.
--
module Network.AWS.LexModels.CreateIntentVersion
    (
    -- * Creating a Request
      createIntentVersion
    , CreateIntentVersion
    -- * Request Lenses
    , civChecksum
    , civName

    -- * Destructuring the Response
    , createIntentVersionResponse
    , CreateIntentVersionResponse
    -- * Response Lenses
    , civrsFulfillmentActivity
    , civrsSlots
    , civrsRejectionStatement
    , civrsChecksum
    , civrsConclusionStatement
    , civrsSampleUtterances
    , civrsParentIntentSignature
    , civrsCreatedDate
    , civrsName
    , civrsVersion
    , civrsFollowUpPrompt
    , civrsLastUpdatedDate
    , civrsConfirmationPrompt
    , civrsDialogCodeHook
    , civrsDescription
    , civrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createIntentVersion' smart constructor.
data CreateIntentVersion = CreateIntentVersion'
  { _civChecksum :: !(Maybe Text)
  , _civName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIntentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'civChecksum' - Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- * 'civName' - The name of the intent that you want to create a new version of. The name is case sensitive.
createIntentVersion
    :: Text -- ^ 'civName'
    -> CreateIntentVersion
createIntentVersion pName_ =
  CreateIntentVersion' {_civChecksum = Nothing, _civName = pName_}


-- | Checksum of the @> LATEST@ version of the intent that should be used to create the new version. If you specify a checksum and the @> LATEST@ version of the intent has a different checksum, Amazon Lex returns a @PreconditionFailedException@ exception and doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
civChecksum :: Lens' CreateIntentVersion (Maybe Text)
civChecksum = lens _civChecksum (\ s a -> s{_civChecksum = a})

-- | The name of the intent that you want to create a new version of. The name is case sensitive.
civName :: Lens' CreateIntentVersion Text
civName = lens _civName (\ s a -> s{_civName = a})

instance AWSRequest CreateIntentVersion where
        type Rs CreateIntentVersion =
             CreateIntentVersionResponse
        request = postJSON lexModels
        response
          = receiveJSON
              (\ s h x ->
                 CreateIntentVersionResponse' <$>
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

instance Hashable CreateIntentVersion where

instance NFData CreateIntentVersion where

instance ToHeaders CreateIntentVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateIntentVersion where
        toJSON CreateIntentVersion'{..}
          = object
              (catMaybes [("checksum" .=) <$> _civChecksum])

instance ToPath CreateIntentVersion where
        toPath CreateIntentVersion'{..}
          = mconcat ["/intents/", toBS _civName, "/versions"]

instance ToQuery CreateIntentVersion where
        toQuery = const mempty

-- | /See:/ 'createIntentVersionResponse' smart constructor.
data CreateIntentVersionResponse = CreateIntentVersionResponse'
  { _civrsFulfillmentActivity   :: !(Maybe FulfillmentActivity)
  , _civrsSlots                 :: !(Maybe [Slot])
  , _civrsRejectionStatement    :: !(Maybe Statement)
  , _civrsChecksum              :: !(Maybe Text)
  , _civrsConclusionStatement   :: !(Maybe Statement)
  , _civrsSampleUtterances      :: !(Maybe [Text])
  , _civrsParentIntentSignature :: !(Maybe Text)
  , _civrsCreatedDate           :: !(Maybe POSIX)
  , _civrsName                  :: !(Maybe Text)
  , _civrsVersion               :: !(Maybe Text)
  , _civrsFollowUpPrompt        :: !(Maybe FollowUpPrompt)
  , _civrsLastUpdatedDate       :: !(Maybe POSIX)
  , _civrsConfirmationPrompt    :: !(Maybe Prompt)
  , _civrsDialogCodeHook        :: !(Maybe CodeHook)
  , _civrsDescription           :: !(Maybe Text)
  , _civrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIntentVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'civrsFulfillmentActivity' - Describes how the intent is fulfilled.
--
-- * 'civrsSlots' - An array of slot types that defines the information required to fulfill the intent.
--
-- * 'civrsRejectionStatement' - If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- * 'civrsChecksum' - Checksum of the intent version created.
--
-- * 'civrsConclusionStatement' - After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
--
-- * 'civrsSampleUtterances' - An array of sample utterances configured for the intent.
--
-- * 'civrsParentIntentSignature' - A unique identifier for a built-in intent.
--
-- * 'civrsCreatedDate' - The date that the intent was created.
--
-- * 'civrsName' - The name of the intent.
--
-- * 'civrsVersion' - The version number assigned to the new version of the intent.
--
-- * 'civrsFollowUpPrompt' - If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
--
-- * 'civrsLastUpdatedDate' - The date that the intent was updated.
--
-- * 'civrsConfirmationPrompt' - If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
--
-- * 'civrsDialogCodeHook' - If defined, Amazon Lex invokes this Lambda function for each user input.
--
-- * 'civrsDescription' - A description of the intent.
--
-- * 'civrsResponseStatus' - -- | The response status code.
createIntentVersionResponse
    :: Int -- ^ 'civrsResponseStatus'
    -> CreateIntentVersionResponse
createIntentVersionResponse pResponseStatus_ =
  CreateIntentVersionResponse'
    { _civrsFulfillmentActivity = Nothing
    , _civrsSlots = Nothing
    , _civrsRejectionStatement = Nothing
    , _civrsChecksum = Nothing
    , _civrsConclusionStatement = Nothing
    , _civrsSampleUtterances = Nothing
    , _civrsParentIntentSignature = Nothing
    , _civrsCreatedDate = Nothing
    , _civrsName = Nothing
    , _civrsVersion = Nothing
    , _civrsFollowUpPrompt = Nothing
    , _civrsLastUpdatedDate = Nothing
    , _civrsConfirmationPrompt = Nothing
    , _civrsDialogCodeHook = Nothing
    , _civrsDescription = Nothing
    , _civrsResponseStatus = pResponseStatus_
    }


-- | Describes how the intent is fulfilled.
civrsFulfillmentActivity :: Lens' CreateIntentVersionResponse (Maybe FulfillmentActivity)
civrsFulfillmentActivity = lens _civrsFulfillmentActivity (\ s a -> s{_civrsFulfillmentActivity = a})

-- | An array of slot types that defines the information required to fulfill the intent.
civrsSlots :: Lens' CreateIntentVersionResponse [Slot]
civrsSlots = lens _civrsSlots (\ s a -> s{_civrsSlots = a}) . _Default . _Coerce

-- | If the user answers "no" to the question defined in @confirmationPrompt@ , Amazon Lex responds with this statement to acknowledge that the intent was canceled.
civrsRejectionStatement :: Lens' CreateIntentVersionResponse (Maybe Statement)
civrsRejectionStatement = lens _civrsRejectionStatement (\ s a -> s{_civrsRejectionStatement = a})

-- | Checksum of the intent version created.
civrsChecksum :: Lens' CreateIntentVersionResponse (Maybe Text)
civrsChecksum = lens _civrsChecksum (\ s a -> s{_civrsChecksum = a})

-- | After the Lambda function specified in the @fulfillmentActivity@ field fulfills the intent, Amazon Lex conveys this statement to the user.
civrsConclusionStatement :: Lens' CreateIntentVersionResponse (Maybe Statement)
civrsConclusionStatement = lens _civrsConclusionStatement (\ s a -> s{_civrsConclusionStatement = a})

-- | An array of sample utterances configured for the intent.
civrsSampleUtterances :: Lens' CreateIntentVersionResponse [Text]
civrsSampleUtterances = lens _civrsSampleUtterances (\ s a -> s{_civrsSampleUtterances = a}) . _Default . _Coerce

-- | A unique identifier for a built-in intent.
civrsParentIntentSignature :: Lens' CreateIntentVersionResponse (Maybe Text)
civrsParentIntentSignature = lens _civrsParentIntentSignature (\ s a -> s{_civrsParentIntentSignature = a})

-- | The date that the intent was created.
civrsCreatedDate :: Lens' CreateIntentVersionResponse (Maybe UTCTime)
civrsCreatedDate = lens _civrsCreatedDate (\ s a -> s{_civrsCreatedDate = a}) . mapping _Time

-- | The name of the intent.
civrsName :: Lens' CreateIntentVersionResponse (Maybe Text)
civrsName = lens _civrsName (\ s a -> s{_civrsName = a})

-- | The version number assigned to the new version of the intent.
civrsVersion :: Lens' CreateIntentVersionResponse (Maybe Text)
civrsVersion = lens _civrsVersion (\ s a -> s{_civrsVersion = a})

-- | If defined, Amazon Lex uses this prompt to solicit additional user activity after the intent is fulfilled.
civrsFollowUpPrompt :: Lens' CreateIntentVersionResponse (Maybe FollowUpPrompt)
civrsFollowUpPrompt = lens _civrsFollowUpPrompt (\ s a -> s{_civrsFollowUpPrompt = a})

-- | The date that the intent was updated.
civrsLastUpdatedDate :: Lens' CreateIntentVersionResponse (Maybe UTCTime)
civrsLastUpdatedDate = lens _civrsLastUpdatedDate (\ s a -> s{_civrsLastUpdatedDate = a}) . mapping _Time

-- | If defined, the prompt that Amazon Lex uses to confirm the user's intent before fulfilling it.
civrsConfirmationPrompt :: Lens' CreateIntentVersionResponse (Maybe Prompt)
civrsConfirmationPrompt = lens _civrsConfirmationPrompt (\ s a -> s{_civrsConfirmationPrompt = a})

-- | If defined, Amazon Lex invokes this Lambda function for each user input.
civrsDialogCodeHook :: Lens' CreateIntentVersionResponse (Maybe CodeHook)
civrsDialogCodeHook = lens _civrsDialogCodeHook (\ s a -> s{_civrsDialogCodeHook = a})

-- | A description of the intent.
civrsDescription :: Lens' CreateIntentVersionResponse (Maybe Text)
civrsDescription = lens _civrsDescription (\ s a -> s{_civrsDescription = a})

-- | -- | The response status code.
civrsResponseStatus :: Lens' CreateIntentVersionResponse Int
civrsResponseStatus = lens _civrsResponseStatus (\ s a -> s{_civrsResponseStatus = a})

instance NFData CreateIntentVersionResponse where
