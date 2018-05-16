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
-- Module      : Network.AWS.LexModels.GetBot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata information for a specific bot. You must provide the bot name and the bot version or alias.
--
--
-- This operation requires permissions for the @lex:GetBot@ action.
--
module Network.AWS.LexModels.GetBot
    (
    -- * Creating a Request
      getBot
    , GetBot
    -- * Request Lenses
    , gbName
    , gbVersionOrAlias

    -- * Destructuring the Response
    , getBotResponse
    , GetBotResponse
    -- * Response Lenses
    , gbrsFailureReason
    , gbrsStatus
    , gbrsAbortStatement
    , gbrsIntents
    , gbrsChecksum
    , gbrsLocale
    , gbrsCreatedDate
    , gbrsName
    , gbrsVersion
    , gbrsIdleSessionTTLInSeconds
    , gbrsClarificationPrompt
    , gbrsVoiceId
    , gbrsLastUpdatedDate
    , gbrsChildDirected
    , gbrsDescription
    , gbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBot' smart constructor.
data GetBot = GetBot'
  { _gbName           :: !Text
  , _gbVersionOrAlias :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbName' - The name of the bot. The name is case sensitive.
--
-- * 'gbVersionOrAlias' - The version or alias of the bot.
getBot
    :: Text -- ^ 'gbName'
    -> Text -- ^ 'gbVersionOrAlias'
    -> GetBot
getBot pName_ pVersionOrAlias_ =
  GetBot' {_gbName = pName_, _gbVersionOrAlias = pVersionOrAlias_}


-- | The name of the bot. The name is case sensitive.
gbName :: Lens' GetBot Text
gbName = lens _gbName (\ s a -> s{_gbName = a})

-- | The version or alias of the bot.
gbVersionOrAlias :: Lens' GetBot Text
gbVersionOrAlias = lens _gbVersionOrAlias (\ s a -> s{_gbVersionOrAlias = a})

instance AWSRequest GetBot where
        type Rs GetBot = GetBotResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBotResponse' <$>
                   (x .?> "failureReason") <*> (x .?> "status") <*>
                     (x .?> "abortStatement")
                     <*> (x .?> "intents" .!@ mempty)
                     <*> (x .?> "checksum")
                     <*> (x .?> "locale")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (x .?> "idleSessionTTLInSeconds")
                     <*> (x .?> "clarificationPrompt")
                     <*> (x .?> "voiceId")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "childDirected")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable GetBot where

instance NFData GetBot where

instance ToHeaders GetBot where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBot where
        toPath GetBot'{..}
          = mconcat
              ["/bots/", toBS _gbName, "/versions/",
               toBS _gbVersionOrAlias]

instance ToQuery GetBot where
        toQuery = const mempty

-- | /See:/ 'getBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { _gbrsFailureReason           :: !(Maybe Text)
  , _gbrsStatus                  :: !(Maybe LexStatus)
  , _gbrsAbortStatement          :: !(Maybe Statement)
  , _gbrsIntents                 :: !(Maybe [Intent])
  , _gbrsChecksum                :: !(Maybe Text)
  , _gbrsLocale                  :: !(Maybe Locale)
  , _gbrsCreatedDate             :: !(Maybe POSIX)
  , _gbrsName                    :: !(Maybe Text)
  , _gbrsVersion                 :: !(Maybe Text)
  , _gbrsIdleSessionTTLInSeconds :: !(Maybe Nat)
  , _gbrsClarificationPrompt     :: !(Maybe Prompt)
  , _gbrsVoiceId                 :: !(Maybe Text)
  , _gbrsLastUpdatedDate         :: !(Maybe POSIX)
  , _gbrsChildDirected           :: !(Maybe Bool)
  , _gbrsDescription             :: !(Maybe Text)
  , _gbrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrsFailureReason' - If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
--
-- * 'gbrsStatus' - The status of the bot. If the bot is ready to run, the status is @READY@ . If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ explains why the bot did not build. If the bot was saved but not built, the status is @NOT BUILT@ .
--
-- * 'gbrsAbortStatement' - The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
--
-- * 'gbrsIntents' - An array of @intent@ objects. For more information, see 'PutBot' .
--
-- * 'gbrsChecksum' - Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
--
-- * 'gbrsLocale' - The target locale for the bot.
--
-- * 'gbrsCreatedDate' - The date that the bot was created.
--
-- * 'gbrsName' - The name of the bot.
--
-- * 'gbrsVersion' - The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- * 'gbrsIdleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- * 'gbrsClarificationPrompt' - The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
--
-- * 'gbrsVoiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
--
-- * 'gbrsLastUpdatedDate' - The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
--
-- * 'gbrsChildDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- * 'gbrsDescription' - A description of the bot.
--
-- * 'gbrsResponseStatus' - -- | The response status code.
getBotResponse
    :: Int -- ^ 'gbrsResponseStatus'
    -> GetBotResponse
getBotResponse pResponseStatus_ =
  GetBotResponse'
    { _gbrsFailureReason = Nothing
    , _gbrsStatus = Nothing
    , _gbrsAbortStatement = Nothing
    , _gbrsIntents = Nothing
    , _gbrsChecksum = Nothing
    , _gbrsLocale = Nothing
    , _gbrsCreatedDate = Nothing
    , _gbrsName = Nothing
    , _gbrsVersion = Nothing
    , _gbrsIdleSessionTTLInSeconds = Nothing
    , _gbrsClarificationPrompt = Nothing
    , _gbrsVoiceId = Nothing
    , _gbrsLastUpdatedDate = Nothing
    , _gbrsChildDirected = Nothing
    , _gbrsDescription = Nothing
    , _gbrsResponseStatus = pResponseStatus_
    }


-- | If @status@ is @FAILED@ , Amazon Lex explains why it failed to build the bot.
gbrsFailureReason :: Lens' GetBotResponse (Maybe Text)
gbrsFailureReason = lens _gbrsFailureReason (\ s a -> s{_gbrsFailureReason = a})

-- | The status of the bot. If the bot is ready to run, the status is @READY@ . If there was a problem with building the bot, the status is @FAILED@ and the @failureReason@ explains why the bot did not build. If the bot was saved but not built, the status is @NOT BUILT@ .
gbrsStatus :: Lens' GetBotResponse (Maybe LexStatus)
gbrsStatus = lens _gbrsStatus (\ s a -> s{_gbrsStatus = a})

-- | The message that Amazon Lex returns when the user elects to end the conversation without completing it. For more information, see 'PutBot' .
gbrsAbortStatement :: Lens' GetBotResponse (Maybe Statement)
gbrsAbortStatement = lens _gbrsAbortStatement (\ s a -> s{_gbrsAbortStatement = a})

-- | An array of @intent@ objects. For more information, see 'PutBot' .
gbrsIntents :: Lens' GetBotResponse [Intent]
gbrsIntents = lens _gbrsIntents (\ s a -> s{_gbrsIntents = a}) . _Default . _Coerce

-- | Checksum of the bot used to identify a specific revision of the bot's @> LATEST@ version.
gbrsChecksum :: Lens' GetBotResponse (Maybe Text)
gbrsChecksum = lens _gbrsChecksum (\ s a -> s{_gbrsChecksum = a})

-- | The target locale for the bot.
gbrsLocale :: Lens' GetBotResponse (Maybe Locale)
gbrsLocale = lens _gbrsLocale (\ s a -> s{_gbrsLocale = a})

-- | The date that the bot was created.
gbrsCreatedDate :: Lens' GetBotResponse (Maybe UTCTime)
gbrsCreatedDate = lens _gbrsCreatedDate (\ s a -> s{_gbrsCreatedDate = a}) . mapping _Time

-- | The name of the bot.
gbrsName :: Lens' GetBotResponse (Maybe Text)
gbrsName = lens _gbrsName (\ s a -> s{_gbrsName = a})

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
gbrsVersion :: Lens' GetBotResponse (Maybe Text)
gbrsVersion = lens _gbrsVersion (\ s a -> s{_gbrsVersion = a})

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
gbrsIdleSessionTTLInSeconds :: Lens' GetBotResponse (Maybe Natural)
gbrsIdleSessionTTLInSeconds = lens _gbrsIdleSessionTTLInSeconds (\ s a -> s{_gbrsIdleSessionTTLInSeconds = a}) . mapping _Nat

-- | The message Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
gbrsClarificationPrompt :: Lens' GetBotResponse (Maybe Prompt)
gbrsClarificationPrompt = lens _gbrsClarificationPrompt (\ s a -> s{_gbrsClarificationPrompt = a})

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction with the user. For more information, see 'PutBot' .
gbrsVoiceId :: Lens' GetBotResponse (Maybe Text)
gbrsVoiceId = lens _gbrsVoiceId (\ s a -> s{_gbrsVoiceId = a})

-- | The date that the bot was updated. When you create a resource, the creation date and last updated date are the same.
gbrsLastUpdatedDate :: Lens' GetBotResponse (Maybe UTCTime)
gbrsLastUpdatedDate = lens _gbrsLastUpdatedDate (\ s a -> s{_gbrsLastUpdatedDate = a}) . mapping _Time

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
gbrsChildDirected :: Lens' GetBotResponse (Maybe Bool)
gbrsChildDirected = lens _gbrsChildDirected (\ s a -> s{_gbrsChildDirected = a})

-- | A description of the bot.
gbrsDescription :: Lens' GetBotResponse (Maybe Text)
gbrsDescription = lens _gbrsDescription (\ s a -> s{_gbrsDescription = a})

-- | -- | The response status code.
gbrsResponseStatus :: Lens' GetBotResponse Int
gbrsResponseStatus = lens _gbrsResponseStatus (\ s a -> s{_gbrsResponseStatus = a})

instance NFData GetBotResponse where
