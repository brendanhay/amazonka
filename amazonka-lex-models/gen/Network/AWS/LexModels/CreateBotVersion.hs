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
-- Module      : Network.AWS.LexModels.CreateBotVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the bot based on the @> LATEST@ version. If the @> LATEST@ version of this resource hasn't changed since you created the last version, Amazon Lex doesn't create a new version. It returns the last created version.
--
--
-- When you create the first version of a bot, Amazon Lex sets the version to 1. Subsequent versions increment by 1. For more information, see 'versioning-intro' .
--
-- This operation requires permission for the @lex:CreateBotVersion@ action.
--
module Network.AWS.LexModels.CreateBotVersion
    (
    -- * Creating a Request
      createBotVersion
    , CreateBotVersion
    -- * Request Lenses
    , cbvChecksum
    , cbvName

    -- * Destructuring the Response
    , createBotVersionResponse
    , CreateBotVersionResponse
    -- * Response Lenses
    , cbvrsFailureReason
    , cbvrsStatus
    , cbvrsAbortStatement
    , cbvrsIntents
    , cbvrsChecksum
    , cbvrsLocale
    , cbvrsCreatedDate
    , cbvrsName
    , cbvrsVersion
    , cbvrsIdleSessionTTLInSeconds
    , cbvrsClarificationPrompt
    , cbvrsVoiceId
    , cbvrsLastUpdatedDate
    , cbvrsChildDirected
    , cbvrsDescription
    , cbvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBotVersion' smart constructor.
data CreateBotVersion = CreateBotVersion'
  { _cbvChecksum :: !(Maybe Text)
  , _cbvName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBotVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbvChecksum' - Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
--
-- * 'cbvName' - The name of the bot that you want to create a new version of. The name is case sensitive.
createBotVersion
    :: Text -- ^ 'cbvName'
    -> CreateBotVersion
createBotVersion pName_ =
  CreateBotVersion' {_cbvChecksum = Nothing, _cbvName = pName_}


-- | Identifies a specific revision of the @> LATEST@ version of the bot. If you specify a checksum and the @> LATEST@ version of the bot has a different checksum, a @PreconditionFailedException@ exception is returned and Amazon Lex doesn't publish a new version. If you don't specify a checksum, Amazon Lex publishes the @> LATEST@ version.
cbvChecksum :: Lens' CreateBotVersion (Maybe Text)
cbvChecksum = lens _cbvChecksum (\ s a -> s{_cbvChecksum = a})

-- | The name of the bot that you want to create a new version of. The name is case sensitive.
cbvName :: Lens' CreateBotVersion Text
cbvName = lens _cbvName (\ s a -> s{_cbvName = a})

instance AWSRequest CreateBotVersion where
        type Rs CreateBotVersion = CreateBotVersionResponse
        request = postJSON lexModels
        response
          = receiveJSON
              (\ s h x ->
                 CreateBotVersionResponse' <$>
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

instance Hashable CreateBotVersion where

instance NFData CreateBotVersion where

instance ToHeaders CreateBotVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBotVersion where
        toJSON CreateBotVersion'{..}
          = object
              (catMaybes [("checksum" .=) <$> _cbvChecksum])

instance ToPath CreateBotVersion where
        toPath CreateBotVersion'{..}
          = mconcat ["/bots/", toBS _cbvName, "/versions"]

instance ToQuery CreateBotVersion where
        toQuery = const mempty

-- | /See:/ 'createBotVersionResponse' smart constructor.
data CreateBotVersionResponse = CreateBotVersionResponse'
  { _cbvrsFailureReason           :: !(Maybe Text)
  , _cbvrsStatus                  :: !(Maybe LexStatus)
  , _cbvrsAbortStatement          :: !(Maybe Statement)
  , _cbvrsIntents                 :: !(Maybe [Intent])
  , _cbvrsChecksum                :: !(Maybe Text)
  , _cbvrsLocale                  :: !(Maybe Locale)
  , _cbvrsCreatedDate             :: !(Maybe POSIX)
  , _cbvrsName                    :: !(Maybe Text)
  , _cbvrsVersion                 :: !(Maybe Text)
  , _cbvrsIdleSessionTTLInSeconds :: !(Maybe Nat)
  , _cbvrsClarificationPrompt     :: !(Maybe Prompt)
  , _cbvrsVoiceId                 :: !(Maybe Text)
  , _cbvrsLastUpdatedDate         :: !(Maybe POSIX)
  , _cbvrsChildDirected           :: !(Maybe Bool)
  , _cbvrsDescription             :: !(Maybe Text)
  , _cbvrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBotVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbvrsFailureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
--
-- * 'cbvrsStatus' - When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
--
-- * 'cbvrsAbortStatement' - The message that Amazon Lex uses to abort a conversation. For more information, see 'PutBot' .
--
-- * 'cbvrsIntents' - An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- * 'cbvrsChecksum' - Checksum identifying the version of the bot that was created.
--
-- * 'cbvrsLocale' - Specifies the target locale for the bot.
--
-- * 'cbvrsCreatedDate' - The date when the bot version was created.
--
-- * 'cbvrsName' - The name of the bot.
--
-- * 'cbvrsVersion' - The version of the bot.
--
-- * 'cbvrsIdleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
--
-- * 'cbvrsClarificationPrompt' - The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
--
-- * 'cbvrsVoiceId' - The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
--
-- * 'cbvrsLastUpdatedDate' - The date when the @> LATEST@ version of this bot was updated.
--
-- * 'cbvrsChildDirected' - For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
--
-- * 'cbvrsDescription' - A description of the bot.
--
-- * 'cbvrsResponseStatus' - -- | The response status code.
createBotVersionResponse
    :: Int -- ^ 'cbvrsResponseStatus'
    -> CreateBotVersionResponse
createBotVersionResponse pResponseStatus_ =
  CreateBotVersionResponse'
    { _cbvrsFailureReason = Nothing
    , _cbvrsStatus = Nothing
    , _cbvrsAbortStatement = Nothing
    , _cbvrsIntents = Nothing
    , _cbvrsChecksum = Nothing
    , _cbvrsLocale = Nothing
    , _cbvrsCreatedDate = Nothing
    , _cbvrsName = Nothing
    , _cbvrsVersion = Nothing
    , _cbvrsIdleSessionTTLInSeconds = Nothing
    , _cbvrsClarificationPrompt = Nothing
    , _cbvrsVoiceId = Nothing
    , _cbvrsLastUpdatedDate = Nothing
    , _cbvrsChildDirected = Nothing
    , _cbvrsDescription = Nothing
    , _cbvrsResponseStatus = pResponseStatus_
    }


-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to build the bot.
cbvrsFailureReason :: Lens' CreateBotVersionResponse (Maybe Text)
cbvrsFailureReason = lens _cbvrsFailureReason (\ s a -> s{_cbvrsFailureReason = a})

-- | When you send a request to create or update a bot, Amazon Lex sets the @status@ response element to @BUILDING@ . After Amazon Lex builds the bot, it sets @status@ to @READY@ . If Amazon Lex can't build the bot, it sets @status@ to @FAILED@ . Amazon Lex returns the reason for the failure in the @failureReason@ response element.
cbvrsStatus :: Lens' CreateBotVersionResponse (Maybe LexStatus)
cbvrsStatus = lens _cbvrsStatus (\ s a -> s{_cbvrsStatus = a})

-- | The message that Amazon Lex uses to abort a conversation. For more information, see 'PutBot' .
cbvrsAbortStatement :: Lens' CreateBotVersionResponse (Maybe Statement)
cbvrsAbortStatement = lens _cbvrsAbortStatement (\ s a -> s{_cbvrsAbortStatement = a})

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
cbvrsIntents :: Lens' CreateBotVersionResponse [Intent]
cbvrsIntents = lens _cbvrsIntents (\ s a -> s{_cbvrsIntents = a}) . _Default . _Coerce

-- | Checksum identifying the version of the bot that was created.
cbvrsChecksum :: Lens' CreateBotVersionResponse (Maybe Text)
cbvrsChecksum = lens _cbvrsChecksum (\ s a -> s{_cbvrsChecksum = a})

-- | Specifies the target locale for the bot.
cbvrsLocale :: Lens' CreateBotVersionResponse (Maybe Locale)
cbvrsLocale = lens _cbvrsLocale (\ s a -> s{_cbvrsLocale = a})

-- | The date when the bot version was created.
cbvrsCreatedDate :: Lens' CreateBotVersionResponse (Maybe UTCTime)
cbvrsCreatedDate = lens _cbvrsCreatedDate (\ s a -> s{_cbvrsCreatedDate = a}) . mapping _Time

-- | The name of the bot.
cbvrsName :: Lens' CreateBotVersionResponse (Maybe Text)
cbvrsName = lens _cbvrsName (\ s a -> s{_cbvrsName = a})

-- | The version of the bot.
cbvrsVersion :: Lens' CreateBotVersionResponse (Maybe Text)
cbvrsVersion = lens _cbvrsVersion (\ s a -> s{_cbvrsVersion = a})

-- | The maximum time in seconds that Amazon Lex retains the data gathered in a conversation. For more information, see 'PutBot' .
cbvrsIdleSessionTTLInSeconds :: Lens' CreateBotVersionResponse (Maybe Natural)
cbvrsIdleSessionTTLInSeconds = lens _cbvrsIdleSessionTTLInSeconds (\ s a -> s{_cbvrsIdleSessionTTLInSeconds = a}) . mapping _Nat

-- | The message that Amazon Lex uses when it doesn't understand the user's request. For more information, see 'PutBot' .
cbvrsClarificationPrompt :: Lens' CreateBotVersionResponse (Maybe Prompt)
cbvrsClarificationPrompt = lens _cbvrsClarificationPrompt (\ s a -> s{_cbvrsClarificationPrompt = a})

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interactions with the user.
cbvrsVoiceId :: Lens' CreateBotVersionResponse (Maybe Text)
cbvrsVoiceId = lens _cbvrsVoiceId (\ s a -> s{_cbvrsVoiceId = a})

-- | The date when the @> LATEST@ version of this bot was updated.
cbvrsLastUpdatedDate :: Lens' CreateBotVersionResponse (Maybe UTCTime)
cbvrsLastUpdatedDate = lens _cbvrsLastUpdatedDate (\ s a -> s{_cbvrsLastUpdatedDate = a}) . mapping _Time

-- | For each Amazon Lex bot created with the Amazon Lex Model Building Service, you must specify whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to the Children's Online Privacy Protection Act (COPPA) by specifying @true@ or @false@ in the @childDirected@ field. By specifying @true@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. By specifying @false@ in the @childDirected@ field, you confirm that your use of Amazon Lex __is not__ related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. You may not specify a default value for the @childDirected@ field that does not accurately reflect whether your use of Amazon Lex is related to a website, program, or other application that is directed or targeted, in whole or in part, to children under age 13 and subject to COPPA. If your use of Amazon Lex relates to a website, program, or other application that is directed in whole or in part, to children under age 13, you must obtain any required verifiable parental consent under COPPA. For information regarding the use of Amazon Lex in connection with websites, programs, or other applications that are directed or targeted, in whole or in part, to children under age 13, see the <https://aws.amazon.com/lex/faqs#data-security Amazon Lex FAQ.>
cbvrsChildDirected :: Lens' CreateBotVersionResponse (Maybe Bool)
cbvrsChildDirected = lens _cbvrsChildDirected (\ s a -> s{_cbvrsChildDirected = a})

-- | A description of the bot.
cbvrsDescription :: Lens' CreateBotVersionResponse (Maybe Text)
cbvrsDescription = lens _cbvrsDescription (\ s a -> s{_cbvrsDescription = a})

-- | -- | The response status code.
cbvrsResponseStatus :: Lens' CreateBotVersionResponse Int
cbvrsResponseStatus = lens _cbvrsResponseStatus (\ s a -> s{_cbvrsResponseStatus = a})

instance NFData CreateBotVersionResponse where
