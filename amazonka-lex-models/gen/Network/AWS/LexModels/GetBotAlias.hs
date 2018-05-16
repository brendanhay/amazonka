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
-- Module      : Network.AWS.LexModels.GetBotAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon Lex bot alias. For more information about aliases, see 'versioning-aliases' .
--
--
-- This operation requires permissions for the @lex:GetBotAlias@ action.
--
module Network.AWS.LexModels.GetBotAlias
    (
    -- * Creating a Request
      getBotAlias
    , GetBotAlias
    -- * Request Lenses
    , gbasName
    , gbasBotName

    -- * Destructuring the Response
    , getBotAliasResponse
    , GetBotAliasResponse
    -- * Response Lenses
    , gbasrsChecksum
    , gbasrsBotVersion
    , gbasrsBotName
    , gbasrsCreatedDate
    , gbasrsName
    , gbasrsLastUpdatedDate
    , gbasrsDescription
    , gbasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBotAlias' smart constructor.
data GetBotAlias = GetBotAlias'
  { _gbasName    :: !Text
  , _gbasBotName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbasName' - The name of the bot alias. The name is case sensitive.
--
-- * 'gbasBotName' - The name of the bot.
getBotAlias
    :: Text -- ^ 'gbasName'
    -> Text -- ^ 'gbasBotName'
    -> GetBotAlias
getBotAlias pName_ pBotName_ =
  GetBotAlias' {_gbasName = pName_, _gbasBotName = pBotName_}


-- | The name of the bot alias. The name is case sensitive.
gbasName :: Lens' GetBotAlias Text
gbasName = lens _gbasName (\ s a -> s{_gbasName = a})

-- | The name of the bot.
gbasBotName :: Lens' GetBotAlias Text
gbasBotName = lens _gbasBotName (\ s a -> s{_gbasBotName = a})

instance AWSRequest GetBotAlias where
        type Rs GetBotAlias = GetBotAliasResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetBotAliasResponse' <$>
                   (x .?> "checksum") <*> (x .?> "botVersion") <*>
                     (x .?> "botName")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "lastUpdatedDate")
                     <*> (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable GetBotAlias where

instance NFData GetBotAlias where

instance ToHeaders GetBotAlias where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetBotAlias where
        toPath GetBotAlias'{..}
          = mconcat
              ["/bots/", toBS _gbasBotName, "/aliases/",
               toBS _gbasName]

instance ToQuery GetBotAlias where
        toQuery = const mempty

-- | /See:/ 'getBotAliasResponse' smart constructor.
data GetBotAliasResponse = GetBotAliasResponse'
  { _gbasrsChecksum        :: !(Maybe Text)
  , _gbasrsBotVersion      :: !(Maybe Text)
  , _gbasrsBotName         :: !(Maybe Text)
  , _gbasrsCreatedDate     :: !(Maybe POSIX)
  , _gbasrsName            :: !(Maybe Text)
  , _gbasrsLastUpdatedDate :: !(Maybe POSIX)
  , _gbasrsDescription     :: !(Maybe Text)
  , _gbasrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBotAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbasrsChecksum' - Checksum of the bot alias.
--
-- * 'gbasrsBotVersion' - The version of the bot that the alias points to.
--
-- * 'gbasrsBotName' - The name of the bot that the alias points to.
--
-- * 'gbasrsCreatedDate' - The date that the bot alias was created.
--
-- * 'gbasrsName' - The name of the bot alias.
--
-- * 'gbasrsLastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- * 'gbasrsDescription' - A description of the bot alias.
--
-- * 'gbasrsResponseStatus' - -- | The response status code.
getBotAliasResponse
    :: Int -- ^ 'gbasrsResponseStatus'
    -> GetBotAliasResponse
getBotAliasResponse pResponseStatus_ =
  GetBotAliasResponse'
    { _gbasrsChecksum = Nothing
    , _gbasrsBotVersion = Nothing
    , _gbasrsBotName = Nothing
    , _gbasrsCreatedDate = Nothing
    , _gbasrsName = Nothing
    , _gbasrsLastUpdatedDate = Nothing
    , _gbasrsDescription = Nothing
    , _gbasrsResponseStatus = pResponseStatus_
    }


-- | Checksum of the bot alias.
gbasrsChecksum :: Lens' GetBotAliasResponse (Maybe Text)
gbasrsChecksum = lens _gbasrsChecksum (\ s a -> s{_gbasrsChecksum = a})

-- | The version of the bot that the alias points to.
gbasrsBotVersion :: Lens' GetBotAliasResponse (Maybe Text)
gbasrsBotVersion = lens _gbasrsBotVersion (\ s a -> s{_gbasrsBotVersion = a})

-- | The name of the bot that the alias points to.
gbasrsBotName :: Lens' GetBotAliasResponse (Maybe Text)
gbasrsBotName = lens _gbasrsBotName (\ s a -> s{_gbasrsBotName = a})

-- | The date that the bot alias was created.
gbasrsCreatedDate :: Lens' GetBotAliasResponse (Maybe UTCTime)
gbasrsCreatedDate = lens _gbasrsCreatedDate (\ s a -> s{_gbasrsCreatedDate = a}) . mapping _Time

-- | The name of the bot alias.
gbasrsName :: Lens' GetBotAliasResponse (Maybe Text)
gbasrsName = lens _gbasrsName (\ s a -> s{_gbasrsName = a})

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
gbasrsLastUpdatedDate :: Lens' GetBotAliasResponse (Maybe UTCTime)
gbasrsLastUpdatedDate = lens _gbasrsLastUpdatedDate (\ s a -> s{_gbasrsLastUpdatedDate = a}) . mapping _Time

-- | A description of the bot alias.
gbasrsDescription :: Lens' GetBotAliasResponse (Maybe Text)
gbasrsDescription = lens _gbasrsDescription (\ s a -> s{_gbasrsDescription = a})

-- | -- | The response status code.
gbasrsResponseStatus :: Lens' GetBotAliasResponse Int
gbasrsResponseStatus = lens _gbasrsResponseStatus (\ s a -> s{_gbasrsResponseStatus = a})

instance NFData GetBotAliasResponse where
