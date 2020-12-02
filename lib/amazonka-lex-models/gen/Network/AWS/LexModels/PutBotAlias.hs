{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for the specified version of the bot or replaces an alias for the specified bot. To change the version of the bot that the alias points to, replace the alias. For more information about aliases, see 'versioning-aliases' .
--
--
-- This operation requires permissions for the @lex:PutBotAlias@ action.
module Network.AWS.LexModels.PutBotAlias
  ( -- * Creating a Request
    putBotAlias,
    PutBotAlias,

    -- * Request Lenses
    pbaChecksum,
    pbaConversationLogs,
    pbaDescription,
    pbaTags,
    pbaName,
    pbaBotVersion,
    pbaBotName,

    -- * Destructuring the Response
    putBotAliasResponse,
    PutBotAliasResponse,

    -- * Response Lenses
    pbarsChecksum,
    pbarsBotVersion,
    pbarsBotName,
    pbarsCreatedDate,
    pbarsName,
    pbarsConversationLogs,
    pbarsLastUpdatedDate,
    pbarsDescription,
    pbarsTags,
    pbarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putBotAlias' smart constructor.
data PutBotAlias = PutBotAlias'
  { _pbaChecksum :: !(Maybe Text),
    _pbaConversationLogs :: !(Maybe ConversationLogsRequest),
    _pbaDescription :: !(Maybe Text),
    _pbaTags :: !(Maybe [Tag]),
    _pbaName :: !Text,
    _pbaBotVersion :: !Text,
    _pbaBotName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBotAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbaChecksum' - Identifies a specific revision of the @> LATEST@ version. When you create a new bot alias, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a bot alias, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- * 'pbaConversationLogs' - Settings for conversation logs for the alias.
--
-- * 'pbaDescription' - A description of the alias.
--
-- * 'pbaTags' - A list of tags to add to the bot alias. You can only add tags when you create an alias, you can't use the @PutBotAlias@ operation to update the tags on a bot alias. To update tags, use the @TagResource@ operation.
--
-- * 'pbaName' - The name of the alias. The name is /not/ case sensitive.
--
-- * 'pbaBotVersion' - The version of the bot.
--
-- * 'pbaBotName' - The name of the bot.
putBotAlias ::
  -- | 'pbaName'
  Text ->
  -- | 'pbaBotVersion'
  Text ->
  -- | 'pbaBotName'
  Text ->
  PutBotAlias
putBotAlias pName_ pBotVersion_ pBotName_ =
  PutBotAlias'
    { _pbaChecksum = Nothing,
      _pbaConversationLogs = Nothing,
      _pbaDescription = Nothing,
      _pbaTags = Nothing,
      _pbaName = pName_,
      _pbaBotVersion = pBotVersion_,
      _pbaBotName = pBotName_
    }

-- | Identifies a specific revision of the @> LATEST@ version. When you create a new bot alias, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception. When you want to update a bot alias, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
pbaChecksum :: Lens' PutBotAlias (Maybe Text)
pbaChecksum = lens _pbaChecksum (\s a -> s {_pbaChecksum = a})

-- | Settings for conversation logs for the alias.
pbaConversationLogs :: Lens' PutBotAlias (Maybe ConversationLogsRequest)
pbaConversationLogs = lens _pbaConversationLogs (\s a -> s {_pbaConversationLogs = a})

-- | A description of the alias.
pbaDescription :: Lens' PutBotAlias (Maybe Text)
pbaDescription = lens _pbaDescription (\s a -> s {_pbaDescription = a})

-- | A list of tags to add to the bot alias. You can only add tags when you create an alias, you can't use the @PutBotAlias@ operation to update the tags on a bot alias. To update tags, use the @TagResource@ operation.
pbaTags :: Lens' PutBotAlias [Tag]
pbaTags = lens _pbaTags (\s a -> s {_pbaTags = a}) . _Default . _Coerce

-- | The name of the alias. The name is /not/ case sensitive.
pbaName :: Lens' PutBotAlias Text
pbaName = lens _pbaName (\s a -> s {_pbaName = a})

-- | The version of the bot.
pbaBotVersion :: Lens' PutBotAlias Text
pbaBotVersion = lens _pbaBotVersion (\s a -> s {_pbaBotVersion = a})

-- | The name of the bot.
pbaBotName :: Lens' PutBotAlias Text
pbaBotName = lens _pbaBotName (\s a -> s {_pbaBotName = a})

instance AWSRequest PutBotAlias where
  type Rs PutBotAlias = PutBotAliasResponse
  request = putJSON lexModels
  response =
    receiveJSON
      ( \s h x ->
          PutBotAliasResponse'
            <$> (x .?> "checksum")
            <*> (x .?> "botVersion")
            <*> (x .?> "botName")
            <*> (x .?> "createdDate")
            <*> (x .?> "name")
            <*> (x .?> "conversationLogs")
            <*> (x .?> "lastUpdatedDate")
            <*> (x .?> "description")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable PutBotAlias

instance NFData PutBotAlias

instance ToHeaders PutBotAlias where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON PutBotAlias where
  toJSON PutBotAlias' {..} =
    object
      ( catMaybes
          [ ("checksum" .=) <$> _pbaChecksum,
            ("conversationLogs" .=) <$> _pbaConversationLogs,
            ("description" .=) <$> _pbaDescription,
            ("tags" .=) <$> _pbaTags,
            Just ("botVersion" .= _pbaBotVersion)
          ]
      )

instance ToPath PutBotAlias where
  toPath PutBotAlias' {..} =
    mconcat ["/bots/", toBS _pbaBotName, "/aliases/", toBS _pbaName]

instance ToQuery PutBotAlias where
  toQuery = const mempty

-- | /See:/ 'putBotAliasResponse' smart constructor.
data PutBotAliasResponse = PutBotAliasResponse'
  { _pbarsChecksum ::
      !(Maybe Text),
    _pbarsBotVersion :: !(Maybe Text),
    _pbarsBotName :: !(Maybe Text),
    _pbarsCreatedDate :: !(Maybe POSIX),
    _pbarsName :: !(Maybe Text),
    _pbarsConversationLogs ::
      !(Maybe ConversationLogsResponse),
    _pbarsLastUpdatedDate :: !(Maybe POSIX),
    _pbarsDescription :: !(Maybe Text),
    _pbarsTags :: !(Maybe [Tag]),
    _pbarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBotAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbarsChecksum' - The checksum for the current version of the alias.
--
-- * 'pbarsBotVersion' - The version of the bot that the alias points to.
--
-- * 'pbarsBotName' - The name of the bot that the alias points to.
--
-- * 'pbarsCreatedDate' - The date that the bot alias was created.
--
-- * 'pbarsName' - The name of the alias.
--
-- * 'pbarsConversationLogs' - The settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- * 'pbarsLastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- * 'pbarsDescription' - A description of the alias.
--
-- * 'pbarsTags' - A list of tags associated with a bot.
--
-- * 'pbarsResponseStatus' - -- | The response status code.
putBotAliasResponse ::
  -- | 'pbarsResponseStatus'
  Int ->
  PutBotAliasResponse
putBotAliasResponse pResponseStatus_ =
  PutBotAliasResponse'
    { _pbarsChecksum = Nothing,
      _pbarsBotVersion = Nothing,
      _pbarsBotName = Nothing,
      _pbarsCreatedDate = Nothing,
      _pbarsName = Nothing,
      _pbarsConversationLogs = Nothing,
      _pbarsLastUpdatedDate = Nothing,
      _pbarsDescription = Nothing,
      _pbarsTags = Nothing,
      _pbarsResponseStatus = pResponseStatus_
    }

-- | The checksum for the current version of the alias.
pbarsChecksum :: Lens' PutBotAliasResponse (Maybe Text)
pbarsChecksum = lens _pbarsChecksum (\s a -> s {_pbarsChecksum = a})

-- | The version of the bot that the alias points to.
pbarsBotVersion :: Lens' PutBotAliasResponse (Maybe Text)
pbarsBotVersion = lens _pbarsBotVersion (\s a -> s {_pbarsBotVersion = a})

-- | The name of the bot that the alias points to.
pbarsBotName :: Lens' PutBotAliasResponse (Maybe Text)
pbarsBotName = lens _pbarsBotName (\s a -> s {_pbarsBotName = a})

-- | The date that the bot alias was created.
pbarsCreatedDate :: Lens' PutBotAliasResponse (Maybe UTCTime)
pbarsCreatedDate = lens _pbarsCreatedDate (\s a -> s {_pbarsCreatedDate = a}) . mapping _Time

-- | The name of the alias.
pbarsName :: Lens' PutBotAliasResponse (Maybe Text)
pbarsName = lens _pbarsName (\s a -> s {_pbarsName = a})

-- | The settings that determine how Amazon Lex uses conversation logs for the alias.
pbarsConversationLogs :: Lens' PutBotAliasResponse (Maybe ConversationLogsResponse)
pbarsConversationLogs = lens _pbarsConversationLogs (\s a -> s {_pbarsConversationLogs = a})

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
pbarsLastUpdatedDate :: Lens' PutBotAliasResponse (Maybe UTCTime)
pbarsLastUpdatedDate = lens _pbarsLastUpdatedDate (\s a -> s {_pbarsLastUpdatedDate = a}) . mapping _Time

-- | A description of the alias.
pbarsDescription :: Lens' PutBotAliasResponse (Maybe Text)
pbarsDescription = lens _pbarsDescription (\s a -> s {_pbarsDescription = a})

-- | A list of tags associated with a bot.
pbarsTags :: Lens' PutBotAliasResponse [Tag]
pbarsTags = lens _pbarsTags (\s a -> s {_pbarsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
pbarsResponseStatus :: Lens' PutBotAliasResponse Int
pbarsResponseStatus = lens _pbarsResponseStatus (\s a -> s {_pbarsResponseStatus = a})

instance NFData PutBotAliasResponse
