{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotAliasMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotAliasMetadata where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.ConversationLogsResponse
import Network.AWS.Prelude

-- | Provides information about a bot alias.
--
--
--
-- /See:/ 'botAliasMetadata' smart constructor.
data BotAliasMetadata = BotAliasMetadata'
  { _bamChecksum ::
      !(Maybe Text),
    _bamBotVersion :: !(Maybe Text),
    _bamBotName :: !(Maybe Text),
    _bamCreatedDate :: !(Maybe POSIX),
    _bamName :: !(Maybe Text),
    _bamConversationLogs :: !(Maybe ConversationLogsResponse),
    _bamLastUpdatedDate :: !(Maybe POSIX),
    _bamDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BotAliasMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bamChecksum' - Checksum of the bot alias.
--
-- * 'bamBotVersion' - The version of the Amazon Lex bot to which the alias points.
--
-- * 'bamBotName' - The name of the bot to which the alias points.
--
-- * 'bamCreatedDate' - The date that the bot alias was created.
--
-- * 'bamName' - The name of the bot alias.
--
-- * 'bamConversationLogs' - Settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- * 'bamLastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
--
-- * 'bamDescription' - A description of the bot alias.
botAliasMetadata ::
  BotAliasMetadata
botAliasMetadata =
  BotAliasMetadata'
    { _bamChecksum = Nothing,
      _bamBotVersion = Nothing,
      _bamBotName = Nothing,
      _bamCreatedDate = Nothing,
      _bamName = Nothing,
      _bamConversationLogs = Nothing,
      _bamLastUpdatedDate = Nothing,
      _bamDescription = Nothing
    }

-- | Checksum of the bot alias.
bamChecksum :: Lens' BotAliasMetadata (Maybe Text)
bamChecksum = lens _bamChecksum (\s a -> s {_bamChecksum = a})

-- | The version of the Amazon Lex bot to which the alias points.
bamBotVersion :: Lens' BotAliasMetadata (Maybe Text)
bamBotVersion = lens _bamBotVersion (\s a -> s {_bamBotVersion = a})

-- | The name of the bot to which the alias points.
bamBotName :: Lens' BotAliasMetadata (Maybe Text)
bamBotName = lens _bamBotName (\s a -> s {_bamBotName = a})

-- | The date that the bot alias was created.
bamCreatedDate :: Lens' BotAliasMetadata (Maybe UTCTime)
bamCreatedDate = lens _bamCreatedDate (\s a -> s {_bamCreatedDate = a}) . mapping _Time

-- | The name of the bot alias.
bamName :: Lens' BotAliasMetadata (Maybe Text)
bamName = lens _bamName (\s a -> s {_bamName = a})

-- | Settings that determine how Amazon Lex uses conversation logs for the alias.
bamConversationLogs :: Lens' BotAliasMetadata (Maybe ConversationLogsResponse)
bamConversationLogs = lens _bamConversationLogs (\s a -> s {_bamConversationLogs = a})

-- | The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
bamLastUpdatedDate :: Lens' BotAliasMetadata (Maybe UTCTime)
bamLastUpdatedDate = lens _bamLastUpdatedDate (\s a -> s {_bamLastUpdatedDate = a}) . mapping _Time

-- | A description of the bot alias.
bamDescription :: Lens' BotAliasMetadata (Maybe Text)
bamDescription = lens _bamDescription (\s a -> s {_bamDescription = a})

instance FromJSON BotAliasMetadata where
  parseJSON =
    withObject
      "BotAliasMetadata"
      ( \x ->
          BotAliasMetadata'
            <$> (x .:? "checksum")
            <*> (x .:? "botVersion")
            <*> (x .:? "botName")
            <*> (x .:? "createdDate")
            <*> (x .:? "name")
            <*> (x .:? "conversationLogs")
            <*> (x .:? "lastUpdatedDate")
            <*> (x .:? "description")
      )

instance Hashable BotAliasMetadata

instance NFData BotAliasMetadata
