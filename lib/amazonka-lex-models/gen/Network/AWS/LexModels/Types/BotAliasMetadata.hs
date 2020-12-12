{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotAliasMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotAliasMetadata
  ( BotAliasMetadata (..),

    -- * Smart constructor
    mkBotAliasMetadata,

    -- * Lenses
    bamChecksum,
    bamBotVersion,
    bamBotName,
    bamCreatedDate,
    bamName,
    bamConversationLogs,
    bamLastUpdatedDate,
    bamDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ConversationLogsResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a bot alias.
--
-- /See:/ 'mkBotAliasMetadata' smart constructor.
data BotAliasMetadata = BotAliasMetadata'
  { checksum ::
      Lude.Maybe Lude.Text,
    botVersion :: Lude.Maybe Lude.Text,
    botName :: Lude.Maybe Lude.Text,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    conversationLogs :: Lude.Maybe ConversationLogsResponse,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BotAliasMetadata' with the minimum fields required to make a request.
--
-- * 'botName' - The name of the bot to which the alias points.
-- * 'botVersion' - The version of the Amazon Lex bot to which the alias points.
-- * 'checksum' - Checksum of the bot alias.
-- * 'conversationLogs' - Settings that determine how Amazon Lex uses conversation logs for the alias.
-- * 'createdDate' - The date that the bot alias was created.
-- * 'description' - A description of the bot alias.
-- * 'lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
-- * 'name' - The name of the bot alias.
mkBotAliasMetadata ::
  BotAliasMetadata
mkBotAliasMetadata =
  BotAliasMetadata'
    { checksum = Lude.Nothing,
      botVersion = Lude.Nothing,
      botName = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      conversationLogs = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Checksum of the bot alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamChecksum :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Text)
bamChecksum = Lens.lens (checksum :: BotAliasMetadata -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: BotAliasMetadata)
{-# DEPRECATED bamChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The version of the Amazon Lex bot to which the alias points.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamBotVersion :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Text)
bamBotVersion = Lens.lens (botVersion :: BotAliasMetadata -> Lude.Maybe Lude.Text) (\s a -> s {botVersion = a} :: BotAliasMetadata)
{-# DEPRECATED bamBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | The name of the bot to which the alias points.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamBotName :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Text)
bamBotName = Lens.lens (botName :: BotAliasMetadata -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: BotAliasMetadata)
{-# DEPRECATED bamBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamCreatedDate :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Timestamp)
bamCreatedDate = Lens.lens (createdDate :: BotAliasMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: BotAliasMetadata)
{-# DEPRECATED bamCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the bot alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamName :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Text)
bamName = Lens.lens (name :: BotAliasMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: BotAliasMetadata)
{-# DEPRECATED bamName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamConversationLogs :: Lens.Lens' BotAliasMetadata (Lude.Maybe ConversationLogsResponse)
bamConversationLogs = Lens.lens (conversationLogs :: BotAliasMetadata -> Lude.Maybe ConversationLogsResponse) (\s a -> s {conversationLogs = a} :: BotAliasMetadata)
{-# DEPRECATED bamConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamLastUpdatedDate :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Timestamp)
bamLastUpdatedDate = Lens.lens (lastUpdatedDate :: BotAliasMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: BotAliasMetadata)
{-# DEPRECATED bamLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the bot alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamDescription :: Lens.Lens' BotAliasMetadata (Lude.Maybe Lude.Text)
bamDescription = Lens.lens (description :: BotAliasMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: BotAliasMetadata)
{-# DEPRECATED bamDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON BotAliasMetadata where
  parseJSON =
    Lude.withObject
      "BotAliasMetadata"
      ( \x ->
          BotAliasMetadata'
            Lude.<$> (x Lude..:? "checksum")
            Lude.<*> (x Lude..:? "botVersion")
            Lude.<*> (x Lude..:? "botName")
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "conversationLogs")
            Lude.<*> (x Lude..:? "lastUpdatedDate")
            Lude.<*> (x Lude..:? "description")
      )
