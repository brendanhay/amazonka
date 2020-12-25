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
    bamBotName,
    bamBotVersion,
    bamChecksum,
    bamConversationLogs,
    bamCreatedDate,
    bamDescription,
    bamLastUpdatedDate,
    bamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.AliasName as Types
import qualified Network.AWS.LexModels.Types.BotName as Types
import qualified Network.AWS.LexModels.Types.ConversationLogsResponse as Types
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.LexModels.Types.String as Types
import qualified Network.AWS.LexModels.Types.Version as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a bot alias.
--
-- /See:/ 'mkBotAliasMetadata' smart constructor.
data BotAliasMetadata = BotAliasMetadata'
  { -- | The name of the bot to which the alias points.
    botName :: Core.Maybe Types.BotName,
    -- | The version of the Amazon Lex bot to which the alias points.
    botVersion :: Core.Maybe Types.Version,
    -- | Checksum of the bot alias.
    checksum :: Core.Maybe Types.String,
    -- | Settings that determine how Amazon Lex uses conversation logs for the alias.
    conversationLogs :: Core.Maybe Types.ConversationLogsResponse,
    -- | The date that the bot alias was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the bot alias.
    description :: Core.Maybe Types.Description,
    -- | The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the bot alias.
    name :: Core.Maybe Types.AliasName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BotAliasMetadata' value with any optional fields omitted.
mkBotAliasMetadata ::
  BotAliasMetadata
mkBotAliasMetadata =
  BotAliasMetadata'
    { botName = Core.Nothing,
      botVersion = Core.Nothing,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing
    }

-- | The name of the bot to which the alias points.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamBotName :: Lens.Lens' BotAliasMetadata (Core.Maybe Types.BotName)
bamBotName = Lens.field @"botName"
{-# DEPRECATED bamBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The version of the Amazon Lex bot to which the alias points.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamBotVersion :: Lens.Lens' BotAliasMetadata (Core.Maybe Types.Version)
bamBotVersion = Lens.field @"botVersion"
{-# DEPRECATED bamBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | Checksum of the bot alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamChecksum :: Lens.Lens' BotAliasMetadata (Core.Maybe Types.String)
bamChecksum = Lens.field @"checksum"
{-# DEPRECATED bamChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamConversationLogs :: Lens.Lens' BotAliasMetadata (Core.Maybe Types.ConversationLogsResponse)
bamConversationLogs = Lens.field @"conversationLogs"
{-# DEPRECATED bamConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamCreatedDate :: Lens.Lens' BotAliasMetadata (Core.Maybe Core.NominalDiffTime)
bamCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED bamCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the bot alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamDescription :: Lens.Lens' BotAliasMetadata (Core.Maybe Types.Description)
bamDescription = Lens.field @"description"
{-# DEPRECATED bamDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamLastUpdatedDate :: Lens.Lens' BotAliasMetadata (Core.Maybe Core.NominalDiffTime)
bamLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED bamLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the bot alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bamName :: Lens.Lens' BotAliasMetadata (Core.Maybe Types.AliasName)
bamName = Lens.field @"name"
{-# DEPRECATED bamName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON BotAliasMetadata where
  parseJSON =
    Core.withObject "BotAliasMetadata" Core.$
      \x ->
        BotAliasMetadata'
          Core.<$> (x Core..:? "botName")
          Core.<*> (x Core..:? "botVersion")
          Core.<*> (x Core..:? "checksum")
          Core.<*> (x Core..:? "conversationLogs")
          Core.<*> (x Core..:? "createdDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "lastUpdatedDate")
          Core.<*> (x Core..:? "name")
