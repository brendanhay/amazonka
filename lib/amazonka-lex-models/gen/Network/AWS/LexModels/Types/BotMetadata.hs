{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotMetadata
  ( BotMetadata (..),

    -- * Smart constructor
    mkBotMetadata,

    -- * Lenses
    bmCreatedDate,
    bmDescription,
    bmLastUpdatedDate,
    bmName,
    bmStatus,
    bmVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.BotName as Types
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.LexModels.Types.LexStatus as Types
import qualified Network.AWS.LexModels.Types.Version as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a bot. .
--
-- /See:/ 'mkBotMetadata' smart constructor.
data BotMetadata = BotMetadata'
  { -- | The date that the bot was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the bot.
    description :: Core.Maybe Types.Description,
    -- | The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the bot.
    name :: Core.Maybe Types.BotName,
    -- | The status of the bot.
    status :: Core.Maybe Types.LexStatus,
    -- | The version of the bot. For a new bot, the version is always @> LATEST@ .
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BotMetadata' value with any optional fields omitted.
mkBotMetadata ::
  BotMetadata
mkBotMetadata =
  BotMetadata'
    { createdDate = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      version = Core.Nothing
    }

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmCreatedDate :: Lens.Lens' BotMetadata (Core.Maybe Core.NominalDiffTime)
bmCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED bmCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmDescription :: Lens.Lens' BotMetadata (Core.Maybe Types.Description)
bmDescription = Lens.field @"description"
{-# DEPRECATED bmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmLastUpdatedDate :: Lens.Lens' BotMetadata (Core.Maybe Core.NominalDiffTime)
bmLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED bmLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmName :: Lens.Lens' BotMetadata (Core.Maybe Types.BotName)
bmName = Lens.field @"name"
{-# DEPRECATED bmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status of the bot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmStatus :: Lens.Lens' BotMetadata (Core.Maybe Types.LexStatus)
bmStatus = Lens.field @"status"
{-# DEPRECATED bmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmVersion :: Lens.Lens' BotMetadata (Core.Maybe Types.Version)
bmVersion = Lens.field @"version"
{-# DEPRECATED bmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON BotMetadata where
  parseJSON =
    Core.withObject "BotMetadata" Core.$
      \x ->
        BotMetadata'
          Core.<$> (x Core..:? "createdDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "lastUpdatedDate")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "version")
