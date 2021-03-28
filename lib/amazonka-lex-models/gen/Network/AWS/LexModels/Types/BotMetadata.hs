{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.BotMetadata
  ( BotMetadata (..)
  -- * Smart constructor
  , mkBotMetadata
  -- * Lenses
  , bmCreatedDate
  , bmDescription
  , bmLastUpdatedDate
  , bmName
  , bmStatus
  , bmVersion
  ) where

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
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the bot.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot was updated. When you create a bot, the creation date and last updated date are the same. 
  , name :: Core.Maybe Types.BotName
    -- ^ The name of the bot. 
  , status :: Core.Maybe Types.LexStatus
    -- ^ The status of the bot.
  , version :: Core.Maybe Types.Version
    -- ^ The version of the bot. For a new bot, the version is always @> LATEST@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BotMetadata' value with any optional fields omitted.
mkBotMetadata
    :: BotMetadata
mkBotMetadata
  = BotMetadata'{createdDate = Core.Nothing,
                 description = Core.Nothing, lastUpdatedDate = Core.Nothing,
                 name = Core.Nothing, status = Core.Nothing, version = Core.Nothing}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmCreatedDate :: Lens.Lens' BotMetadata (Core.Maybe Core.NominalDiffTime)
bmCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE bmCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmDescription :: Lens.Lens' BotMetadata (Core.Maybe Types.Description)
bmDescription = Lens.field @"description"
{-# INLINEABLE bmDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The date that the bot was updated. When you create a bot, the creation date and last updated date are the same. 
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmLastUpdatedDate :: Lens.Lens' BotMetadata (Core.Maybe Core.NominalDiffTime)
bmLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE bmLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the bot. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmName :: Lens.Lens' BotMetadata (Core.Maybe Types.BotName)
bmName = Lens.field @"name"
{-# INLINEABLE bmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status of the bot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmStatus :: Lens.Lens' BotMetadata (Core.Maybe Types.LexStatus)
bmStatus = Lens.field @"status"
{-# INLINEABLE bmStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmVersion :: Lens.Lens' BotMetadata (Core.Maybe Types.Version)
bmVersion = Lens.field @"version"
{-# INLINEABLE bmVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON BotMetadata where
        parseJSON
          = Core.withObject "BotMetadata" Core.$
              \ x ->
                BotMetadata' Core.<$>
                  (x Core..:? "createdDate") Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "lastUpdatedDate"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "version"
