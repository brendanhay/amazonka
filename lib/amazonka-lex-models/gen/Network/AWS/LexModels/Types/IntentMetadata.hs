{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.IntentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.IntentMetadata
  ( IntentMetadata (..),

    -- * Smart constructor
    mkIntentMetadata,

    -- * Lenses
    imCreatedDate,
    imDescription,
    imLastUpdatedDate,
    imName,
    imVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.LexModels.Types.IntentName as Types
import qualified Network.AWS.LexModels.Types.Version as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about an intent.
--
-- /See:/ 'mkIntentMetadata' smart constructor.
data IntentMetadata = IntentMetadata'
  { -- | The date that the intent was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the intent.
    description :: Core.Maybe Types.Description,
    -- | The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the intent.
    name :: Core.Maybe Types.IntentName,
    -- | The version of the intent.
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'IntentMetadata' value with any optional fields omitted.
mkIntentMetadata ::
  IntentMetadata
mkIntentMetadata =
  IntentMetadata'
    { createdDate = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      version = Core.Nothing
    }

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imCreatedDate :: Lens.Lens' IntentMetadata (Core.Maybe Core.NominalDiffTime)
imCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED imCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imDescription :: Lens.Lens' IntentMetadata (Core.Maybe Types.Description)
imDescription = Lens.field @"description"
{-# DEPRECATED imDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imLastUpdatedDate :: Lens.Lens' IntentMetadata (Core.Maybe Core.NominalDiffTime)
imLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED imLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imName :: Lens.Lens' IntentMetadata (Core.Maybe Types.IntentName)
imName = Lens.field @"name"
{-# DEPRECATED imName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imVersion :: Lens.Lens' IntentMetadata (Core.Maybe Types.Version)
imVersion = Lens.field @"version"
{-# DEPRECATED imVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON IntentMetadata where
  parseJSON =
    Core.withObject "IntentMetadata" Core.$
      \x ->
        IntentMetadata'
          Core.<$> (x Core..:? "createdDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "lastUpdatedDate")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "version")
