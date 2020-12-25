{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeMetadata
  ( SlotTypeMetadata (..),

    -- * Smart constructor
    mkSlotTypeMetadata,

    -- * Lenses
    stmCreatedDate,
    stmDescription,
    stmLastUpdatedDate,
    stmName,
    stmVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.LexModels.Types.SlotTypeName as Types
import qualified Network.AWS.LexModels.Types.Version as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about a slot type..
--
-- /See:/ 'mkSlotTypeMetadata' smart constructor.
data SlotTypeMetadata = SlotTypeMetadata'
  { -- | The date that the slot type was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the slot type.
    description :: Core.Maybe Types.Description,
    -- | The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the slot type.
    name :: Core.Maybe Types.SlotTypeName,
    -- | The version of the slot type.
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SlotTypeMetadata' value with any optional fields omitted.
mkSlotTypeMetadata ::
  SlotTypeMetadata
mkSlotTypeMetadata =
  SlotTypeMetadata'
    { createdDate = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      version = Core.Nothing
    }

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmCreatedDate :: Lens.Lens' SlotTypeMetadata (Core.Maybe Core.NominalDiffTime)
stmCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED stmCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmDescription :: Lens.Lens' SlotTypeMetadata (Core.Maybe Types.Description)
stmDescription = Lens.field @"description"
{-# DEPRECATED stmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmLastUpdatedDate :: Lens.Lens' SlotTypeMetadata (Core.Maybe Core.NominalDiffTime)
stmLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED stmLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmName :: Lens.Lens' SlotTypeMetadata (Core.Maybe Types.SlotTypeName)
stmName = Lens.field @"name"
{-# DEPRECATED stmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmVersion :: Lens.Lens' SlotTypeMetadata (Core.Maybe Types.Version)
stmVersion = Lens.field @"version"
{-# DEPRECATED stmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON SlotTypeMetadata where
  parseJSON =
    Core.withObject "SlotTypeMetadata" Core.$
      \x ->
        SlotTypeMetadata'
          Core.<$> (x Core..:? "createdDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "lastUpdatedDate")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "version")
