{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SlotMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.SlotMigration
  ( SlotMigration (..)
  -- * Smart constructor
  , mkSlotMigration
  -- * Lenses
  , smProgressPercentage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the progress of an online resharding operation.
--
-- /See:/ 'mkSlotMigration' smart constructor.
newtype SlotMigration = SlotMigration'
  { progressPercentage :: Core.Maybe Core.Double
    -- ^ The percentage of the slot migration that is complete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SlotMigration' value with any optional fields omitted.
mkSlotMigration
    :: SlotMigration
mkSlotMigration = SlotMigration'{progressPercentage = Core.Nothing}

-- | The percentage of the slot migration that is complete.
--
-- /Note:/ Consider using 'progressPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smProgressPercentage :: Lens.Lens' SlotMigration (Core.Maybe Core.Double)
smProgressPercentage = Lens.field @"progressPercentage"
{-# INLINEABLE smProgressPercentage #-}
{-# DEPRECATED progressPercentage "Use generic-lens or generic-optics with 'progressPercentage' instead"  #-}

instance Core.FromXML SlotMigration where
        parseXML x
          = SlotMigration' Core.<$> (x Core..@? "ProgressPercentage")
