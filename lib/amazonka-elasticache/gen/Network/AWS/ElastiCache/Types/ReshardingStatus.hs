{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReshardingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ReshardingStatus
  ( ReshardingStatus (..)
  -- * Smart constructor
  , mkReshardingStatus
  -- * Lenses
  , rsSlotMigration
  ) where

import qualified Network.AWS.ElastiCache.Types.SlotMigration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of an online resharding operation.
--
-- /See:/ 'mkReshardingStatus' smart constructor.
newtype ReshardingStatus = ReshardingStatus'
  { slotMigration :: Core.Maybe Types.SlotMigration
    -- ^ Represents the progress of an online resharding operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReshardingStatus' value with any optional fields omitted.
mkReshardingStatus
    :: ReshardingStatus
mkReshardingStatus
  = ReshardingStatus'{slotMigration = Core.Nothing}

-- | Represents the progress of an online resharding operation.
--
-- /Note:/ Consider using 'slotMigration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSlotMigration :: Lens.Lens' ReshardingStatus (Core.Maybe Types.SlotMigration)
rsSlotMigration = Lens.field @"slotMigration"
{-# INLINEABLE rsSlotMigration #-}
{-# DEPRECATED slotMigration "Use generic-lens or generic-optics with 'slotMigration' instead"  #-}

instance Core.FromXML ReshardingStatus where
        parseXML x
          = ReshardingStatus' Core.<$> (x Core..@? "SlotMigration")
