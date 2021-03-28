{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalNodeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.GlobalNodeGroup
  ( GlobalNodeGroup (..)
  -- * Smart constructor
  , mkGlobalNodeGroup
  -- * Lenses
  , gngGlobalNodeGroupId
  , gngSlots
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates the slot configuration and global identifier for a slice group.
--
-- /See:/ 'mkGlobalNodeGroup' smart constructor.
data GlobalNodeGroup = GlobalNodeGroup'
  { globalNodeGroupId :: Core.Maybe Core.Text
    -- ^ The name of the global node group
  , slots :: Core.Maybe Core.Text
    -- ^ The keyspace for this node group
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalNodeGroup' value with any optional fields omitted.
mkGlobalNodeGroup
    :: GlobalNodeGroup
mkGlobalNodeGroup
  = GlobalNodeGroup'{globalNodeGroupId = Core.Nothing,
                     slots = Core.Nothing}

-- | The name of the global node group
--
-- /Note:/ Consider using 'globalNodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gngGlobalNodeGroupId :: Lens.Lens' GlobalNodeGroup (Core.Maybe Core.Text)
gngGlobalNodeGroupId = Lens.field @"globalNodeGroupId"
{-# INLINEABLE gngGlobalNodeGroupId #-}
{-# DEPRECATED globalNodeGroupId "Use generic-lens or generic-optics with 'globalNodeGroupId' instead"  #-}

-- | The keyspace for this node group
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gngSlots :: Lens.Lens' GlobalNodeGroup (Core.Maybe Core.Text)
gngSlots = Lens.field @"slots"
{-# INLINEABLE gngSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

instance Core.FromXML GlobalNodeGroup where
        parseXML x
          = GlobalNodeGroup' Core.<$>
              (x Core..@? "GlobalNodeGroupId") Core.<*> x Core..@? "Slots"
