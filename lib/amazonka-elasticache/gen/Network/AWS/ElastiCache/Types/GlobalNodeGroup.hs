{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalNodeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalNodeGroup
  ( GlobalNodeGroup (..),

    -- * Smart constructor
    mkGlobalNodeGroup,

    -- * Lenses
    gngGlobalNodeGroupId,
    gngSlots,
  )
where

import qualified Network.AWS.ElastiCache.Types.GlobalNodeGroupId as Types
import qualified Network.AWS.ElastiCache.Types.Slots as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates the slot configuration and global identifier for a slice group.
--
-- /See:/ 'mkGlobalNodeGroup' smart constructor.
data GlobalNodeGroup = GlobalNodeGroup'
  { -- | The name of the global node group
    globalNodeGroupId :: Core.Maybe Types.GlobalNodeGroupId,
    -- | The keyspace for this node group
    slots :: Core.Maybe Types.Slots
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalNodeGroup' value with any optional fields omitted.
mkGlobalNodeGroup ::
  GlobalNodeGroup
mkGlobalNodeGroup =
  GlobalNodeGroup'
    { globalNodeGroupId = Core.Nothing,
      slots = Core.Nothing
    }

-- | The name of the global node group
--
-- /Note:/ Consider using 'globalNodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gngGlobalNodeGroupId :: Lens.Lens' GlobalNodeGroup (Core.Maybe Types.GlobalNodeGroupId)
gngGlobalNodeGroupId = Lens.field @"globalNodeGroupId"
{-# DEPRECATED gngGlobalNodeGroupId "Use generic-lens or generic-optics with 'globalNodeGroupId' instead." #-}

-- | The keyspace for this node group
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gngSlots :: Lens.Lens' GlobalNodeGroup (Core.Maybe Types.Slots)
gngSlots = Lens.field @"slots"
{-# DEPRECATED gngSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

instance Core.FromXML GlobalNodeGroup where
  parseXML x =
    GlobalNodeGroup'
      Core.<$> (x Core..@? "GlobalNodeGroupId") Core.<*> (x Core..@? "Slots")
