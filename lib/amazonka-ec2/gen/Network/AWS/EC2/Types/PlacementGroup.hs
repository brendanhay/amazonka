{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PlacementGroup
  ( PlacementGroup (..)
  -- * Smart constructor
  , mkPlacementGroup
  -- * Lenses
  , pgGroupId
  , pgGroupName
  , pgPartitionCount
  , pgState
  , pgStrategy
  , pgTags
  ) where

import qualified Network.AWS.EC2.Types.PlacementGroupState as Types
import qualified Network.AWS.EC2.Types.PlacementStrategy as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a placement group.
--
-- /See:/ 'mkPlacementGroup' smart constructor.
data PlacementGroup = PlacementGroup'
  { groupId :: Core.Maybe Core.Text
    -- ^ The ID of the placement group.
  , groupName :: Core.Maybe Core.Text
    -- ^ The name of the placement group.
  , partitionCount :: Core.Maybe Core.Int
    -- ^ The number of partitions. Valid only if __strategy__ is set to @partition@ .
  , state :: Core.Maybe Types.PlacementGroupState
    -- ^ The state of the placement group.
  , strategy :: Core.Maybe Types.PlacementStrategy
    -- ^ The placement strategy.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags applied to the placement group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementGroup' value with any optional fields omitted.
mkPlacementGroup
    :: PlacementGroup
mkPlacementGroup
  = PlacementGroup'{groupId = Core.Nothing, groupName = Core.Nothing,
                    partitionCount = Core.Nothing, state = Core.Nothing,
                    strategy = Core.Nothing, tags = Core.Nothing}

-- | The ID of the placement group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgGroupId :: Lens.Lens' PlacementGroup (Core.Maybe Core.Text)
pgGroupId = Lens.field @"groupId"
{-# INLINEABLE pgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgGroupName :: Lens.Lens' PlacementGroup (Core.Maybe Core.Text)
pgGroupName = Lens.field @"groupName"
{-# INLINEABLE pgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The number of partitions. Valid only if __strategy__ is set to @partition@ .
--
-- /Note:/ Consider using 'partitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgPartitionCount :: Lens.Lens' PlacementGroup (Core.Maybe Core.Int)
pgPartitionCount = Lens.field @"partitionCount"
{-# INLINEABLE pgPartitionCount #-}
{-# DEPRECATED partitionCount "Use generic-lens or generic-optics with 'partitionCount' instead"  #-}

-- | The state of the placement group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgState :: Lens.Lens' PlacementGroup (Core.Maybe Types.PlacementGroupState)
pgState = Lens.field @"state"
{-# INLINEABLE pgState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The placement strategy.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgStrategy :: Lens.Lens' PlacementGroup (Core.Maybe Types.PlacementStrategy)
pgStrategy = Lens.field @"strategy"
{-# INLINEABLE pgStrategy #-}
{-# DEPRECATED strategy "Use generic-lens or generic-optics with 'strategy' instead"  #-}

-- | Any tags applied to the placement group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgTags :: Lens.Lens' PlacementGroup (Core.Maybe [Types.Tag])
pgTags = Lens.field @"tags"
{-# INLINEABLE pgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML PlacementGroup where
        parseXML x
          = PlacementGroup' Core.<$>
              (x Core..@? "groupId") Core.<*> x Core..@? "groupName" Core.<*>
                x Core..@? "partitionCount"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "strategy"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
