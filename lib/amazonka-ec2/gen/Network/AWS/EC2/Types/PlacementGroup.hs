{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PlacementGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PlacementGroup
  ( PlacementGroup (..),

    -- * Smart constructor
    mkPlacementGroup,

    -- * Lenses
    pgState,
    pgStrategy,
    pgGroupId,
    pgGroupName,
    pgPartitionCount,
    pgTags,
  )
where

import Network.AWS.EC2.Types.PlacementGroupState
import Network.AWS.EC2.Types.PlacementStrategy
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a placement group.
--
-- /See:/ 'mkPlacementGroup' smart constructor.
data PlacementGroup = PlacementGroup'
  { state ::
      Lude.Maybe PlacementGroupState,
    strategy :: Lude.Maybe PlacementStrategy,
    groupId :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    partitionCount :: Lude.Maybe Lude.Int,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlacementGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the placement group.
-- * 'groupName' - The name of the placement group.
-- * 'partitionCount' - The number of partitions. Valid only if __strategy__ is set to @partition@ .
-- * 'state' - The state of the placement group.
-- * 'strategy' - The placement strategy.
-- * 'tags' - Any tags applied to the placement group.
mkPlacementGroup ::
  PlacementGroup
mkPlacementGroup =
  PlacementGroup'
    { state = Lude.Nothing,
      strategy = Lude.Nothing,
      groupId = Lude.Nothing,
      groupName = Lude.Nothing,
      partitionCount = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the placement group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgState :: Lens.Lens' PlacementGroup (Lude.Maybe PlacementGroupState)
pgState = Lens.lens (state :: PlacementGroup -> Lude.Maybe PlacementGroupState) (\s a -> s {state = a} :: PlacementGroup)
{-# DEPRECATED pgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The placement strategy.
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgStrategy :: Lens.Lens' PlacementGroup (Lude.Maybe PlacementStrategy)
pgStrategy = Lens.lens (strategy :: PlacementGroup -> Lude.Maybe PlacementStrategy) (\s a -> s {strategy = a} :: PlacementGroup)
{-# DEPRECATED pgStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The ID of the placement group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgGroupId :: Lens.Lens' PlacementGroup (Lude.Maybe Lude.Text)
pgGroupId = Lens.lens (groupId :: PlacementGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: PlacementGroup)
{-# DEPRECATED pgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgGroupName :: Lens.Lens' PlacementGroup (Lude.Maybe Lude.Text)
pgGroupName = Lens.lens (groupName :: PlacementGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: PlacementGroup)
{-# DEPRECATED pgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The number of partitions. Valid only if __strategy__ is set to @partition@ .
--
-- /Note:/ Consider using 'partitionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgPartitionCount :: Lens.Lens' PlacementGroup (Lude.Maybe Lude.Int)
pgPartitionCount = Lens.lens (partitionCount :: PlacementGroup -> Lude.Maybe Lude.Int) (\s a -> s {partitionCount = a} :: PlacementGroup)
{-# DEPRECATED pgPartitionCount "Use generic-lens or generic-optics with 'partitionCount' instead." #-}

-- | Any tags applied to the placement group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgTags :: Lens.Lens' PlacementGroup (Lude.Maybe [Tag])
pgTags = Lens.lens (tags :: PlacementGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PlacementGroup)
{-# DEPRECATED pgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML PlacementGroup where
  parseXML x =
    PlacementGroup'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "strategy")
      Lude.<*> (x Lude..@? "groupId")
      Lude.<*> (x Lude..@? "groupName")
      Lude.<*> (x Lude..@? "partitionCount")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
