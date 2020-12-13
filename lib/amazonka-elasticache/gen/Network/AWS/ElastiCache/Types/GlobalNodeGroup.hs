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
    gngSlots,
    gngGlobalNodeGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates the slot configuration and global identifier for a slice group.
--
-- /See:/ 'mkGlobalNodeGroup' smart constructor.
data GlobalNodeGroup = GlobalNodeGroup'
  { -- | The keyspace for this node group
    slots :: Lude.Maybe Lude.Text,
    -- | The name of the global node group
    globalNodeGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalNodeGroup' with the minimum fields required to make a request.
--
-- * 'slots' - The keyspace for this node group
-- * 'globalNodeGroupId' - The name of the global node group
mkGlobalNodeGroup ::
  GlobalNodeGroup
mkGlobalNodeGroup =
  GlobalNodeGroup'
    { slots = Lude.Nothing,
      globalNodeGroupId = Lude.Nothing
    }

-- | The keyspace for this node group
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gngSlots :: Lens.Lens' GlobalNodeGroup (Lude.Maybe Lude.Text)
gngSlots = Lens.lens (slots :: GlobalNodeGroup -> Lude.Maybe Lude.Text) (\s a -> s {slots = a} :: GlobalNodeGroup)
{-# DEPRECATED gngSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The name of the global node group
--
-- /Note:/ Consider using 'globalNodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gngGlobalNodeGroupId :: Lens.Lens' GlobalNodeGroup (Lude.Maybe Lude.Text)
gngGlobalNodeGroupId = Lens.lens (globalNodeGroupId :: GlobalNodeGroup -> Lude.Maybe Lude.Text) (\s a -> s {globalNodeGroupId = a} :: GlobalNodeGroup)
{-# DEPRECATED gngGlobalNodeGroupId "Use generic-lens or generic-optics with 'globalNodeGroupId' instead." #-}

instance Lude.FromXML GlobalNodeGroup where
  parseXML x =
    GlobalNodeGroup'
      Lude.<$> (x Lude..@? "Slots") Lude.<*> (x Lude..@? "GlobalNodeGroupId")
