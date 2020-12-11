-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
  ( NodeGroupUpdateStatus (..),

    -- * Smart constructor
    mkNodeGroupUpdateStatus,

    -- * Lenses
    ngusNodeGroupMemberUpdateStatus,
    ngusNodeGroupId,
  )
where

import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the service update on the node group
--
-- /See:/ 'mkNodeGroupUpdateStatus' smart constructor.
data NodeGroupUpdateStatus = NodeGroupUpdateStatus'
  { nodeGroupMemberUpdateStatus ::
      Lude.Maybe [NodeGroupMemberUpdateStatus],
    nodeGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeGroupUpdateStatus' with the minimum fields required to make a request.
--
-- * 'nodeGroupId' - The ID of the node group
-- * 'nodeGroupMemberUpdateStatus' - The status of the service update on the node group member
mkNodeGroupUpdateStatus ::
  NodeGroupUpdateStatus
mkNodeGroupUpdateStatus =
  NodeGroupUpdateStatus'
    { nodeGroupMemberUpdateStatus =
        Lude.Nothing,
      nodeGroupId = Lude.Nothing
    }

-- | The status of the service update on the node group member
--
-- /Note:/ Consider using 'nodeGroupMemberUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngusNodeGroupMemberUpdateStatus :: Lens.Lens' NodeGroupUpdateStatus (Lude.Maybe [NodeGroupMemberUpdateStatus])
ngusNodeGroupMemberUpdateStatus = Lens.lens (nodeGroupMemberUpdateStatus :: NodeGroupUpdateStatus -> Lude.Maybe [NodeGroupMemberUpdateStatus]) (\s a -> s {nodeGroupMemberUpdateStatus = a} :: NodeGroupUpdateStatus)
{-# DEPRECATED ngusNodeGroupMemberUpdateStatus "Use generic-lens or generic-optics with 'nodeGroupMemberUpdateStatus' instead." #-}

-- | The ID of the node group
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngusNodeGroupId :: Lens.Lens' NodeGroupUpdateStatus (Lude.Maybe Lude.Text)
ngusNodeGroupId = Lens.lens (nodeGroupId :: NodeGroupUpdateStatus -> Lude.Maybe Lude.Text) (\s a -> s {nodeGroupId = a} :: NodeGroupUpdateStatus)
{-# DEPRECATED ngusNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

instance Lude.FromXML NodeGroupUpdateStatus where
  parseXML x =
    NodeGroupUpdateStatus'
      Lude.<$> ( x Lude..@? "NodeGroupMemberUpdateStatus" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "NodeGroupMemberUpdateStatus")
               )
      Lude.<*> (x Lude..@? "NodeGroupId")
