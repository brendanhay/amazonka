{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ngusNodeGroupId,
    ngusNodeGroupMemberUpdateStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the service update on the node group
--
-- /See:/ 'mkNodeGroupUpdateStatus' smart constructor.
data NodeGroupUpdateStatus = NodeGroupUpdateStatus'
  { -- | The ID of the node group
    nodeGroupId :: Core.Maybe Types.String,
    -- | The status of the service update on the node group member
    nodeGroupMemberUpdateStatus :: Core.Maybe [Types.NodeGroupMemberUpdateStatus]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NodeGroupUpdateStatus' value with any optional fields omitted.
mkNodeGroupUpdateStatus ::
  NodeGroupUpdateStatus
mkNodeGroupUpdateStatus =
  NodeGroupUpdateStatus'
    { nodeGroupId = Core.Nothing,
      nodeGroupMemberUpdateStatus = Core.Nothing
    }

-- | The ID of the node group
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngusNodeGroupId :: Lens.Lens' NodeGroupUpdateStatus (Core.Maybe Types.String)
ngusNodeGroupId = Lens.field @"nodeGroupId"
{-# DEPRECATED ngusNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The status of the service update on the node group member
--
-- /Note:/ Consider using 'nodeGroupMemberUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngusNodeGroupMemberUpdateStatus :: Lens.Lens' NodeGroupUpdateStatus (Core.Maybe [Types.NodeGroupMemberUpdateStatus])
ngusNodeGroupMemberUpdateStatus = Lens.field @"nodeGroupMemberUpdateStatus"
{-# DEPRECATED ngusNodeGroupMemberUpdateStatus "Use generic-lens or generic-optics with 'nodeGroupMemberUpdateStatus' instead." #-}

instance Core.FromXML NodeGroupUpdateStatus where
  parseXML x =
    NodeGroupUpdateStatus'
      Core.<$> (x Core..@? "NodeGroupId")
      Core.<*> ( x Core..@? "NodeGroupMemberUpdateStatus"
                   Core..<@> Core.parseXMLList "NodeGroupMemberUpdateStatus"
               )
