{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterNode
  ( ClusterNode (..),

    -- * Smart constructor
    mkClusterNode,

    -- * Lenses
    cnNodeRole,
    cnPrivateIPAddress,
    cnPublicIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.NodeRole as Types
import qualified Network.AWS.Redshift.Types.PrivateIPAddress as Types
import qualified Network.AWS.Redshift.Types.PublicIPAddress as Types

-- | The identifier of a node in a cluster.
--
-- /See:/ 'mkClusterNode' smart constructor.
data ClusterNode = ClusterNode'
  { -- | Whether the node is a leader node or a compute node.
    nodeRole :: Core.Maybe Types.NodeRole,
    -- | The private IP address of a node within a cluster.
    privateIPAddress :: Core.Maybe Types.PrivateIPAddress,
    -- | The public IP address of a node within a cluster.
    publicIPAddress :: Core.Maybe Types.PublicIPAddress
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterNode' value with any optional fields omitted.
mkClusterNode ::
  ClusterNode
mkClusterNode =
  ClusterNode'
    { nodeRole = Core.Nothing,
      privateIPAddress = Core.Nothing,
      publicIPAddress = Core.Nothing
    }

-- | Whether the node is a leader node or a compute node.
--
-- /Note:/ Consider using 'nodeRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnNodeRole :: Lens.Lens' ClusterNode (Core.Maybe Types.NodeRole)
cnNodeRole = Lens.field @"nodeRole"
{-# DEPRECATED cnNodeRole "Use generic-lens or generic-optics with 'nodeRole' instead." #-}

-- | The private IP address of a node within a cluster.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnPrivateIPAddress :: Lens.Lens' ClusterNode (Core.Maybe Types.PrivateIPAddress)
cnPrivateIPAddress = Lens.field @"privateIPAddress"
{-# DEPRECATED cnPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The public IP address of a node within a cluster.
--
-- /Note:/ Consider using 'publicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnPublicIPAddress :: Lens.Lens' ClusterNode (Core.Maybe Types.PublicIPAddress)
cnPublicIPAddress = Lens.field @"publicIPAddress"
{-# DEPRECATED cnPublicIPAddress "Use generic-lens or generic-optics with 'publicIPAddress' instead." #-}

instance Core.FromXML ClusterNode where
  parseXML x =
    ClusterNode'
      Core.<$> (x Core..@? "NodeRole")
      Core.<*> (x Core..@? "PrivateIPAddress")
      Core.<*> (x Core..@? "PublicIPAddress")
