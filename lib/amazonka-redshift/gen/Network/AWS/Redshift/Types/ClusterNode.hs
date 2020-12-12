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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | The identifier of a node in a cluster.
--
-- /See:/ 'mkClusterNode' smart constructor.
data ClusterNode = ClusterNode'
  { nodeRole :: Lude.Maybe Lude.Text,
    privateIPAddress :: Lude.Maybe Lude.Text,
    publicIPAddress :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterNode' with the minimum fields required to make a request.
--
-- * 'nodeRole' - Whether the node is a leader node or a compute node.
-- * 'privateIPAddress' - The private IP address of a node within a cluster.
-- * 'publicIPAddress' - The public IP address of a node within a cluster.
mkClusterNode ::
  ClusterNode
mkClusterNode =
  ClusterNode'
    { nodeRole = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      publicIPAddress = Lude.Nothing
    }

-- | Whether the node is a leader node or a compute node.
--
-- /Note:/ Consider using 'nodeRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnNodeRole :: Lens.Lens' ClusterNode (Lude.Maybe Lude.Text)
cnNodeRole = Lens.lens (nodeRole :: ClusterNode -> Lude.Maybe Lude.Text) (\s a -> s {nodeRole = a} :: ClusterNode)
{-# DEPRECATED cnNodeRole "Use generic-lens or generic-optics with 'nodeRole' instead." #-}

-- | The private IP address of a node within a cluster.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnPrivateIPAddress :: Lens.Lens' ClusterNode (Lude.Maybe Lude.Text)
cnPrivateIPAddress = Lens.lens (privateIPAddress :: ClusterNode -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: ClusterNode)
{-# DEPRECATED cnPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The public IP address of a node within a cluster.
--
-- /Note:/ Consider using 'publicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnPublicIPAddress :: Lens.Lens' ClusterNode (Lude.Maybe Lude.Text)
cnPublicIPAddress = Lens.lens (publicIPAddress :: ClusterNode -> Lude.Maybe Lude.Text) (\s a -> s {publicIPAddress = a} :: ClusterNode)
{-# DEPRECATED cnPublicIPAddress "Use generic-lens or generic-optics with 'publicIPAddress' instead." #-}

instance Lude.FromXML ClusterNode where
  parseXML x =
    ClusterNode'
      Lude.<$> (x Lude..@? "NodeRole")
      Lude.<*> (x Lude..@? "PrivateIPAddress")
      Lude.<*> (x Lude..@? "PublicIPAddress")
