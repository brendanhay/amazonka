{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ResizeClusterMessage
  ( ResizeClusterMessage (..)
  -- * Smart constructor
  , mkResizeClusterMessage
  -- * Lenses
  , rcmClusterIdentifier
  , rcmClassic
  , rcmClusterType
  , rcmNodeType
  , rcmNumberOfNodes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes a resize cluster operation. For example, a scheduled action to run the @ResizeCluster@ API operation. 
--
-- /See:/ 'mkResizeClusterMessage' smart constructor.
data ResizeClusterMessage = ResizeClusterMessage'
  { clusterIdentifier :: Core.Text
    -- ^ The unique identifier for the cluster to resize.
  , classic :: Core.Maybe Core.Bool
    -- ^ A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic. 
  , clusterType :: Core.Maybe Core.Text
    -- ^ The new cluster type for the specified cluster.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResizeClusterMessage' value with any optional fields omitted.
mkResizeClusterMessage
    :: Core.Text -- ^ 'clusterIdentifier'
    -> ResizeClusterMessage
mkResizeClusterMessage clusterIdentifier
  = ResizeClusterMessage'{clusterIdentifier, classic = Core.Nothing,
                          clusterType = Core.Nothing, nodeType = Core.Nothing,
                          numberOfNodes = Core.Nothing}

-- | The unique identifier for the cluster to resize.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmClusterIdentifier :: Lens.Lens' ResizeClusterMessage Core.Text
rcmClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rcmClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic. 
--
-- /Note:/ Consider using 'classic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmClassic :: Lens.Lens' ResizeClusterMessage (Core.Maybe Core.Bool)
rcmClassic = Lens.field @"classic"
{-# INLINEABLE rcmClassic #-}
{-# DEPRECATED classic "Use generic-lens or generic-optics with 'classic' instead"  #-}

-- | The new cluster type for the specified cluster.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmClusterType :: Lens.Lens' ResizeClusterMessage (Core.Maybe Core.Text)
rcmClusterType = Lens.field @"clusterType"
{-# INLINEABLE rcmClusterType #-}
{-# DEPRECATED clusterType "Use generic-lens or generic-optics with 'clusterType' instead"  #-}

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmNodeType :: Lens.Lens' ResizeClusterMessage (Core.Maybe Core.Text)
rcmNodeType = Lens.field @"nodeType"
{-# INLINEABLE rcmNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmNumberOfNodes :: Lens.Lens' ResizeClusterMessage (Core.Maybe Core.Int)
rcmNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE rcmNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

instance Core.ToQuery ResizeClusterMessage where
        toQuery ResizeClusterMessage{..}
          = Core.toQueryPair "ClusterIdentifier" clusterIdentifier Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Classic") classic
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClusterType") clusterType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NodeType") nodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumberOfNodes")
                numberOfNodes

instance Core.FromXML ResizeClusterMessage where
        parseXML x
          = ResizeClusterMessage' Core.<$>
              (x Core..@ "ClusterIdentifier") Core.<*> x Core..@? "Classic"
                Core.<*> x Core..@? "ClusterType"
                Core.<*> x Core..@? "NodeType"
                Core.<*> x Core..@? "NumberOfNodes"
