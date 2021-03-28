{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OrderableClusterOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.OrderableClusterOption
  ( OrderableClusterOption (..)
  -- * Smart constructor
  , mkOrderableClusterOption
  -- * Lenses
  , ocoAvailabilityZones
  , ocoClusterType
  , ocoClusterVersion
  , ocoNodeType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.AvailabilityZone as Types

-- | Describes an orderable cluster option.
--
-- /See:/ 'mkOrderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { availabilityZones :: Core.Maybe [Types.AvailabilityZone]
    -- ^ A list of availability zones for the orderable cluster.
  , clusterType :: Core.Maybe Core.Text
    -- ^ The cluster type, for example @multi-node@ . 
  , clusterVersion :: Core.Maybe Core.Text
    -- ^ The version of the orderable cluster.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type for the orderable cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderableClusterOption' value with any optional fields omitted.
mkOrderableClusterOption
    :: OrderableClusterOption
mkOrderableClusterOption
  = OrderableClusterOption'{availabilityZones = Core.Nothing,
                            clusterType = Core.Nothing, clusterVersion = Core.Nothing,
                            nodeType = Core.Nothing}

-- | A list of availability zones for the orderable cluster.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoAvailabilityZones :: Lens.Lens' OrderableClusterOption (Core.Maybe [Types.AvailabilityZone])
ocoAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE ocoAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The cluster type, for example @multi-node@ . 
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoClusterType :: Lens.Lens' OrderableClusterOption (Core.Maybe Core.Text)
ocoClusterType = Lens.field @"clusterType"
{-# INLINEABLE ocoClusterType #-}
{-# DEPRECATED clusterType "Use generic-lens or generic-optics with 'clusterType' instead"  #-}

-- | The version of the orderable cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoClusterVersion :: Lens.Lens' OrderableClusterOption (Core.Maybe Core.Text)
ocoClusterVersion = Lens.field @"clusterVersion"
{-# INLINEABLE ocoClusterVersion #-}
{-# DEPRECATED clusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead"  #-}

-- | The node type for the orderable cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoNodeType :: Lens.Lens' OrderableClusterOption (Core.Maybe Core.Text)
ocoNodeType = Lens.field @"nodeType"
{-# INLINEABLE ocoNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

instance Core.FromXML OrderableClusterOption where
        parseXML x
          = OrderableClusterOption' Core.<$>
              (x Core..@? "AvailabilityZones" Core..<@>
                 Core.parseXMLList "AvailabilityZone")
                Core.<*> x Core..@? "ClusterType"
                Core.<*> x Core..@? "ClusterVersion"
                Core.<*> x Core..@? "NodeType"
