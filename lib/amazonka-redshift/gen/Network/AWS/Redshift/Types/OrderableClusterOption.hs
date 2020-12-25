{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OrderableClusterOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OrderableClusterOption
  ( OrderableClusterOption (..),

    -- * Smart constructor
    mkOrderableClusterOption,

    -- * Lenses
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.AvailabilityZone as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes an orderable cluster option.
--
-- /See:/ 'mkOrderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { -- | A list of availability zones for the orderable cluster.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | The cluster type, for example @multi-node@ .
    clusterType :: Core.Maybe Types.String,
    -- | The version of the orderable cluster.
    clusterVersion :: Core.Maybe Types.String,
    -- | The node type for the orderable cluster.
    nodeType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderableClusterOption' value with any optional fields omitted.
mkOrderableClusterOption ::
  OrderableClusterOption
mkOrderableClusterOption =
  OrderableClusterOption'
    { availabilityZones = Core.Nothing,
      clusterType = Core.Nothing,
      clusterVersion = Core.Nothing,
      nodeType = Core.Nothing
    }

-- | A list of availability zones for the orderable cluster.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoAvailabilityZones :: Lens.Lens' OrderableClusterOption (Core.Maybe [Types.AvailabilityZone])
ocoAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED ocoAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The cluster type, for example @multi-node@ .
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoClusterType :: Lens.Lens' OrderableClusterOption (Core.Maybe Types.String)
ocoClusterType = Lens.field @"clusterType"
{-# DEPRECATED ocoClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The version of the orderable cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoClusterVersion :: Lens.Lens' OrderableClusterOption (Core.Maybe Types.String)
ocoClusterVersion = Lens.field @"clusterVersion"
{-# DEPRECATED ocoClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The node type for the orderable cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoNodeType :: Lens.Lens' OrderableClusterOption (Core.Maybe Types.String)
ocoNodeType = Lens.field @"nodeType"
{-# DEPRECATED ocoNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Core.FromXML OrderableClusterOption where
  parseXML x =
    OrderableClusterOption'
      Core.<$> ( x Core..@? "AvailabilityZones"
                   Core..<@> Core.parseXMLList "AvailabilityZone"
               )
      Core.<*> (x Core..@? "ClusterType")
      Core.<*> (x Core..@? "ClusterVersion")
      Core.<*> (x Core..@? "NodeType")
