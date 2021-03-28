{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterSubnetGroup
  ( ClusterSubnetGroup (..)
  -- * Smart constructor
  , mkClusterSubnetGroup
  -- * Lenses
  , csgClusterSubnetGroupName
  , csgDescription
  , csgSubnetGroupStatus
  , csgSubnets
  , csgTags
  , csgVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Subnet as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | Describes a subnet group.
--
-- /See:/ 'mkClusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { clusterSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cluster subnet group.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the cluster subnet group.
  , subnetGroupStatus :: Core.Maybe Core.Text
    -- ^ The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ . 
  , subnets :: Core.Maybe [Types.Subnet]
    -- ^ A list of the VPC 'Subnet' elements. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags for the cluster subnet group.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The VPC ID of the cluster subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterSubnetGroup' value with any optional fields omitted.
mkClusterSubnetGroup
    :: ClusterSubnetGroup
mkClusterSubnetGroup
  = ClusterSubnetGroup'{clusterSubnetGroupName = Core.Nothing,
                        description = Core.Nothing, subnetGroupStatus = Core.Nothing,
                        subnets = Core.Nothing, tags = Core.Nothing, vpcId = Core.Nothing}

-- | The name of the cluster subnet group.
--
-- /Note:/ Consider using 'clusterSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgClusterSubnetGroupName :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
csgClusterSubnetGroupName = Lens.field @"clusterSubnetGroupName"
{-# INLINEABLE csgClusterSubnetGroupName #-}
{-# DEPRECATED clusterSubnetGroupName "Use generic-lens or generic-optics with 'clusterSubnetGroupName' instead"  #-}

-- | The description of the cluster subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
csgDescription = Lens.field @"description"
{-# INLINEABLE csgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ . 
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnetGroupStatus :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
csgSubnetGroupStatus = Lens.field @"subnetGroupStatus"
{-# INLINEABLE csgSubnetGroupStatus #-}
{-# DEPRECATED subnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead"  #-}

-- | A list of the VPC 'Subnet' elements. 
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnets :: Lens.Lens' ClusterSubnetGroup (Core.Maybe [Types.Subnet])
csgSubnets = Lens.field @"subnets"
{-# INLINEABLE csgSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The list of tags for the cluster subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTags :: Lens.Lens' ClusterSubnetGroup (Core.Maybe [Types.Tag])
csgTags = Lens.field @"tags"
{-# INLINEABLE csgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The VPC ID of the cluster subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgVpcId :: Lens.Lens' ClusterSubnetGroup (Core.Maybe Core.Text)
csgVpcId = Lens.field @"vpcId"
{-# INLINEABLE csgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML ClusterSubnetGroup where
        parseXML x
          = ClusterSubnetGroup' Core.<$>
              (x Core..@? "ClusterSubnetGroupName") Core.<*>
                x Core..@? "Description"
                Core.<*> x Core..@? "SubnetGroupStatus"
                Core.<*> x Core..@? "Subnets" Core..<@> Core.parseXMLList "Subnet"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag"
                Core.<*> x Core..@? "VpcId"
