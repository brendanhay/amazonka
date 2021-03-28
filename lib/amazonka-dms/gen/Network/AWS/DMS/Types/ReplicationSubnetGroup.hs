{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationSubnetGroup
  ( ReplicationSubnetGroup (..)
  -- * Smart constructor
  , mkReplicationSubnetGroup
  -- * Lenses
  , rsgReplicationSubnetGroupDescription
  , rsgReplicationSubnetGroupIdentifier
  , rsgSubnetGroupStatus
  , rsgSubnets
  , rsgVpcId
  ) where

import qualified Network.AWS.DMS.Types.Subnet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a subnet group in response to a request by the @DescribeReplicationSubnetGroups@ operation.
--
-- /See:/ 'mkReplicationSubnetGroup' smart constructor.
data ReplicationSubnetGroup = ReplicationSubnetGroup'
  { replicationSubnetGroupDescription :: Core.Maybe Core.Text
    -- ^ A description for the replication subnet group.
  , replicationSubnetGroupIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the replication instance subnet group.
  , subnetGroupStatus :: Core.Maybe Core.Text
    -- ^ The status of the subnet group.
  , subnets :: Core.Maybe [Types.Subnet]
    -- ^ The subnets that are in the subnet group.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationSubnetGroup' value with any optional fields omitted.
mkReplicationSubnetGroup
    :: ReplicationSubnetGroup
mkReplicationSubnetGroup
  = ReplicationSubnetGroup'{replicationSubnetGroupDescription =
                              Core.Nothing,
                            replicationSubnetGroupIdentifier = Core.Nothing,
                            subnetGroupStatus = Core.Nothing, subnets = Core.Nothing,
                            vpcId = Core.Nothing}

-- | A description for the replication subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgReplicationSubnetGroupDescription :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
rsgReplicationSubnetGroupDescription = Lens.field @"replicationSubnetGroupDescription"
{-# INLINEABLE rsgReplicationSubnetGroupDescription #-}
{-# DEPRECATED replicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead"  #-}

-- | The identifier of the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgReplicationSubnetGroupIdentifier :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
rsgReplicationSubnetGroupIdentifier = Lens.field @"replicationSubnetGroupIdentifier"
{-# INLINEABLE rsgReplicationSubnetGroupIdentifier #-}
{-# DEPRECATED replicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead"  #-}

-- | The status of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgSubnetGroupStatus :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
rsgSubnetGroupStatus = Lens.field @"subnetGroupStatus"
{-# INLINEABLE rsgSubnetGroupStatus #-}
{-# DEPRECATED subnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead"  #-}

-- | The subnets that are in the subnet group.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgSubnets :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe [Types.Subnet])
rsgSubnets = Lens.field @"subnets"
{-# INLINEABLE rsgSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgVpcId :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
rsgVpcId = Lens.field @"vpcId"
{-# INLINEABLE rsgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON ReplicationSubnetGroup where
        parseJSON
          = Core.withObject "ReplicationSubnetGroup" Core.$
              \ x ->
                ReplicationSubnetGroup' Core.<$>
                  (x Core..:? "ReplicationSubnetGroupDescription") Core.<*>
                    x Core..:? "ReplicationSubnetGroupIdentifier"
                    Core.<*> x Core..:? "SubnetGroupStatus"
                    Core.<*> x Core..:? "Subnets"
                    Core.<*> x Core..:? "VpcId"
