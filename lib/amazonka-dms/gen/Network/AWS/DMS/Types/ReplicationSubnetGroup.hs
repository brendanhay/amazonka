{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationSubnetGroup
  ( ReplicationSubnetGroup (..),

    -- * Smart constructor
    mkReplicationSubnetGroup,

    -- * Lenses
    rsgVPCId,
    rsgSubnets,
    rsgReplicationSubnetGroupIdentifier,
    rsgSubnetGroupStatus,
    rsgReplicationSubnetGroupDescription,
  )
where

import Network.AWS.DMS.Types.Subnet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a subnet group in response to a request by the @DescribeReplicationSubnetGroups@ operation.
--
-- /See:/ 'mkReplicationSubnetGroup' smart constructor.
data ReplicationSubnetGroup = ReplicationSubnetGroup'
  { -- | The ID of the VPC.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The subnets that are in the subnet group.
    subnets :: Lude.Maybe [Subnet],
    -- | The identifier of the replication instance subnet group.
    replicationSubnetGroupIdentifier :: Lude.Maybe Lude.Text,
    -- | The status of the subnet group.
    subnetGroupStatus :: Lude.Maybe Lude.Text,
    -- | A description for the replication subnet group.
    replicationSubnetGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'subnets' - The subnets that are in the subnet group.
-- * 'replicationSubnetGroupIdentifier' - The identifier of the replication instance subnet group.
-- * 'subnetGroupStatus' - The status of the subnet group.
-- * 'replicationSubnetGroupDescription' - A description for the replication subnet group.
mkReplicationSubnetGroup ::
  ReplicationSubnetGroup
mkReplicationSubnetGroup =
  ReplicationSubnetGroup'
    { vpcId = Lude.Nothing,
      subnets = Lude.Nothing,
      replicationSubnetGroupIdentifier = Lude.Nothing,
      subnetGroupStatus = Lude.Nothing,
      replicationSubnetGroupDescription = Lude.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgVPCId :: Lens.Lens' ReplicationSubnetGroup (Lude.Maybe Lude.Text)
rsgVPCId = Lens.lens (vpcId :: ReplicationSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ReplicationSubnetGroup)
{-# DEPRECATED rsgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The subnets that are in the subnet group.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgSubnets :: Lens.Lens' ReplicationSubnetGroup (Lude.Maybe [Subnet])
rsgSubnets = Lens.lens (subnets :: ReplicationSubnetGroup -> Lude.Maybe [Subnet]) (\s a -> s {subnets = a} :: ReplicationSubnetGroup)
{-# DEPRECATED rsgSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The identifier of the replication instance subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgReplicationSubnetGroupIdentifier :: Lens.Lens' ReplicationSubnetGroup (Lude.Maybe Lude.Text)
rsgReplicationSubnetGroupIdentifier = Lens.lens (replicationSubnetGroupIdentifier :: ReplicationSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {replicationSubnetGroupIdentifier = a} :: ReplicationSubnetGroup)
{-# DEPRECATED rsgReplicationSubnetGroupIdentifier "Use generic-lens or generic-optics with 'replicationSubnetGroupIdentifier' instead." #-}

-- | The status of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgSubnetGroupStatus :: Lens.Lens' ReplicationSubnetGroup (Lude.Maybe Lude.Text)
rsgSubnetGroupStatus = Lens.lens (subnetGroupStatus :: ReplicationSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {subnetGroupStatus = a} :: ReplicationSubnetGroup)
{-# DEPRECATED rsgSubnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead." #-}

-- | A description for the replication subnet group.
--
-- /Note:/ Consider using 'replicationSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsgReplicationSubnetGroupDescription :: Lens.Lens' ReplicationSubnetGroup (Lude.Maybe Lude.Text)
rsgReplicationSubnetGroupDescription = Lens.lens (replicationSubnetGroupDescription :: ReplicationSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {replicationSubnetGroupDescription = a} :: ReplicationSubnetGroup)
{-# DEPRECATED rsgReplicationSubnetGroupDescription "Use generic-lens or generic-optics with 'replicationSubnetGroupDescription' instead." #-}

instance Lude.FromJSON ReplicationSubnetGroup where
  parseJSON =
    Lude.withObject
      "ReplicationSubnetGroup"
      ( \x ->
          ReplicationSubnetGroup'
            Lude.<$> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "Subnets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ReplicationSubnetGroupIdentifier")
            Lude.<*> (x Lude..:? "SubnetGroupStatus")
            Lude.<*> (x Lude..:? "ReplicationSubnetGroupDescription")
      )
