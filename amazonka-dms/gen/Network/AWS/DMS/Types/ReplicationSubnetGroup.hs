{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationSubnetGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.Subnet
import qualified Network.AWS.Lens as Lens

-- | Describes a subnet group in response to a request by the
-- @DescribeReplicationSubnetGroups@ operation.
--
-- /See:/ 'newReplicationSubnetGroup' smart constructor.
data ReplicationSubnetGroup = ReplicationSubnetGroup'
  { -- | The identifier of the replication instance subnet group.
    replicationSubnetGroupIdentifier :: Core.Maybe Core.Text,
    -- | The status of the subnet group.
    subnetGroupStatus :: Core.Maybe Core.Text,
    -- | A description for the replication subnet group.
    replicationSubnetGroupDescription :: Core.Maybe Core.Text,
    -- | The subnets that are in the subnet group.
    subnets :: Core.Maybe [Subnet],
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroupIdentifier', 'replicationSubnetGroup_replicationSubnetGroupIdentifier' - The identifier of the replication instance subnet group.
--
-- 'subnetGroupStatus', 'replicationSubnetGroup_subnetGroupStatus' - The status of the subnet group.
--
-- 'replicationSubnetGroupDescription', 'replicationSubnetGroup_replicationSubnetGroupDescription' - A description for the replication subnet group.
--
-- 'subnets', 'replicationSubnetGroup_subnets' - The subnets that are in the subnet group.
--
-- 'vpcId', 'replicationSubnetGroup_vpcId' - The ID of the VPC.
newReplicationSubnetGroup ::
  ReplicationSubnetGroup
newReplicationSubnetGroup =
  ReplicationSubnetGroup'
    { replicationSubnetGroupIdentifier =
        Core.Nothing,
      subnetGroupStatus = Core.Nothing,
      replicationSubnetGroupDescription = Core.Nothing,
      subnets = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The identifier of the replication instance subnet group.
replicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
replicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\ReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@ReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: ReplicationSubnetGroup)

-- | The status of the subnet group.
replicationSubnetGroup_subnetGroupStatus :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
replicationSubnetGroup_subnetGroupStatus = Lens.lens (\ReplicationSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@ReplicationSubnetGroup' {} a -> s {subnetGroupStatus = a} :: ReplicationSubnetGroup)

-- | A description for the replication subnet group.
replicationSubnetGroup_replicationSubnetGroupDescription :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
replicationSubnetGroup_replicationSubnetGroupDescription = Lens.lens (\ReplicationSubnetGroup' {replicationSubnetGroupDescription} -> replicationSubnetGroupDescription) (\s@ReplicationSubnetGroup' {} a -> s {replicationSubnetGroupDescription = a} :: ReplicationSubnetGroup)

-- | The subnets that are in the subnet group.
replicationSubnetGroup_subnets :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe [Subnet])
replicationSubnetGroup_subnets = Lens.lens (\ReplicationSubnetGroup' {subnets} -> subnets) (\s@ReplicationSubnetGroup' {} a -> s {subnets = a} :: ReplicationSubnetGroup) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC.
replicationSubnetGroup_vpcId :: Lens.Lens' ReplicationSubnetGroup (Core.Maybe Core.Text)
replicationSubnetGroup_vpcId = Lens.lens (\ReplicationSubnetGroup' {vpcId} -> vpcId) (\s@ReplicationSubnetGroup' {} a -> s {vpcId = a} :: ReplicationSubnetGroup)

instance Core.FromJSON ReplicationSubnetGroup where
  parseJSON =
    Core.withObject
      "ReplicationSubnetGroup"
      ( \x ->
          ReplicationSubnetGroup'
            Core.<$> (x Core..:? "ReplicationSubnetGroupIdentifier")
            Core.<*> (x Core..:? "SubnetGroupStatus")
            Core.<*> (x Core..:? "ReplicationSubnetGroupDescription")
            Core.<*> (x Core..:? "Subnets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VpcId")
      )

instance Core.Hashable ReplicationSubnetGroup

instance Core.NFData ReplicationSubnetGroup
