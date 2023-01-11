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
-- Module      : Amazonka.DMS.Types.ReplicationSubnetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationSubnetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.Subnet
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a subnet group in response to a request by the
-- @DescribeReplicationSubnetGroups@ operation.
--
-- /See:/ 'newReplicationSubnetGroup' smart constructor.
data ReplicationSubnetGroup = ReplicationSubnetGroup'
  { -- | A description for the replication subnet group.
    replicationSubnetGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the replication instance subnet group.
    replicationSubnetGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The status of the subnet group.
    subnetGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The subnets that are in the subnet group.
    subnets :: Prelude.Maybe [Subnet],
    -- | The IP addressing protocol supported by the subnet group. This is used
    -- by a replication instance with values such as IPv4 only or Dual-stack
    -- that supports both IPv4 and IPv6 addressing. IPv6 only is not yet
    -- supported.
    supportedNetworkTypes :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroupDescription', 'replicationSubnetGroup_replicationSubnetGroupDescription' - A description for the replication subnet group.
--
-- 'replicationSubnetGroupIdentifier', 'replicationSubnetGroup_replicationSubnetGroupIdentifier' - The identifier of the replication instance subnet group.
--
-- 'subnetGroupStatus', 'replicationSubnetGroup_subnetGroupStatus' - The status of the subnet group.
--
-- 'subnets', 'replicationSubnetGroup_subnets' - The subnets that are in the subnet group.
--
-- 'supportedNetworkTypes', 'replicationSubnetGroup_supportedNetworkTypes' - The IP addressing protocol supported by the subnet group. This is used
-- by a replication instance with values such as IPv4 only or Dual-stack
-- that supports both IPv4 and IPv6 addressing. IPv6 only is not yet
-- supported.
--
-- 'vpcId', 'replicationSubnetGroup_vpcId' - The ID of the VPC.
newReplicationSubnetGroup ::
  ReplicationSubnetGroup
newReplicationSubnetGroup =
  ReplicationSubnetGroup'
    { replicationSubnetGroupDescription =
        Prelude.Nothing,
      replicationSubnetGroupIdentifier = Prelude.Nothing,
      subnetGroupStatus = Prelude.Nothing,
      subnets = Prelude.Nothing,
      supportedNetworkTypes = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | A description for the replication subnet group.
replicationSubnetGroup_replicationSubnetGroupDescription :: Lens.Lens' ReplicationSubnetGroup (Prelude.Maybe Prelude.Text)
replicationSubnetGroup_replicationSubnetGroupDescription = Lens.lens (\ReplicationSubnetGroup' {replicationSubnetGroupDescription} -> replicationSubnetGroupDescription) (\s@ReplicationSubnetGroup' {} a -> s {replicationSubnetGroupDescription = a} :: ReplicationSubnetGroup)

-- | The identifier of the replication instance subnet group.
replicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' ReplicationSubnetGroup (Prelude.Maybe Prelude.Text)
replicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\ReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@ReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: ReplicationSubnetGroup)

-- | The status of the subnet group.
replicationSubnetGroup_subnetGroupStatus :: Lens.Lens' ReplicationSubnetGroup (Prelude.Maybe Prelude.Text)
replicationSubnetGroup_subnetGroupStatus = Lens.lens (\ReplicationSubnetGroup' {subnetGroupStatus} -> subnetGroupStatus) (\s@ReplicationSubnetGroup' {} a -> s {subnetGroupStatus = a} :: ReplicationSubnetGroup)

-- | The subnets that are in the subnet group.
replicationSubnetGroup_subnets :: Lens.Lens' ReplicationSubnetGroup (Prelude.Maybe [Subnet])
replicationSubnetGroup_subnets = Lens.lens (\ReplicationSubnetGroup' {subnets} -> subnets) (\s@ReplicationSubnetGroup' {} a -> s {subnets = a} :: ReplicationSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The IP addressing protocol supported by the subnet group. This is used
-- by a replication instance with values such as IPv4 only or Dual-stack
-- that supports both IPv4 and IPv6 addressing. IPv6 only is not yet
-- supported.
replicationSubnetGroup_supportedNetworkTypes :: Lens.Lens' ReplicationSubnetGroup (Prelude.Maybe [Prelude.Text])
replicationSubnetGroup_supportedNetworkTypes = Lens.lens (\ReplicationSubnetGroup' {supportedNetworkTypes} -> supportedNetworkTypes) (\s@ReplicationSubnetGroup' {} a -> s {supportedNetworkTypes = a} :: ReplicationSubnetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
replicationSubnetGroup_vpcId :: Lens.Lens' ReplicationSubnetGroup (Prelude.Maybe Prelude.Text)
replicationSubnetGroup_vpcId = Lens.lens (\ReplicationSubnetGroup' {vpcId} -> vpcId) (\s@ReplicationSubnetGroup' {} a -> s {vpcId = a} :: ReplicationSubnetGroup)

instance Data.FromJSON ReplicationSubnetGroup where
  parseJSON =
    Data.withObject
      "ReplicationSubnetGroup"
      ( \x ->
          ReplicationSubnetGroup'
            Prelude.<$> (x Data..:? "ReplicationSubnetGroupDescription")
            Prelude.<*> (x Data..:? "ReplicationSubnetGroupIdentifier")
            Prelude.<*> (x Data..:? "SubnetGroupStatus")
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "SupportedNetworkTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable ReplicationSubnetGroup where
  hashWithSalt _salt ReplicationSubnetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` replicationSubnetGroupDescription
      `Prelude.hashWithSalt` replicationSubnetGroupIdentifier
      `Prelude.hashWithSalt` subnetGroupStatus
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` supportedNetworkTypes
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData ReplicationSubnetGroup where
  rnf ReplicationSubnetGroup' {..} =
    Prelude.rnf replicationSubnetGroupDescription
      `Prelude.seq` Prelude.rnf replicationSubnetGroupIdentifier
      `Prelude.seq` Prelude.rnf subnetGroupStatus
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf supportedNetworkTypes
      `Prelude.seq` Prelude.rnf vpcId
