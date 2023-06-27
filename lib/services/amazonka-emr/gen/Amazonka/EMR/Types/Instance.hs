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
-- Module      : Amazonka.EMR.Types.Instance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.EbsVolume
import Amazonka.EMR.Types.InstanceStatus
import Amazonka.EMR.Types.MarketType
import qualified Amazonka.Prelude as Prelude

-- | Represents an Amazon EC2 instance provisioned as part of cluster.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The list of Amazon EBS volumes that are attached to this instance.
    ebsVolumes :: Prelude.Maybe [EbsVolume],
    -- | The unique identifier of the instance in Amazon EC2.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the instance in Amazon EMR.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the instance fleet to which an Amazon EC2
    -- instance belongs.
    instanceFleetId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the instance group to which this instance belongs.
    instanceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 instance type, for example @m3.xlarge@.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@.
    market :: Prelude.Maybe MarketType,
    -- | The private DNS name of the instance.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name of the instance.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of the instance.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The current status of the instance.
    status :: Prelude.Maybe InstanceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsVolumes', 'instance_ebsVolumes' - The list of Amazon EBS volumes that are attached to this instance.
--
-- 'ec2InstanceId', 'instance_ec2InstanceId' - The unique identifier of the instance in Amazon EC2.
--
-- 'id', 'instance_id' - The unique identifier for the instance in Amazon EMR.
--
-- 'instanceFleetId', 'instance_instanceFleetId' - The unique identifier of the instance fleet to which an Amazon EC2
-- instance belongs.
--
-- 'instanceGroupId', 'instance_instanceGroupId' - The identifier of the instance group to which this instance belongs.
--
-- 'instanceType', 'instance_instanceType' - The Amazon EC2 instance type, for example @m3.xlarge@.
--
-- 'market', 'instance_market' - The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@.
--
-- 'privateDnsName', 'instance_privateDnsName' - The private DNS name of the instance.
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IP address of the instance.
--
-- 'publicDnsName', 'instance_publicDnsName' - The public DNS name of the instance.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IP address of the instance.
--
-- 'status', 'instance_status' - The current status of the instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { ebsVolumes = Prelude.Nothing,
      ec2InstanceId = Prelude.Nothing,
      id = Prelude.Nothing,
      instanceFleetId = Prelude.Nothing,
      instanceGroupId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      market = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The list of Amazon EBS volumes that are attached to this instance.
instance_ebsVolumes :: Lens.Lens' Instance (Prelude.Maybe [EbsVolume])
instance_ebsVolumes = Lens.lens (\Instance' {ebsVolumes} -> ebsVolumes) (\s@Instance' {} a -> s {ebsVolumes = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the instance in Amazon EC2.
instance_ec2InstanceId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_ec2InstanceId = Lens.lens (\Instance' {ec2InstanceId} -> ec2InstanceId) (\s@Instance' {} a -> s {ec2InstanceId = a} :: Instance)

-- | The unique identifier for the instance in Amazon EMR.
instance_id :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_id = Lens.lens (\Instance' {id} -> id) (\s@Instance' {} a -> s {id = a} :: Instance)

-- | The unique identifier of the instance fleet to which an Amazon EC2
-- instance belongs.
instance_instanceFleetId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceFleetId = Lens.lens (\Instance' {instanceFleetId} -> instanceFleetId) (\s@Instance' {} a -> s {instanceFleetId = a} :: Instance)

-- | The identifier of the instance group to which this instance belongs.
instance_instanceGroupId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceGroupId = Lens.lens (\Instance' {instanceGroupId} -> instanceGroupId) (\s@Instance' {} a -> s {instanceGroupId = a} :: Instance)

-- | The Amazon EC2 instance type, for example @m3.xlarge@.
instance_instanceType :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@.
instance_market :: Lens.Lens' Instance (Prelude.Maybe MarketType)
instance_market = Lens.lens (\Instance' {market} -> market) (\s@Instance' {} a -> s {market = a} :: Instance)

-- | The private DNS name of the instance.
instance_privateDnsName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateDnsName = Lens.lens (\Instance' {privateDnsName} -> privateDnsName) (\s@Instance' {} a -> s {privateDnsName = a} :: Instance)

-- | The private IP address of the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

-- | The public DNS name of the instance.
instance_publicDnsName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicDnsName = Lens.lens (\Instance' {publicDnsName} -> publicDnsName) (\s@Instance' {} a -> s {publicDnsName = a} :: Instance)

-- | The public IP address of the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The current status of the instance.
instance_status :: Lens.Lens' Instance (Prelude.Maybe InstanceStatus)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "EbsVolumes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Ec2InstanceId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InstanceFleetId")
            Prelude.<*> (x Data..:? "InstanceGroupId")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "Market")
            Prelude.<*> (x Data..:? "PrivateDnsName")
            Prelude.<*> (x Data..:? "PrivateIpAddress")
            Prelude.<*> (x Data..:? "PublicDnsName")
            Prelude.<*> (x Data..:? "PublicIpAddress")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt
      `Prelude.hashWithSalt` ebsVolumes
      `Prelude.hashWithSalt` ec2InstanceId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instanceFleetId
      `Prelude.hashWithSalt` instanceGroupId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` market
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` publicIpAddress
      `Prelude.hashWithSalt` status

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf ebsVolumes
      `Prelude.seq` Prelude.rnf ec2InstanceId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceFleetId
      `Prelude.seq` Prelude.rnf instanceGroupId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf market
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf publicDnsName
      `Prelude.seq` Prelude.rnf publicIpAddress
      `Prelude.seq` Prelude.rnf status
