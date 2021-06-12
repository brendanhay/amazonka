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
-- Module      : Network.AWS.EMR.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Instance where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.EbsVolume
import Network.AWS.EMR.Types.InstanceStatus
import Network.AWS.EMR.Types.MarketType
import qualified Network.AWS.Lens as Lens

-- | Represents an EC2 instance provisioned as part of cluster.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The list of EBS volumes that are attached to this instance.
    ebsVolumes :: Core.Maybe [EbsVolume],
    -- | The current status of the instance.
    status :: Core.Maybe InstanceStatus,
    -- | The EC2 instance type, for example @m3.xlarge@.
    instanceType :: Core.Maybe Core.Text,
    -- | The identifier of the instance group to which this instance belongs.
    instanceGroupId :: Core.Maybe Core.Text,
    -- | The unique identifier for the instance in Amazon EMR.
    id :: Core.Maybe Core.Text,
    -- | The unique identifier of the instance fleet to which an EC2 instance
    -- belongs.
    instanceFleetId :: Core.Maybe Core.Text,
    -- | The public DNS name of the instance.
    publicDnsName :: Core.Maybe Core.Text,
    -- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@.
    market :: Core.Maybe MarketType,
    -- | The public IP address of the instance.
    publicIpAddress :: Core.Maybe Core.Text,
    -- | The private DNS name of the instance.
    privateDnsName :: Core.Maybe Core.Text,
    -- | The unique identifier of the instance in Amazon EC2.
    ec2InstanceId :: Core.Maybe Core.Text,
    -- | The private IP address of the instance.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsVolumes', 'instance_ebsVolumes' - The list of EBS volumes that are attached to this instance.
--
-- 'status', 'instance_status' - The current status of the instance.
--
-- 'instanceType', 'instance_instanceType' - The EC2 instance type, for example @m3.xlarge@.
--
-- 'instanceGroupId', 'instance_instanceGroupId' - The identifier of the instance group to which this instance belongs.
--
-- 'id', 'instance_id' - The unique identifier for the instance in Amazon EMR.
--
-- 'instanceFleetId', 'instance_instanceFleetId' - The unique identifier of the instance fleet to which an EC2 instance
-- belongs.
--
-- 'publicDnsName', 'instance_publicDnsName' - The public DNS name of the instance.
--
-- 'market', 'instance_market' - The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IP address of the instance.
--
-- 'privateDnsName', 'instance_privateDnsName' - The private DNS name of the instance.
--
-- 'ec2InstanceId', 'instance_ec2InstanceId' - The unique identifier of the instance in Amazon EC2.
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IP address of the instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { ebsVolumes = Core.Nothing,
      status = Core.Nothing,
      instanceType = Core.Nothing,
      instanceGroupId = Core.Nothing,
      id = Core.Nothing,
      instanceFleetId = Core.Nothing,
      publicDnsName = Core.Nothing,
      market = Core.Nothing,
      publicIpAddress = Core.Nothing,
      privateDnsName = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | The list of EBS volumes that are attached to this instance.
instance_ebsVolumes :: Lens.Lens' Instance (Core.Maybe [EbsVolume])
instance_ebsVolumes = Lens.lens (\Instance' {ebsVolumes} -> ebsVolumes) (\s@Instance' {} a -> s {ebsVolumes = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | The current status of the instance.
instance_status :: Lens.Lens' Instance (Core.Maybe InstanceStatus)
instance_status = Lens.lens (\Instance' {status} -> status) (\s@Instance' {} a -> s {status = a} :: Instance)

-- | The EC2 instance type, for example @m3.xlarge@.
instance_instanceType :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_instanceType = Lens.lens (\Instance' {instanceType} -> instanceType) (\s@Instance' {} a -> s {instanceType = a} :: Instance)

-- | The identifier of the instance group to which this instance belongs.
instance_instanceGroupId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_instanceGroupId = Lens.lens (\Instance' {instanceGroupId} -> instanceGroupId) (\s@Instance' {} a -> s {instanceGroupId = a} :: Instance)

-- | The unique identifier for the instance in Amazon EMR.
instance_id :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_id = Lens.lens (\Instance' {id} -> id) (\s@Instance' {} a -> s {id = a} :: Instance)

-- | The unique identifier of the instance fleet to which an EC2 instance
-- belongs.
instance_instanceFleetId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_instanceFleetId = Lens.lens (\Instance' {instanceFleetId} -> instanceFleetId) (\s@Instance' {} a -> s {instanceFleetId = a} :: Instance)

-- | The public DNS name of the instance.
instance_publicDnsName :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_publicDnsName = Lens.lens (\Instance' {publicDnsName} -> publicDnsName) (\s@Instance' {} a -> s {publicDnsName = a} :: Instance)

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@.
instance_market :: Lens.Lens' Instance (Core.Maybe MarketType)
instance_market = Lens.lens (\Instance' {market} -> market) (\s@Instance' {} a -> s {market = a} :: Instance)

-- | The public IP address of the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The private DNS name of the instance.
instance_privateDnsName :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_privateDnsName = Lens.lens (\Instance' {privateDnsName} -> privateDnsName) (\s@Instance' {} a -> s {privateDnsName = a} :: Instance)

-- | The unique identifier of the instance in Amazon EC2.
instance_ec2InstanceId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_ec2InstanceId = Lens.lens (\Instance' {ec2InstanceId} -> ec2InstanceId) (\s@Instance' {} a -> s {ec2InstanceId = a} :: Instance)

-- | The private IP address of the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Core.<$> (x Core..:? "EbsVolumes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "InstanceGroupId")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "InstanceFleetId")
            Core.<*> (x Core..:? "PublicDnsName")
            Core.<*> (x Core..:? "Market")
            Core.<*> (x Core..:? "PublicIpAddress")
            Core.<*> (x Core..:? "PrivateDnsName")
            Core.<*> (x Core..:? "Ec2InstanceId")
            Core.<*> (x Core..:? "PrivateIpAddress")
      )

instance Core.Hashable Instance

instance Core.NFData Instance
