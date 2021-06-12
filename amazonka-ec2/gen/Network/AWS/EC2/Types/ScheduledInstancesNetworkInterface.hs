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
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstancesIpv6Address
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import qualified Network.AWS.Lens as Lens

-- | Describes a network interface for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { -- | The IDs of the security groups.
    groups :: Core.Maybe [Core.Text],
    -- | The specific IPv6 addresses from the subnet range.
    ipv6Addresses :: Core.Maybe [ScheduledInstancesIpv6Address],
    -- | Indicates whether to assign a public IPv4 address to instances launched
    -- in a VPC. The public IPv4 address can only be assigned to a network
    -- interface for eth0, and can only be assigned to a new network interface,
    -- not an existing one. You cannot specify more than one network interface
    -- in the request. If launching into a default subnet, the default value is
    -- @true@.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | The number of IPv6 addresses to assign to the network interface. The
    -- IPv6 addresses are automatically selected from the subnet range.
    ipv6AddressCount :: Core.Maybe Core.Int,
    -- | Indicates whether to delete the interface when the instance is
    -- terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | The description.
    description :: Core.Maybe Core.Text,
    -- | The index of the device for the network interface attachment.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The number of secondary private IPv4 addresses.
    secondaryPrivateIpAddressCount :: Core.Maybe Core.Int,
    -- | The private IPv4 addresses.
    privateIpAddressConfigs :: Core.Maybe [ScheduledInstancesPrivateIpAddressConfig],
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledInstancesNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'scheduledInstancesNetworkInterface_groups' - The IDs of the security groups.
--
-- 'ipv6Addresses', 'scheduledInstancesNetworkInterface_ipv6Addresses' - The specific IPv6 addresses from the subnet range.
--
-- 'associatePublicIpAddress', 'scheduledInstancesNetworkInterface_associatePublicIpAddress' - Indicates whether to assign a public IPv4 address to instances launched
-- in a VPC. The public IPv4 address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
--
-- 'ipv6AddressCount', 'scheduledInstancesNetworkInterface_ipv6AddressCount' - The number of IPv6 addresses to assign to the network interface. The
-- IPv6 addresses are automatically selected from the subnet range.
--
-- 'deleteOnTermination', 'scheduledInstancesNetworkInterface_deleteOnTermination' - Indicates whether to delete the interface when the instance is
-- terminated.
--
-- 'networkInterfaceId', 'scheduledInstancesNetworkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'scheduledInstancesNetworkInterface_subnetId' - The ID of the subnet.
--
-- 'description', 'scheduledInstancesNetworkInterface_description' - The description.
--
-- 'deviceIndex', 'scheduledInstancesNetworkInterface_deviceIndex' - The index of the device for the network interface attachment.
--
-- 'secondaryPrivateIpAddressCount', 'scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses.
--
-- 'privateIpAddressConfigs', 'scheduledInstancesNetworkInterface_privateIpAddressConfigs' - The private IPv4 addresses.
--
-- 'privateIpAddress', 'scheduledInstancesNetworkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
newScheduledInstancesNetworkInterface ::
  ScheduledInstancesNetworkInterface
newScheduledInstancesNetworkInterface =
  ScheduledInstancesNetworkInterface'
    { groups =
        Core.Nothing,
      ipv6Addresses = Core.Nothing,
      associatePublicIpAddress = Core.Nothing,
      ipv6AddressCount = Core.Nothing,
      deleteOnTermination = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      subnetId = Core.Nothing,
      description = Core.Nothing,
      deviceIndex = Core.Nothing,
      secondaryPrivateIpAddressCount =
        Core.Nothing,
      privateIpAddressConfigs = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | The IDs of the security groups.
scheduledInstancesNetworkInterface_groups :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe [Core.Text])
scheduledInstancesNetworkInterface_groups = Lens.lens (\ScheduledInstancesNetworkInterface' {groups} -> groups) (\s@ScheduledInstancesNetworkInterface' {} a -> s {groups = a} :: ScheduledInstancesNetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The specific IPv6 addresses from the subnet range.
scheduledInstancesNetworkInterface_ipv6Addresses :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe [ScheduledInstancesIpv6Address])
scheduledInstancesNetworkInterface_ipv6Addresses = Lens.lens (\ScheduledInstancesNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@ScheduledInstancesNetworkInterface' {} a -> s {ipv6Addresses = a} :: ScheduledInstancesNetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether to assign a public IPv4 address to instances launched
-- in a VPC. The public IPv4 address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
scheduledInstancesNetworkInterface_associatePublicIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Bool)
scheduledInstancesNetworkInterface_associatePublicIpAddress = Lens.lens (\ScheduledInstancesNetworkInterface' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ScheduledInstancesNetworkInterface' {} a -> s {associatePublicIpAddress = a} :: ScheduledInstancesNetworkInterface)

-- | The number of IPv6 addresses to assign to the network interface. The
-- IPv6 addresses are automatically selected from the subnet range.
scheduledInstancesNetworkInterface_ipv6AddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Int)
scheduledInstancesNetworkInterface_ipv6AddressCount = Lens.lens (\ScheduledInstancesNetworkInterface' {ipv6AddressCount} -> ipv6AddressCount) (\s@ScheduledInstancesNetworkInterface' {} a -> s {ipv6AddressCount = a} :: ScheduledInstancesNetworkInterface)

-- | Indicates whether to delete the interface when the instance is
-- terminated.
scheduledInstancesNetworkInterface_deleteOnTermination :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Bool)
scheduledInstancesNetworkInterface_deleteOnTermination = Lens.lens (\ScheduledInstancesNetworkInterface' {deleteOnTermination} -> deleteOnTermination) (\s@ScheduledInstancesNetworkInterface' {} a -> s {deleteOnTermination = a} :: ScheduledInstancesNetworkInterface)

-- | The ID of the network interface.
scheduledInstancesNetworkInterface_networkInterfaceId :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Text)
scheduledInstancesNetworkInterface_networkInterfaceId = Lens.lens (\ScheduledInstancesNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@ScheduledInstancesNetworkInterface' {} a -> s {networkInterfaceId = a} :: ScheduledInstancesNetworkInterface)

-- | The ID of the subnet.
scheduledInstancesNetworkInterface_subnetId :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Text)
scheduledInstancesNetworkInterface_subnetId = Lens.lens (\ScheduledInstancesNetworkInterface' {subnetId} -> subnetId) (\s@ScheduledInstancesNetworkInterface' {} a -> s {subnetId = a} :: ScheduledInstancesNetworkInterface)

-- | The description.
scheduledInstancesNetworkInterface_description :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Text)
scheduledInstancesNetworkInterface_description = Lens.lens (\ScheduledInstancesNetworkInterface' {description} -> description) (\s@ScheduledInstancesNetworkInterface' {} a -> s {description = a} :: ScheduledInstancesNetworkInterface)

-- | The index of the device for the network interface attachment.
scheduledInstancesNetworkInterface_deviceIndex :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Int)
scheduledInstancesNetworkInterface_deviceIndex = Lens.lens (\ScheduledInstancesNetworkInterface' {deviceIndex} -> deviceIndex) (\s@ScheduledInstancesNetworkInterface' {} a -> s {deviceIndex = a} :: ScheduledInstancesNetworkInterface)

-- | The number of secondary private IPv4 addresses.
scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Int)
scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount = Lens.lens (\ScheduledInstancesNetworkInterface' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@ScheduledInstancesNetworkInterface' {} a -> s {secondaryPrivateIpAddressCount = a} :: ScheduledInstancesNetworkInterface)

-- | The private IPv4 addresses.
scheduledInstancesNetworkInterface_privateIpAddressConfigs :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe [ScheduledInstancesPrivateIpAddressConfig])
scheduledInstancesNetworkInterface_privateIpAddressConfigs = Lens.lens (\ScheduledInstancesNetworkInterface' {privateIpAddressConfigs} -> privateIpAddressConfigs) (\s@ScheduledInstancesNetworkInterface' {} a -> s {privateIpAddressConfigs = a} :: ScheduledInstancesNetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The IPv4 address of the network interface within the subnet.
scheduledInstancesNetworkInterface_privateIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Text)
scheduledInstancesNetworkInterface_privateIpAddress = Lens.lens (\ScheduledInstancesNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@ScheduledInstancesNetworkInterface' {} a -> s {privateIpAddress = a} :: ScheduledInstancesNetworkInterface)

instance
  Core.Hashable
    ScheduledInstancesNetworkInterface

instance
  Core.NFData
    ScheduledInstancesNetworkInterface

instance
  Core.ToQuery
    ScheduledInstancesNetworkInterface
  where
  toQuery ScheduledInstancesNetworkInterface' {..} =
    Core.mconcat
      [ Core.toQuery
          (Core.toQueryList "Group" Core.<$> groups),
        Core.toQuery
          ( Core.toQueryList "Ipv6Address"
              Core.<$> ipv6Addresses
          ),
        "AssociatePublicIpAddress"
          Core.=: associatePublicIpAddress,
        "Ipv6AddressCount" Core.=: ipv6AddressCount,
        "DeleteOnTermination" Core.=: deleteOnTermination,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "SubnetId" Core.=: subnetId,
        "Description" Core.=: description,
        "DeviceIndex" Core.=: deviceIndex,
        "SecondaryPrivateIpAddressCount"
          Core.=: secondaryPrivateIpAddressCount,
        Core.toQuery
          ( Core.toQueryList "PrivateIpAddressConfig"
              Core.<$> privateIpAddressConfigs
          ),
        "PrivateIpAddress" Core.=: privateIpAddress
      ]
