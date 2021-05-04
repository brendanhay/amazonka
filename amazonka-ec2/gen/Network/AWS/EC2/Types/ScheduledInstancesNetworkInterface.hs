{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ScheduledInstancesIpv6Address
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { -- | The IDs of the security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The specific IPv6 addresses from the subnet range.
    ipv6Addresses :: Prelude.Maybe [ScheduledInstancesIpv6Address],
    -- | Indicates whether to assign a public IPv4 address to instances launched
    -- in a VPC. The public IPv4 address can only be assigned to a network
    -- interface for eth0, and can only be assigned to a new network interface,
    -- not an existing one. You cannot specify more than one network interface
    -- in the request. If launching into a default subnet, the default value is
    -- @true@.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The number of IPv6 addresses to assign to the network interface. The
    -- IPv6 addresses are automatically selected from the subnet range.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to delete the interface when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The index of the device for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The private IPv4 addresses.
    privateIpAddressConfigs :: Prelude.Maybe [ScheduledInstancesPrivateIpAddressConfig],
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      privateIpAddressConfigs =
        Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The IDs of the security groups.
scheduledInstancesNetworkInterface_groups :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe [Prelude.Text])
scheduledInstancesNetworkInterface_groups = Lens.lens (\ScheduledInstancesNetworkInterface' {groups} -> groups) (\s@ScheduledInstancesNetworkInterface' {} a -> s {groups = a} :: ScheduledInstancesNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The specific IPv6 addresses from the subnet range.
scheduledInstancesNetworkInterface_ipv6Addresses :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe [ScheduledInstancesIpv6Address])
scheduledInstancesNetworkInterface_ipv6Addresses = Lens.lens (\ScheduledInstancesNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@ScheduledInstancesNetworkInterface' {} a -> s {ipv6Addresses = a} :: ScheduledInstancesNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether to assign a public IPv4 address to instances launched
-- in a VPC. The public IPv4 address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
scheduledInstancesNetworkInterface_associatePublicIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Bool)
scheduledInstancesNetworkInterface_associatePublicIpAddress = Lens.lens (\ScheduledInstancesNetworkInterface' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ScheduledInstancesNetworkInterface' {} a -> s {associatePublicIpAddress = a} :: ScheduledInstancesNetworkInterface)

-- | The number of IPv6 addresses to assign to the network interface. The
-- IPv6 addresses are automatically selected from the subnet range.
scheduledInstancesNetworkInterface_ipv6AddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Int)
scheduledInstancesNetworkInterface_ipv6AddressCount = Lens.lens (\ScheduledInstancesNetworkInterface' {ipv6AddressCount} -> ipv6AddressCount) (\s@ScheduledInstancesNetworkInterface' {} a -> s {ipv6AddressCount = a} :: ScheduledInstancesNetworkInterface)

-- | Indicates whether to delete the interface when the instance is
-- terminated.
scheduledInstancesNetworkInterface_deleteOnTermination :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Bool)
scheduledInstancesNetworkInterface_deleteOnTermination = Lens.lens (\ScheduledInstancesNetworkInterface' {deleteOnTermination} -> deleteOnTermination) (\s@ScheduledInstancesNetworkInterface' {} a -> s {deleteOnTermination = a} :: ScheduledInstancesNetworkInterface)

-- | The ID of the network interface.
scheduledInstancesNetworkInterface_networkInterfaceId :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_networkInterfaceId = Lens.lens (\ScheduledInstancesNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@ScheduledInstancesNetworkInterface' {} a -> s {networkInterfaceId = a} :: ScheduledInstancesNetworkInterface)

-- | The ID of the subnet.
scheduledInstancesNetworkInterface_subnetId :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_subnetId = Lens.lens (\ScheduledInstancesNetworkInterface' {subnetId} -> subnetId) (\s@ScheduledInstancesNetworkInterface' {} a -> s {subnetId = a} :: ScheduledInstancesNetworkInterface)

-- | The description.
scheduledInstancesNetworkInterface_description :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_description = Lens.lens (\ScheduledInstancesNetworkInterface' {description} -> description) (\s@ScheduledInstancesNetworkInterface' {} a -> s {description = a} :: ScheduledInstancesNetworkInterface)

-- | The index of the device for the network interface attachment.
scheduledInstancesNetworkInterface_deviceIndex :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Int)
scheduledInstancesNetworkInterface_deviceIndex = Lens.lens (\ScheduledInstancesNetworkInterface' {deviceIndex} -> deviceIndex) (\s@ScheduledInstancesNetworkInterface' {} a -> s {deviceIndex = a} :: ScheduledInstancesNetworkInterface)

-- | The number of secondary private IPv4 addresses.
scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Int)
scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount = Lens.lens (\ScheduledInstancesNetworkInterface' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@ScheduledInstancesNetworkInterface' {} a -> s {secondaryPrivateIpAddressCount = a} :: ScheduledInstancesNetworkInterface)

-- | The private IPv4 addresses.
scheduledInstancesNetworkInterface_privateIpAddressConfigs :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe [ScheduledInstancesPrivateIpAddressConfig])
scheduledInstancesNetworkInterface_privateIpAddressConfigs = Lens.lens (\ScheduledInstancesNetworkInterface' {privateIpAddressConfigs} -> privateIpAddressConfigs) (\s@ScheduledInstancesNetworkInterface' {} a -> s {privateIpAddressConfigs = a} :: ScheduledInstancesNetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The IPv4 address of the network interface within the subnet.
scheduledInstancesNetworkInterface_privateIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_privateIpAddress = Lens.lens (\ScheduledInstancesNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@ScheduledInstancesNetworkInterface' {} a -> s {privateIpAddress = a} :: ScheduledInstancesNetworkInterface)

instance
  Prelude.Hashable
    ScheduledInstancesNetworkInterface

instance
  Prelude.NFData
    ScheduledInstancesNetworkInterface

instance
  Prelude.ToQuery
    ScheduledInstancesNetworkInterface
  where
  toQuery ScheduledInstancesNetworkInterface' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          (Prelude.toQueryList "Group" Prelude.<$> groups),
        Prelude.toQuery
          ( Prelude.toQueryList "Ipv6Address"
              Prelude.<$> ipv6Addresses
          ),
        "AssociatePublicIpAddress"
          Prelude.=: associatePublicIpAddress,
        "Ipv6AddressCount" Prelude.=: ipv6AddressCount,
        "DeleteOnTermination" Prelude.=: deleteOnTermination,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId,
        "SubnetId" Prelude.=: subnetId,
        "Description" Prelude.=: description,
        "DeviceIndex" Prelude.=: deviceIndex,
        "SecondaryPrivateIpAddressCount"
          Prelude.=: secondaryPrivateIpAddressCount,
        Prelude.toQuery
          ( Prelude.toQueryList "PrivateIpAddressConfig"
              Prelude.<$> privateIpAddressConfigs
          ),
        "PrivateIpAddress" Prelude.=: privateIpAddress
      ]
