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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceIpv6AddressRequest
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import qualified Network.AWS.Lens as Lens

-- | The parameters for a network interface.
--
-- /See:/ 'newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { -- | The IDs of one or more security groups.
    groups :: Core.Maybe [Core.Text],
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Core.Maybe [PrivateIpAddressSpecification],
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet. You can\'t use this option if you\'re specifying a number
    -- of IPv6 addresses.
    ipv6Addresses :: Core.Maybe [InstanceIpv6AddressRequest],
    -- | The type of network interface. To create an Elastic Fabric Adapter
    -- (EFA), specify @efa@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- If you are not creating an EFA, specify @interface@ or omit this
    -- parameter.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Core.Maybe Core.Text,
    -- | Associates a public IPv4 address with eth0 for a new network interface.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | Associates a Carrier IP address with eth0 for a new network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and
    -- want to associate a Carrier IP address with the network interface. For
    -- more information about Carrier IP addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
    -- in the /AWS Wavelength Developer Guide/.
    associateCarrierIpAddress :: Core.Maybe Core.Bool,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon
    -- EC2 automatically selects the IPv6 addresses from the subnet range. You
    -- can\'t use this option if specifying specific IPv6 addresses.
    ipv6AddressCount :: Core.Maybe Core.Int,
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index 0. The default is network card index 0.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the subnet for the network interface.
    subnetId :: Core.Maybe Core.Text,
    -- | A description for the network interface.
    description :: Core.Maybe Core.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The number of secondary private IPv4 addresses to assign to a network
    -- interface.
    secondaryPrivateIpAddressCount :: Core.Maybe Core.Int,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups' - The IDs of one or more security groups.
--
-- 'privateIpAddresses', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'ipv6Addresses', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
--
-- 'interfaceType', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType' - The type of network interface. To create an Elastic Fabric Adapter
-- (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are not creating an EFA, specify @interface@ or omit this
-- parameter.
--
-- Valid values: @interface@ | @efa@
--
-- 'associatePublicIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress' - Associates a public IPv4 address with eth0 for a new network interface.
--
-- 'associateCarrierIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress' - Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /AWS Wavelength Developer Guide/.
--
-- 'ipv6AddressCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses.
--
-- 'networkCardIndex', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- 'deleteOnTermination', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'networkInterfaceId', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId' - The ID of the subnet for the network interface.
--
-- 'description', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_description' - A description for the network interface.
--
-- 'deviceIndex', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex' - The device index for the network interface attachment.
--
-- 'secondaryPrivateIpAddressCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses to assign to a network
-- interface.
--
-- 'privateIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress' - The primary private IPv4 address of the network interface.
newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest ::
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { groups =
        Core.Nothing,
      privateIpAddresses =
        Core.Nothing,
      ipv6Addresses =
        Core.Nothing,
      interfaceType =
        Core.Nothing,
      associatePublicIpAddress =
        Core.Nothing,
      associateCarrierIpAddress =
        Core.Nothing,
      ipv6AddressCount =
        Core.Nothing,
      networkCardIndex =
        Core.Nothing,
      deleteOnTermination =
        Core.Nothing,
      networkInterfaceId =
        Core.Nothing,
      subnetId =
        Core.Nothing,
      description =
        Core.Nothing,
      deviceIndex =
        Core.Nothing,
      secondaryPrivateIpAddressCount =
        Core.Nothing,
      privateIpAddress =
        Core.Nothing
    }

-- | The IDs of one or more security groups.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Core.Text])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {groups} -> groups) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Core.. Lens.mapping Lens._Coerce

-- | One or more private IPv4 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [PrivateIpAddressSpecification])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {privateIpAddresses} -> privateIpAddresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {privateIpAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Core.. Lens.mapping Lens._Coerce

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [InstanceIpv6AddressRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6Addresses} -> ipv6Addresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Core.. Lens.mapping Lens._Coerce

-- | The type of network interface. To create an Elastic Fabric Adapter
-- (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are not creating an EFA, specify @interface@ or omit this
-- parameter.
--
-- Valid values: @interface@ | @efa@
launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {interfaceType} -> interfaceType) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Associates a public IPv4 address with eth0 for a new network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {associatePublicIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /AWS Wavelength Developer Guide/.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {associateCarrierIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6AddressCount} -> ipv6AddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {networkCardIndex} -> networkCardIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The ID of the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {networkInterfaceId} -> networkInterfaceId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The ID of the subnet for the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {subnetId} -> subnetId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | A description for the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_description :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_description = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {description} -> description) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The device index for the network interface attachment.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {deviceIndex} -> deviceIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of secondary private IPv4 addresses to assign to a network
-- interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {secondaryPrivateIpAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The primary private IPv4 address of the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {privateIpAddress} -> privateIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {privateIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

instance
  Core.Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest

instance
  Core.NFData
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest

instance
  Core.ToQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  toQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      Core.mconcat
        [ Core.toQuery
            (Core.toQueryList "SecurityGroupId" Core.<$> groups),
          Core.toQuery
            ( Core.toQueryList "PrivateIpAddresses"
                Core.<$> privateIpAddresses
            ),
          Core.toQuery
            ( Core.toQueryList "Ipv6Addresses"
                Core.<$> ipv6Addresses
            ),
          "InterfaceType" Core.=: interfaceType,
          "AssociatePublicIpAddress"
            Core.=: associatePublicIpAddress,
          "AssociateCarrierIpAddress"
            Core.=: associateCarrierIpAddress,
          "Ipv6AddressCount" Core.=: ipv6AddressCount,
          "NetworkCardIndex" Core.=: networkCardIndex,
          "DeleteOnTermination" Core.=: deleteOnTermination,
          "NetworkInterfaceId" Core.=: networkInterfaceId,
          "SubnetId" Core.=: subnetId,
          "Description" Core.=: description,
          "DeviceIndex" Core.=: deviceIndex,
          "SecondaryPrivateIpAddressCount"
            Core.=: secondaryPrivateIpAddressCount,
          "PrivateIpAddress" Core.=: privateIpAddress
        ]
