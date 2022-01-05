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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceIpv6AddressRequest
import Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest
import Amazonka.EC2.Types.PrivateIpAddressSpecification
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The parameters for a network interface.
--
-- /See:/ 'newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Associates a Carrier IP address with eth0 for a new network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and
    -- want to associate a Carrier IP address with the network interface. For
    -- more information about Carrier IP addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
    -- in the /Wavelength Developer Guide/.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Associates a public IPv4 address with eth0 for a new network interface.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | One or more IPv4 prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv4PrefixCount@ option.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecificationRequest],
    -- | The type of network interface. To create an Elastic Fabric Adapter
    -- (EFA), specify @efa@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- If you are not creating an EFA, specify @interface@ or omit this
    -- parameter.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv4 prefixes to be automatically assigned to the network
    -- interface. You cannot use this option if you use the @Ipv4Prefix@
    -- option.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet for the network interface.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon
    -- EC2 automatically selects the IPv6 addresses from the subnet range. You
    -- can\'t use this option if specifying specific IPv6 addresses.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index 0. The default is network card index 0.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv6 prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv6PrefixCount@ option.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecificationRequest],
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 prefixes to be automatically assigned to the network
    -- interface. You cannot use this option if you use the @Ipv6Prefix@
    -- option.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses to assign to a network
    -- interface.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet. You can\'t use this option if you\'re specifying a number
    -- of IPv6 addresses.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6AddressRequest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'deleteOnTermination', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'associateCarrierIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress' - Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /Wavelength Developer Guide/.
--
-- 'associatePublicIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress' - Associates a public IPv4 address with eth0 for a new network interface.
--
-- 'ipv4Prefixes', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes' - One or more IPv4 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
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
-- 'ipv4PrefixCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount' - The number of IPv4 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv4Prefix@
-- option.
--
-- 'networkInterfaceId', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId' - The ID of the subnet for the network interface.
--
-- 'ipv6AddressCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses.
--
-- 'networkCardIndex', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- 'ipv6Prefixes', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes' - One or more IPv6 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
--
-- 'privateIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress' - The primary private IPv4 address of the network interface.
--
-- 'ipv6PrefixCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount' - The number of IPv6 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv6Prefix@
-- option.
--
-- 'secondaryPrivateIpAddressCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses to assign to a network
-- interface.
--
-- 'description', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_description' - A description for the network interface.
--
-- 'deviceIndex', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex' - The device index for the network interface attachment.
--
-- 'ipv6Addresses', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest ::
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { groups =
        Prelude.Nothing,
      privateIpAddresses =
        Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      associateCarrierIpAddress =
        Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      ipv4Prefixes =
        Prelude.Nothing,
      interfaceType =
        Prelude.Nothing,
      ipv4PrefixCount =
        Prelude.Nothing,
      networkInterfaceId =
        Prelude.Nothing,
      subnetId =
        Prelude.Nothing,
      ipv6AddressCount =
        Prelude.Nothing,
      networkCardIndex =
        Prelude.Nothing,
      ipv6Prefixes =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing,
      ipv6PrefixCount =
        Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      deviceIndex =
        Prelude.Nothing,
      ipv6Addresses =
        Prelude.Nothing
    }

-- | The IDs of one or more security groups.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [Prelude.Text])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {groups} -> groups) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | One or more private IPv4 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [PrivateIpAddressSpecification])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {privateIpAddresses} -> privateIpAddresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {privateIpAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /Wavelength Developer Guide/.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {associateCarrierIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Associates a public IPv4 address with eth0 for a new network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {associatePublicIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | One or more IPv4 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [Ipv4PrefixSpecificationRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv4Prefixes} -> ipv4Prefixes) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv4Prefixes = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The type of network interface. To create an Elastic Fabric Adapter
-- (EFA), specify @efa@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are not creating an EFA, specify @interface@ or omit this
-- parameter.
--
-- Valid values: @interface@ | @efa@
launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_interfaceType = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {interfaceType} -> interfaceType) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of IPv4 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv4Prefix@
-- option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv4PrefixCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The ID of the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {networkInterfaceId} -> networkInterfaceId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The ID of the subnet for the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {subnetId} -> subnetId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6AddressCount} -> ipv6AddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {networkCardIndex} -> networkCardIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | One or more IPv6 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [Ipv6PrefixSpecificationRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6Prefixes} -> ipv6Prefixes) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6Prefixes = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The primary private IPv4 address of the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {privateIpAddress} -> privateIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {privateIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of IPv6 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv6Prefix@
-- option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6PrefixCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6PrefixCount} -> ipv6PrefixCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6PrefixCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of secondary private IPv4 addresses to assign to a network
-- interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_secondaryPrivateIpAddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {secondaryPrivateIpAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | A description for the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_description :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_description = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {description} -> description) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The device index for the network interface attachment.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {deviceIndex} -> deviceIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [InstanceIpv6AddressRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6Addresses} -> ipv6Addresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` groups
        `Prelude.hashWithSalt` privateIpAddresses
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` associateCarrierIpAddress
        `Prelude.hashWithSalt` associatePublicIpAddress
        `Prelude.hashWithSalt` ipv4Prefixes
        `Prelude.hashWithSalt` interfaceType
        `Prelude.hashWithSalt` ipv4PrefixCount
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` ipv6AddressCount
        `Prelude.hashWithSalt` networkCardIndex
        `Prelude.hashWithSalt` ipv6Prefixes
        `Prelude.hashWithSalt` privateIpAddress
        `Prelude.hashWithSalt` ipv6PrefixCount
        `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` ipv6Addresses

instance
  Prelude.NFData
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  rnf
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      Prelude.rnf groups
        `Prelude.seq` Prelude.rnf privateIpAddresses
        `Prelude.seq` Prelude.rnf deleteOnTermination
        `Prelude.seq` Prelude.rnf associateCarrierIpAddress
        `Prelude.seq` Prelude.rnf associatePublicIpAddress
        `Prelude.seq` Prelude.rnf ipv4Prefixes
        `Prelude.seq` Prelude.rnf interfaceType
        `Prelude.seq` Prelude.rnf ipv4PrefixCount
        `Prelude.seq` Prelude.rnf networkInterfaceId
        `Prelude.seq` Prelude.rnf subnetId
        `Prelude.seq` Prelude.rnf ipv6AddressCount
        `Prelude.seq` Prelude.rnf networkCardIndex
        `Prelude.seq` Prelude.rnf ipv6Prefixes
        `Prelude.seq` Prelude.rnf privateIpAddress
        `Prelude.seq` Prelude.rnf ipv6PrefixCount
        `Prelude.seq` Prelude.rnf
          secondaryPrivateIpAddressCount
        `Prelude.seq` Prelude.rnf description
        `Prelude.seq` Prelude.rnf deviceIndex
        `Prelude.seq` Prelude.rnf ipv6Addresses

instance
  Core.ToQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  toQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      Prelude.mconcat
        [ Core.toQuery
            ( Core.toQueryList "SecurityGroupId"
                Prelude.<$> groups
            ),
          Core.toQuery
            ( Core.toQueryList "PrivateIpAddresses"
                Prelude.<$> privateIpAddresses
            ),
          "DeleteOnTermination" Core.=: deleteOnTermination,
          "AssociateCarrierIpAddress"
            Core.=: associateCarrierIpAddress,
          "AssociatePublicIpAddress"
            Core.=: associatePublicIpAddress,
          Core.toQuery
            ( Core.toQueryList "Ipv4Prefix"
                Prelude.<$> ipv4Prefixes
            ),
          "InterfaceType" Core.=: interfaceType,
          "Ipv4PrefixCount" Core.=: ipv4PrefixCount,
          "NetworkInterfaceId" Core.=: networkInterfaceId,
          "SubnetId" Core.=: subnetId,
          "Ipv6AddressCount" Core.=: ipv6AddressCount,
          "NetworkCardIndex" Core.=: networkCardIndex,
          Core.toQuery
            ( Core.toQueryList "Ipv6Prefix"
                Prelude.<$> ipv6Prefixes
            ),
          "PrivateIpAddress" Core.=: privateIpAddress,
          "Ipv6PrefixCount" Core.=: ipv6PrefixCount,
          "SecondaryPrivateIpAddressCount"
            Core.=: secondaryPrivateIpAddressCount,
          "Description" Core.=: description,
          "DeviceIndex" Core.=: deviceIndex,
          Core.toQuery
            ( Core.toQueryList "Ipv6Addresses"
                Prelude.<$> ipv6Addresses
            )
        ]
