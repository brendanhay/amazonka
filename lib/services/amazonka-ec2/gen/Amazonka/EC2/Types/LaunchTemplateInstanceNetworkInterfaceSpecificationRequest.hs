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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceIpv6AddressRequest
import Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest
import Amazonka.EC2.Types.PrivateIpAddressSpecification
import qualified Amazonka.Prelude as Prelude

-- | The parameters for a network interface.
--
-- /See:/ 'newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { -- | The number of IPv4 prefixes to be automatically assigned to the network
    -- interface. You cannot use this option if you use the @Ipv4Prefix@
    -- option.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
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
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index 0. The default is network card index 0.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Associates a public IPv4 address with eth0 for a new network interface.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | The ID of the subnet for the network interface.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
    -- | Associates a Carrier IP address with eth0 for a new network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and
    -- want to associate a Carrier IP address with the network interface. For
    -- more information about Carrier IP addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
    -- in the /Wavelength Developer Guide/.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon
    -- EC2 automatically selects the IPv6 addresses from the subnet range. You
    -- can\'t use this option if specifying specific IPv6 addresses.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv4 prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv4PrefixCount@ option.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecificationRequest],
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 prefixes to be automatically assigned to the network
    -- interface. You cannot use this option if you use the @Ipv6Prefix@
    -- option.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses to assign to a network
    -- interface.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv6 prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv6PrefixCount@ option.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecificationRequest],
    -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet. You can\'t use this option if you\'re specifying a number
    -- of IPv6 addresses.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6AddressRequest],
    -- | The device index for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int
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
-- 'ipv4PrefixCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount' - The number of IPv4 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv4Prefix@
-- option.
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
-- 'networkCardIndex', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- 'associatePublicIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress' - Associates a public IPv4 address with eth0 for a new network interface.
--
-- 'deleteOnTermination', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'privateIpAddresses', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'subnetId', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId' - The ID of the subnet for the network interface.
--
-- 'description', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_description' - A description for the network interface.
--
-- 'associateCarrierIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress' - Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /Wavelength Developer Guide/.
--
-- 'networkInterfaceId', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId' - The ID of the network interface.
--
-- 'ipv6AddressCount', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses.
--
-- 'ipv4Prefixes', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes' - One or more IPv4 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
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
-- 'ipv6Prefixes', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes' - One or more IPv6 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
--
-- 'groups', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups' - The IDs of one or more security groups.
--
-- 'ipv6Addresses', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
--
-- 'deviceIndex', 'launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex' - The device index for the network interface attachment.
newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest ::
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
newLaunchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { ipv4PrefixCount =
        Prelude.Nothing,
      interfaceType =
        Prelude.Nothing,
      networkCardIndex =
        Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      privateIpAddresses =
        Prelude.Nothing,
      subnetId =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      associateCarrierIpAddress =
        Prelude.Nothing,
      networkInterfaceId =
        Prelude.Nothing,
      ipv6AddressCount =
        Prelude.Nothing,
      ipv4Prefixes =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing,
      ipv6PrefixCount =
        Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      ipv6Prefixes =
        Prelude.Nothing,
      groups =
        Prelude.Nothing,
      ipv6Addresses =
        Prelude.Nothing,
      deviceIndex =
        Prelude.Nothing
    }

-- | The number of IPv4 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv4Prefix@
-- option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4PrefixCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv4PrefixCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

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

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkCardIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {networkCardIndex} -> networkCardIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Associates a public IPv4 address with eth0 for a new network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associatePublicIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {associatePublicIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deleteOnTermination = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | One or more private IPv4 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [PrivateIpAddressSpecification])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_privateIpAddresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {privateIpAddresses} -> privateIpAddresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {privateIpAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnet for the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_subnetId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {subnetId} -> subnetId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | A description for the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_description :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_description = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {description} -> description) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /Wavelength Developer Guide/.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_associateCarrierIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {associateCarrierIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The ID of the network interface.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_networkInterfaceId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {networkInterfaceId} -> networkInterfaceId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if specifying specific IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6AddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6AddressCount} -> ipv6AddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

-- | One or more IPv4 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [Ipv4PrefixSpecificationRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv4Prefixes = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv4Prefixes} -> ipv4Prefixes) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv4Prefixes = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

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

-- | One or more IPv6 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [Ipv6PrefixSpecificationRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Prefixes = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6Prefixes} -> ipv6Prefixes) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6Prefixes = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of one or more security groups.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [Prelude.Text])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_groups = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {groups} -> groups) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you\'re specifying a number
-- of IPv6 addresses.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe [InstanceIpv6AddressRequest])
launchTemplateInstanceNetworkInterfaceSpecificationRequest_ipv6Addresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {ipv6Addresses} -> ipv6Addresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The device index for the network interface attachment.
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecificationRequest_deviceIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {deviceIndex} -> deviceIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {} a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)

instance
  Prelude.Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      _salt `Prelude.hashWithSalt` ipv4PrefixCount
        `Prelude.hashWithSalt` interfaceType
        `Prelude.hashWithSalt` networkCardIndex
        `Prelude.hashWithSalt` associatePublicIpAddress
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` privateIpAddresses
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` associateCarrierIpAddress
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` ipv6AddressCount
        `Prelude.hashWithSalt` ipv4Prefixes
        `Prelude.hashWithSalt` privateIpAddress
        `Prelude.hashWithSalt` ipv6PrefixCount
        `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
        `Prelude.hashWithSalt` ipv6Prefixes
        `Prelude.hashWithSalt` groups
        `Prelude.hashWithSalt` ipv6Addresses
        `Prelude.hashWithSalt` deviceIndex

instance
  Prelude.NFData
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  rnf
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      Prelude.rnf ipv4PrefixCount
        `Prelude.seq` Prelude.rnf interfaceType
        `Prelude.seq` Prelude.rnf networkCardIndex
        `Prelude.seq` Prelude.rnf associatePublicIpAddress
        `Prelude.seq` Prelude.rnf deleteOnTermination
        `Prelude.seq` Prelude.rnf privateIpAddresses
        `Prelude.seq` Prelude.rnf subnetId
        `Prelude.seq` Prelude.rnf description
        `Prelude.seq` Prelude.rnf associateCarrierIpAddress
        `Prelude.seq` Prelude.rnf networkInterfaceId
        `Prelude.seq` Prelude.rnf ipv6AddressCount
        `Prelude.seq` Prelude.rnf ipv4Prefixes
        `Prelude.seq` Prelude.rnf privateIpAddress
        `Prelude.seq` Prelude.rnf ipv6PrefixCount
        `Prelude.seq` Prelude.rnf
          secondaryPrivateIpAddressCount
        `Prelude.seq` Prelude.rnf ipv6Prefixes
        `Prelude.seq` Prelude.rnf groups
        `Prelude.seq` Prelude.rnf ipv6Addresses
        `Prelude.seq` Prelude.rnf deviceIndex

instance
  Core.ToQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  toQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
      Prelude.mconcat
        [ "Ipv4PrefixCount" Core.=: ipv4PrefixCount,
          "InterfaceType" Core.=: interfaceType,
          "NetworkCardIndex" Core.=: networkCardIndex,
          "AssociatePublicIpAddress"
            Core.=: associatePublicIpAddress,
          "DeleteOnTermination" Core.=: deleteOnTermination,
          Core.toQuery
            ( Core.toQueryList "PrivateIpAddresses"
                Prelude.<$> privateIpAddresses
            ),
          "SubnetId" Core.=: subnetId,
          "Description" Core.=: description,
          "AssociateCarrierIpAddress"
            Core.=: associateCarrierIpAddress,
          "NetworkInterfaceId" Core.=: networkInterfaceId,
          "Ipv6AddressCount" Core.=: ipv6AddressCount,
          Core.toQuery
            ( Core.toQueryList "Ipv4Prefix"
                Prelude.<$> ipv4Prefixes
            ),
          "PrivateIpAddress" Core.=: privateIpAddress,
          "Ipv6PrefixCount" Core.=: ipv6PrefixCount,
          "SecondaryPrivateIpAddressCount"
            Core.=: secondaryPrivateIpAddressCount,
          Core.toQuery
            ( Core.toQueryList "Ipv6Prefix"
                Prelude.<$> ipv6Prefixes
            ),
          Core.toQuery
            ( Core.toQueryList "SecurityGroupId"
                Prelude.<$> groups
            ),
          Core.toQuery
            ( Core.toQueryList "Ipv6Addresses"
                Prelude.<$> ipv6Addresses
            ),
          "DeviceIndex" Core.=: deviceIndex
        ]
