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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails

-- | One or more network interfaces to attach to an Amazon EC2 instance. If
-- you specify a network interface, you must specify security groups and
-- subnets as part of the network interface.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' smart constructor.
data AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails = AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails'
  { -- | Indicates whether to associate a Carrier IP address with eth0 for a new
    -- network interface. You use this option when you launch an instance in a
    -- Wavelength Zone and want to associate a Carrier IP address with the
    -- network interface. For more information, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP address>
    -- in the /Wavelength Developer Guide/.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Associates a public IPv4 address with eth0 for a new network interface.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The type of network interface.
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv4 prefixes to be automatically assigned to the network
    -- interface. You cannot use this option if you use the @Ipv4Prefixes@
    -- option.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv4 prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv4PrefixCount@ option.
    ipv4Prefixes :: Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails],
    -- | The number of IPv6 addresses to assign to a network interface. Amazon
    -- EC2 automatically selects the IPv6 addresses from the subnet range. You
    -- can\'t use this option if you use @Ipv6Addresses@.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet. You can\'t use this option if you use @Ipv6AddressCount@.
    ipv6Addresses :: Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails],
    -- | The number of IPv6 prefixes to be automatically assigned to the network
    -- interface. You cannot use this option if you use the @Ipv6Prefix@
    -- option.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv6 prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv6PrefixCount@ option.
    ipv6Prefixes :: Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails],
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index @0@. The default is network card index @0@.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails],
    -- | The number of secondary private IPv4 addresses to assign to a network
    -- interface.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the subnet for the network interface.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associateCarrierIpAddress', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associateCarrierIpAddress' - Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface. You use this option when you launch an instance in a
-- Wavelength Zone and want to associate a Carrier IP address with the
-- network interface. For more information, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP address>
-- in the /Wavelength Developer Guide/.
--
-- 'associatePublicIpAddress', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associatePublicIpAddress' - Associates a public IPv4 address with eth0 for a new network interface.
--
-- 'deleteOnTermination', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'description', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_description' - A description for the network interface.
--
-- 'deviceIndex', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deviceIndex' - The device index for the network interface attachment.
--
-- 'groups', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_groups' - The IDs of one or more security groups.
--
-- 'interfaceType', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_interfaceType' - The type of network interface.
--
-- 'ipv4PrefixCount', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4PrefixCount' - The number of IPv4 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv4Prefixes@
-- option.
--
-- 'ipv4Prefixes', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4Prefixes' - One or more IPv4 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
--
-- 'ipv6AddressCount', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if you use @Ipv6Addresses@.
--
-- 'ipv6Addresses', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you use @Ipv6AddressCount@.
--
-- 'ipv6PrefixCount', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6PrefixCount' - The number of IPv6 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv6Prefix@
-- option.
--
-- 'ipv6Prefixes', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Prefixes' - One or more IPv6 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
--
-- 'networkCardIndex', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index @0@. The default is network card index @0@.
--
-- 'networkInterfaceId', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkInterfaceId' - The ID of the network interface.
--
-- 'privateIpAddress', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddress' - The primary private IPv4 address of the network interface.
--
-- 'privateIpAddresses', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'secondaryPrivateIpAddressCount', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses to assign to a network
-- interface.
--
-- 'subnetId', 'awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_subnetId' - The ID of the subnet for the network interface.
newAwsEc2LaunchTemplateDataNetworkInterfaceSetDetails ::
  AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
newAwsEc2LaunchTemplateDataNetworkInterfaceSetDetails =
  AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails'
    { associateCarrierIpAddress =
        Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      deviceIndex =
        Prelude.Nothing,
      groups =
        Prelude.Nothing,
      interfaceType =
        Prelude.Nothing,
      ipv4PrefixCount =
        Prelude.Nothing,
      ipv4Prefixes =
        Prelude.Nothing,
      ipv6AddressCount =
        Prelude.Nothing,
      ipv6Addresses =
        Prelude.Nothing,
      ipv6PrefixCount =
        Prelude.Nothing,
      ipv6Prefixes =
        Prelude.Nothing,
      networkCardIndex =
        Prelude.Nothing,
      networkInterfaceId =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing,
      privateIpAddresses =
        Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      subnetId =
        Prelude.Nothing
    }

-- | Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface. You use this option when you launch an instance in a
-- Wavelength Zone and want to associate a Carrier IP address with the
-- network interface. For more information, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP address>
-- in the /Wavelength Developer Guide/.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associateCarrierIpAddress :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associateCarrierIpAddress = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {associateCarrierIpAddress = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | Associates a public IPv4 address with eth0 for a new network interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associatePublicIpAddress :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_associatePublicIpAddress = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {associatePublicIpAddress = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deleteOnTermination :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deleteOnTermination = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {deleteOnTermination} -> deleteOnTermination) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {deleteOnTermination = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | A description for the network interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_description :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_description = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {description} -> description) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {description = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | The device index for the network interface attachment.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deviceIndex :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_deviceIndex = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {deviceIndex} -> deviceIndex) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {deviceIndex = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | The IDs of one or more security groups.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_groups :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe [Prelude.Text])
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_groups = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {groups} -> groups) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {groups = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of network interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_interfaceType :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_interfaceType = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {interfaceType} -> interfaceType) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {interfaceType = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | The number of IPv4 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv4Prefixes@
-- option.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4PrefixCount :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4PrefixCount = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {ipv4PrefixCount = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | One or more IPv4 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4Prefixes :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails])
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv4Prefixes = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {ipv4Prefixes} -> ipv4Prefixes) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {ipv4Prefixes = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of IPv6 addresses to assign to a network interface. Amazon
-- EC2 automatically selects the IPv6 addresses from the subnet range. You
-- can\'t use this option if you use @Ipv6Addresses@.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6AddressCount :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6AddressCount = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {ipv6AddressCount} -> ipv6AddressCount) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {ipv6AddressCount = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet. You can\'t use this option if you use @Ipv6AddressCount@.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Addresses :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails])
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Addresses = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {ipv6Addresses} -> ipv6Addresses) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {ipv6Addresses = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of IPv6 prefixes to be automatically assigned to the network
-- interface. You cannot use this option if you use the @Ipv6Prefix@
-- option.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6PrefixCount :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6PrefixCount = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {ipv6PrefixCount} -> ipv6PrefixCount) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {ipv6PrefixCount = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | One or more IPv6 prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Prefixes :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails])
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_ipv6Prefixes = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {ipv6Prefixes} -> ipv6Prefixes) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {ipv6Prefixes = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails) Prelude.. Lens.mapping Lens.coerced

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index @0@. The default is network card index @0@.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkCardIndex :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkCardIndex = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {networkCardIndex} -> networkCardIndex) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {networkCardIndex = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | The ID of the network interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkInterfaceId :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_networkInterfaceId = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {networkInterfaceId} -> networkInterfaceId) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {networkInterfaceId = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | The primary private IPv4 address of the network interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddress :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddress = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {privateIpAddress} -> privateIpAddress) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {privateIpAddress = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | One or more private IPv4 addresses.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddresses :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe [AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails])
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_privateIpAddresses = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {privateIpAddresses} -> privateIpAddresses) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {privateIpAddresses = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of secondary private IPv4 addresses to assign to a network
-- interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_secondaryPrivateIpAddressCount :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_secondaryPrivateIpAddressCount = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {secondaryPrivateIpAddressCount = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

-- | The ID of the subnet for the network interface.
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_subnetId :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetDetails_subnetId = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {subnetId} -> subnetId) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {} a -> s {subnetId = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails'
            Prelude.<$> (x Data..:? "AssociateCarrierIpAddress")
              Prelude.<*> (x Data..:? "AssociatePublicIpAddress")
              Prelude.<*> (x Data..:? "DeleteOnTermination")
              Prelude.<*> (x Data..:? "Description")
              Prelude.<*> (x Data..:? "DeviceIndex")
              Prelude.<*> (x Data..:? "Groups" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "InterfaceType")
              Prelude.<*> (x Data..:? "Ipv4PrefixCount")
              Prelude.<*> (x Data..:? "Ipv4Prefixes" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Ipv6AddressCount")
              Prelude.<*> (x Data..:? "Ipv6Addresses" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Ipv6PrefixCount")
              Prelude.<*> (x Data..:? "Ipv6Prefixes" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "NetworkCardIndex")
              Prelude.<*> (x Data..:? "NetworkInterfaceId")
              Prelude.<*> (x Data..:? "PrivateIpAddress")
              Prelude.<*> ( x Data..:? "PrivateIpAddresses"
                              Data..!= Prelude.mempty
                          )
              Prelude.<*> (x Data..:? "SecondaryPrivateIpAddressCount")
              Prelude.<*> (x Data..:? "SubnetId")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {..} =
      _salt
        `Prelude.hashWithSalt` associateCarrierIpAddress
        `Prelude.hashWithSalt` associatePublicIpAddress
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` groups
        `Prelude.hashWithSalt` interfaceType
        `Prelude.hashWithSalt` ipv4PrefixCount
        `Prelude.hashWithSalt` ipv4Prefixes
        `Prelude.hashWithSalt` ipv6AddressCount
        `Prelude.hashWithSalt` ipv6Addresses
        `Prelude.hashWithSalt` ipv6PrefixCount
        `Prelude.hashWithSalt` ipv6Prefixes
        `Prelude.hashWithSalt` networkCardIndex
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` privateIpAddress
        `Prelude.hashWithSalt` privateIpAddresses
        `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
        `Prelude.hashWithSalt` subnetId

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
  where
  rnf
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {..} =
      Prelude.rnf associateCarrierIpAddress
        `Prelude.seq` Prelude.rnf associatePublicIpAddress
        `Prelude.seq` Prelude.rnf deleteOnTermination
        `Prelude.seq` Prelude.rnf description
        `Prelude.seq` Prelude.rnf deviceIndex
        `Prelude.seq` Prelude.rnf groups
        `Prelude.seq` Prelude.rnf interfaceType
        `Prelude.seq` Prelude.rnf ipv4PrefixCount
        `Prelude.seq` Prelude.rnf ipv4Prefixes
        `Prelude.seq` Prelude.rnf ipv6AddressCount
        `Prelude.seq` Prelude.rnf ipv6Addresses
        `Prelude.seq` Prelude.rnf ipv6PrefixCount
        `Prelude.seq` Prelude.rnf ipv6Prefixes
        `Prelude.seq` Prelude.rnf networkCardIndex
        `Prelude.seq` Prelude.rnf networkInterfaceId
        `Prelude.seq` Prelude.rnf privateIpAddress
        `Prelude.seq` Prelude.rnf privateIpAddresses
        `Prelude.seq` Prelude.rnf
          secondaryPrivateIpAddressCount
        `Prelude.seq` Prelude.rnf subnetId

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AssociateCarrierIpAddress" Data..=)
                Prelude.<$> associateCarrierIpAddress,
              ("AssociatePublicIpAddress" Data..=)
                Prelude.<$> associatePublicIpAddress,
              ("DeleteOnTermination" Data..=)
                Prelude.<$> deleteOnTermination,
              ("Description" Data..=) Prelude.<$> description,
              ("DeviceIndex" Data..=) Prelude.<$> deviceIndex,
              ("Groups" Data..=) Prelude.<$> groups,
              ("InterfaceType" Data..=) Prelude.<$> interfaceType,
              ("Ipv4PrefixCount" Data..=)
                Prelude.<$> ipv4PrefixCount,
              ("Ipv4Prefixes" Data..=) Prelude.<$> ipv4Prefixes,
              ("Ipv6AddressCount" Data..=)
                Prelude.<$> ipv6AddressCount,
              ("Ipv6Addresses" Data..=) Prelude.<$> ipv6Addresses,
              ("Ipv6PrefixCount" Data..=)
                Prelude.<$> ipv6PrefixCount,
              ("Ipv6Prefixes" Data..=) Prelude.<$> ipv6Prefixes,
              ("NetworkCardIndex" Data..=)
                Prelude.<$> networkCardIndex,
              ("NetworkInterfaceId" Data..=)
                Prelude.<$> networkInterfaceId,
              ("PrivateIpAddress" Data..=)
                Prelude.<$> privateIpAddress,
              ("PrivateIpAddresses" Data..=)
                Prelude.<$> privateIpAddresses,
              ("SecondaryPrivateIpAddressCount" Data..=)
                Prelude.<$> secondaryPrivateIpAddressCount,
              ("SubnetId" Data..=) Prelude.<$> subnetId
            ]
        )
