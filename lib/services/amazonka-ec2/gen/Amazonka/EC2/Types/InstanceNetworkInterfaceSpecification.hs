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
-- Module      : Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceNetworkInterfaceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceIpv6Address
import Amazonka.EC2.Types.Ipv4PrefixSpecificationRequest
import Amazonka.EC2.Types.Ipv6PrefixSpecificationRequest
import Amazonka.EC2.Types.PrivateIpAddressSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newInstanceNetworkInterfaceSpecification' smart constructor.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'
  { -- | The number of IPv4 delegated prefixes to be automatically assigned to
    -- the network interface. You cannot use this option if you use the
    -- @Ipv4Prefix@ option.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | The type of network interface.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index 0. The default is network card index 0.
    --
    -- If you are using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances>
    -- to create Spot Instances, omit this parameter because you can’t specify
    -- the network card index when using this API. To specify the network card
    -- index, use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to assign a public IPv4 address to an instance you
    -- launch in a VPC. The public IP address can only be assigned to a network
    -- interface for eth0, and can only be assigned to a new network interface,
    -- not an existing one. You cannot specify more than one network interface
    -- in the request. If launching into a default subnet, the default value is
    -- @true@.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | If set to @true@, the interface is deleted when the instance is
    -- terminated. You can specify @true@ only if creating a new network
    -- interface when launching an instance.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The private IPv4 addresses to assign to the network interface. Only one
    -- private IPv4 address can be designated as primary. You cannot specify
    -- this option if you\'re launching more than one instance in a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
    -- request.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | The ID of the subnet associated with the network interface. Applies only
    -- if creating a network interface when launching an instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The description of the network interface. Applies only if creating a
    -- network interface when launching an instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to assign a carrier IP address to the network
    -- interface.
    --
    -- You can only assign a carrier IP address to a network interface that is
    -- in a subnet in a Wavelength Zone. For more information about carrier IP
    -- addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP address>
    -- in the /Amazon Web Services Wavelength Developer Guide/.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    --
    -- If you are creating a Spot Fleet, omit this parameter because you can’t
    -- specify a network interface ID in a launch specification.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | A number of IPv6 addresses to assign to the network interface. Amazon
    -- EC2 chooses the IPv6 addresses from the range of the subnet. You cannot
    -- specify this option and the option to assign specific IPv6 addresses in
    -- the same request. You can specify this option if you\'ve specified a
    -- minimum number of instances to launch.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The IPv4 delegated prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv4PrefixCount@ option.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecificationRequest],
    -- | The private IPv4 address of the network interface. Applies only if
    -- creating a network interface when launching an instance. You cannot
    -- specify this option if you\'re launching more than one instance in a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
    -- request.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 delegated prefixes to be automatically assigned to
    -- the network interface. You cannot use this option if you use the
    -- @Ipv6Prefix@ option.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses. You can\'t specify this
    -- option and specify more than one private IP address using the private IP
    -- addresses option. You cannot specify this option if you\'re launching
    -- more than one instance in a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
    -- request.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The IPv6 delegated prefixes to be assigned to the network interface. You
    -- cannot use this option if you use the @Ipv6PrefixCount@ option.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecificationRequest],
    -- | The IDs of the security groups for the network interface. Applies only
    -- if creating a network interface when launching an instance.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The IPv6 addresses to assign to the network interface. You cannot
    -- specify this option and the option to assign a number of IPv6 addresses
    -- in the same request. You cannot specify this option if you\'ve specified
    -- a minimum number of instances to launch.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | The position of the network interface in the attachment order. A primary
    -- network interface has a device index of 0.
    --
    -- If you specify a network interface when launching an instance, you must
    -- specify the device index.
    deviceIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworkInterfaceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4PrefixCount', 'instanceNetworkInterfaceSpecification_ipv4PrefixCount' - The number of IPv4 delegated prefixes to be automatically assigned to
-- the network interface. You cannot use this option if you use the
-- @Ipv4Prefix@ option.
--
-- 'interfaceType', 'instanceNetworkInterfaceSpecification_interfaceType' - The type of network interface.
--
-- Valid values: @interface@ | @efa@
--
-- 'networkCardIndex', 'instanceNetworkInterfaceSpecification_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- If you are using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances>
-- to create Spot Instances, omit this parameter because you can’t specify
-- the network card index when using this API. To specify the network card
-- index, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>.
--
-- 'associatePublicIpAddress', 'instanceNetworkInterfaceSpecification_associatePublicIpAddress' - Indicates whether to assign a public IPv4 address to an instance you
-- launch in a VPC. The public IP address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
--
-- 'deleteOnTermination', 'instanceNetworkInterfaceSpecification_deleteOnTermination' - If set to @true@, the interface is deleted when the instance is
-- terminated. You can specify @true@ only if creating a new network
-- interface when launching an instance.
--
-- 'privateIpAddresses', 'instanceNetworkInterfaceSpecification_privateIpAddresses' - The private IPv4 addresses to assign to the network interface. Only one
-- private IPv4 address can be designated as primary. You cannot specify
-- this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
--
-- 'subnetId', 'instanceNetworkInterfaceSpecification_subnetId' - The ID of the subnet associated with the network interface. Applies only
-- if creating a network interface when launching an instance.
--
-- 'description', 'instanceNetworkInterfaceSpecification_description' - The description of the network interface. Applies only if creating a
-- network interface when launching an instance.
--
-- 'associateCarrierIpAddress', 'instanceNetworkInterfaceSpecification_associateCarrierIpAddress' - Indicates whether to assign a carrier IP address to the network
-- interface.
--
-- You can only assign a carrier IP address to a network interface that is
-- in a subnet in a Wavelength Zone. For more information about carrier IP
-- addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP address>
-- in the /Amazon Web Services Wavelength Developer Guide/.
--
-- 'networkInterfaceId', 'instanceNetworkInterfaceSpecification_networkInterfaceId' - The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t
-- specify a network interface ID in a launch specification.
--
-- 'ipv6AddressCount', 'instanceNetworkInterfaceSpecification_ipv6AddressCount' - A number of IPv6 addresses to assign to the network interface. Amazon
-- EC2 chooses the IPv6 addresses from the range of the subnet. You cannot
-- specify this option and the option to assign specific IPv6 addresses in
-- the same request. You can specify this option if you\'ve specified a
-- minimum number of instances to launch.
--
-- 'ipv4Prefixes', 'instanceNetworkInterfaceSpecification_ipv4Prefixes' - The IPv4 delegated prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
--
-- 'privateIpAddress', 'instanceNetworkInterfaceSpecification_privateIpAddress' - The private IPv4 address of the network interface. Applies only if
-- creating a network interface when launching an instance. You cannot
-- specify this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
--
-- 'ipv6PrefixCount', 'instanceNetworkInterfaceSpecification_ipv6PrefixCount' - The number of IPv6 delegated prefixes to be automatically assigned to
-- the network interface. You cannot use this option if you use the
-- @Ipv6Prefix@ option.
--
-- 'secondaryPrivateIpAddressCount', 'instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses. You can\'t specify this
-- option and specify more than one private IP address using the private IP
-- addresses option. You cannot specify this option if you\'re launching
-- more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
--
-- 'ipv6Prefixes', 'instanceNetworkInterfaceSpecification_ipv6Prefixes' - The IPv6 delegated prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
--
-- 'groups', 'instanceNetworkInterfaceSpecification_groups' - The IDs of the security groups for the network interface. Applies only
-- if creating a network interface when launching an instance.
--
-- 'ipv6Addresses', 'instanceNetworkInterfaceSpecification_ipv6Addresses' - The IPv6 addresses to assign to the network interface. You cannot
-- specify this option and the option to assign a number of IPv6 addresses
-- in the same request. You cannot specify this option if you\'ve specified
-- a minimum number of instances to launch.
--
-- 'deviceIndex', 'instanceNetworkInterfaceSpecification_deviceIndex' - The position of the network interface in the attachment order. A primary
-- network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must
-- specify the device index.
newInstanceNetworkInterfaceSpecification ::
  InstanceNetworkInterfaceSpecification
newInstanceNetworkInterfaceSpecification =
  InstanceNetworkInterfaceSpecification'
    { ipv4PrefixCount =
        Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      description = Prelude.Nothing,
      associateCarrierIpAddress =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      ipv6PrefixCount = Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      groups = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      deviceIndex = Prelude.Nothing
    }

-- | The number of IPv4 delegated prefixes to be automatically assigned to
-- the network interface. You cannot use this option if you use the
-- @Ipv4Prefix@ option.
instanceNetworkInterfaceSpecification_ipv4PrefixCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_ipv4PrefixCount = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv4PrefixCount = a} :: InstanceNetworkInterfaceSpecification)

-- | The type of network interface.
--
-- Valid values: @interface@ | @efa@
instanceNetworkInterfaceSpecification_interfaceType :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_interfaceType = Lens.lens (\InstanceNetworkInterfaceSpecification' {interfaceType} -> interfaceType) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {interfaceType = a} :: InstanceNetworkInterfaceSpecification)

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- If you are using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances>
-- to create Spot Instances, omit this parameter because you can’t specify
-- the network card index when using this API. To specify the network card
-- index, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>.
instanceNetworkInterfaceSpecification_networkCardIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_networkCardIndex = Lens.lens (\InstanceNetworkInterfaceSpecification' {networkCardIndex} -> networkCardIndex) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceSpecification)

-- | Indicates whether to assign a public IPv4 address to an instance you
-- launch in a VPC. The public IP address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
instanceNetworkInterfaceSpecification_associatePublicIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceSpecification_associatePublicIpAddress = Lens.lens (\InstanceNetworkInterfaceSpecification' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {associatePublicIpAddress = a} :: InstanceNetworkInterfaceSpecification)

-- | If set to @true@, the interface is deleted when the instance is
-- terminated. You can specify @true@ only if creating a new network
-- interface when launching an instance.
instanceNetworkInterfaceSpecification_deleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceSpecification_deleteOnTermination = Lens.lens (\InstanceNetworkInterfaceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceSpecification)

-- | The private IPv4 addresses to assign to the network interface. Only one
-- private IPv4 address can be designated as primary. You cannot specify
-- this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
instanceNetworkInterfaceSpecification_privateIpAddresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [PrivateIpAddressSpecification])
instanceNetworkInterfaceSpecification_privateIpAddresses = Lens.lens (\InstanceNetworkInterfaceSpecification' {privateIpAddresses} -> privateIpAddresses) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddresses = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnet associated with the network interface. Applies only
-- if creating a network interface when launching an instance.
instanceNetworkInterfaceSpecification_subnetId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_subnetId = Lens.lens (\InstanceNetworkInterfaceSpecification' {subnetId} -> subnetId) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {subnetId = a} :: InstanceNetworkInterfaceSpecification)

-- | The description of the network interface. Applies only if creating a
-- network interface when launching an instance.
instanceNetworkInterfaceSpecification_description :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_description = Lens.lens (\InstanceNetworkInterfaceSpecification' {description} -> description) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {description = a} :: InstanceNetworkInterfaceSpecification)

-- | Indicates whether to assign a carrier IP address to the network
-- interface.
--
-- You can only assign a carrier IP address to a network interface that is
-- in a subnet in a Wavelength Zone. For more information about carrier IP
-- addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP address>
-- in the /Amazon Web Services Wavelength Developer Guide/.
instanceNetworkInterfaceSpecification_associateCarrierIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceSpecification_associateCarrierIpAddress = Lens.lens (\InstanceNetworkInterfaceSpecification' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {associateCarrierIpAddress = a} :: InstanceNetworkInterfaceSpecification)

-- | The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t
-- specify a network interface ID in a launch specification.
instanceNetworkInterfaceSpecification_networkInterfaceId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_networkInterfaceId = Lens.lens (\InstanceNetworkInterfaceSpecification' {networkInterfaceId} -> networkInterfaceId) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {networkInterfaceId = a} :: InstanceNetworkInterfaceSpecification)

-- | A number of IPv6 addresses to assign to the network interface. Amazon
-- EC2 chooses the IPv6 addresses from the range of the subnet. You cannot
-- specify this option and the option to assign specific IPv6 addresses in
-- the same request. You can specify this option if you\'ve specified a
-- minimum number of instances to launch.
instanceNetworkInterfaceSpecification_ipv6AddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_ipv6AddressCount = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv6AddressCount} -> ipv6AddressCount) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv6AddressCount = a} :: InstanceNetworkInterfaceSpecification)

-- | The IPv4 delegated prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv4PrefixCount@ option.
instanceNetworkInterfaceSpecification_ipv4Prefixes :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [Ipv4PrefixSpecificationRequest])
instanceNetworkInterfaceSpecification_ipv4Prefixes = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv4Prefixes} -> ipv4Prefixes) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv4Prefixes = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The private IPv4 address of the network interface. Applies only if
-- creating a network interface when launching an instance. You cannot
-- specify this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
instanceNetworkInterfaceSpecification_privateIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_privateIpAddress = Lens.lens (\InstanceNetworkInterfaceSpecification' {privateIpAddress} -> privateIpAddress) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddress = a} :: InstanceNetworkInterfaceSpecification)

-- | The number of IPv6 delegated prefixes to be automatically assigned to
-- the network interface. You cannot use this option if you use the
-- @Ipv6Prefix@ option.
instanceNetworkInterfaceSpecification_ipv6PrefixCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_ipv6PrefixCount = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv6PrefixCount} -> ipv6PrefixCount) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv6PrefixCount = a} :: InstanceNetworkInterfaceSpecification)

-- | The number of secondary private IPv4 addresses. You can\'t specify this
-- option and specify more than one private IP address using the private IP
-- addresses option. You cannot specify this option if you\'re launching
-- more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount = Lens.lens (\InstanceNetworkInterfaceSpecification' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {secondaryPrivateIpAddressCount = a} :: InstanceNetworkInterfaceSpecification)

-- | The IPv6 delegated prefixes to be assigned to the network interface. You
-- cannot use this option if you use the @Ipv6PrefixCount@ option.
instanceNetworkInterfaceSpecification_ipv6Prefixes :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [Ipv6PrefixSpecificationRequest])
instanceNetworkInterfaceSpecification_ipv6Prefixes = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv6Prefixes} -> ipv6Prefixes) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv6Prefixes = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the security groups for the network interface. Applies only
-- if creating a network interface when launching an instance.
instanceNetworkInterfaceSpecification_groups :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [Prelude.Text])
instanceNetworkInterfaceSpecification_groups = Lens.lens (\InstanceNetworkInterfaceSpecification' {groups} -> groups) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {groups = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 addresses to assign to the network interface. You cannot
-- specify this option and the option to assign a number of IPv6 addresses
-- in the same request. You cannot specify this option if you\'ve specified
-- a minimum number of instances to launch.
instanceNetworkInterfaceSpecification_ipv6Addresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [InstanceIpv6Address])
instanceNetworkInterfaceSpecification_ipv6Addresses = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv6Addresses} -> ipv6Addresses) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv6Addresses = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The position of the network interface in the attachment order. A primary
-- network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must
-- specify the device index.
instanceNetworkInterfaceSpecification_deviceIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_deviceIndex = Lens.lens (\InstanceNetworkInterfaceSpecification' {deviceIndex} -> deviceIndex) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {deviceIndex = a} :: InstanceNetworkInterfaceSpecification)

instance
  Data.FromXML
    InstanceNetworkInterfaceSpecification
  where
  parseXML x =
    InstanceNetworkInterfaceSpecification'
      Prelude.<$> (x Data..@? "Ipv4PrefixCount")
      Prelude.<*> (x Data..@? "InterfaceType")
      Prelude.<*> (x Data..@? "NetworkCardIndex")
      Prelude.<*> (x Data..@? "associatePublicIpAddress")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> ( x Data..@? "privateIpAddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "AssociateCarrierIpAddress")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "ipv6AddressCount")
      Prelude.<*> ( x Data..@? "Ipv4Prefix" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "privateIpAddress")
      Prelude.<*> (x Data..@? "Ipv6PrefixCount")
      Prelude.<*> (x Data..@? "secondaryPrivateIpAddressCount")
      Prelude.<*> ( x Data..@? "Ipv6Prefix" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "SecurityGroupId" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "SecurityGroupId")
                  )
      Prelude.<*> ( x Data..@? "ipv6AddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "deviceIndex")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceSpecification
  where
  hashWithSalt
    _salt
    InstanceNetworkInterfaceSpecification' {..} =
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
    InstanceNetworkInterfaceSpecification
  where
  rnf InstanceNetworkInterfaceSpecification' {..} =
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
  Data.ToQuery
    InstanceNetworkInterfaceSpecification
  where
  toQuery InstanceNetworkInterfaceSpecification' {..} =
    Prelude.mconcat
      [ "Ipv4PrefixCount" Data.=: ipv4PrefixCount,
        "InterfaceType" Data.=: interfaceType,
        "NetworkCardIndex" Data.=: networkCardIndex,
        "AssociatePublicIpAddress"
          Data.=: associatePublicIpAddress,
        "DeleteOnTermination" Data.=: deleteOnTermination,
        Data.toQuery
          ( Data.toQueryList "PrivateIpAddresses"
              Prelude.<$> privateIpAddresses
          ),
        "SubnetId" Data.=: subnetId,
        "Description" Data.=: description,
        "AssociateCarrierIpAddress"
          Data.=: associateCarrierIpAddress,
        "NetworkInterfaceId" Data.=: networkInterfaceId,
        "Ipv6AddressCount" Data.=: ipv6AddressCount,
        Data.toQuery
          ( Data.toQueryList "Ipv4Prefix"
              Prelude.<$> ipv4Prefixes
          ),
        "PrivateIpAddress" Data.=: privateIpAddress,
        "Ipv6PrefixCount" Data.=: ipv6PrefixCount,
        "SecondaryPrivateIpAddressCount"
          Data.=: secondaryPrivateIpAddressCount,
        Data.toQuery
          ( Data.toQueryList "Ipv6Prefix"
              Prelude.<$> ipv6Prefixes
          ),
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> groups
          ),
        Data.toQuery
          ( Data.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        "DeviceIndex" Data.=: deviceIndex
      ]
