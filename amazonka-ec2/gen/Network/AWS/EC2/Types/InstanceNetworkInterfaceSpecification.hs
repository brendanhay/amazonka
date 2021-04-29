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
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newInstanceNetworkInterfaceSpecification' smart constructor.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'
  { -- | The IDs of the security groups for the network interface. Applies only
    -- if creating a network interface when launching an instance.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | One or more private IPv4 addresses to assign to the network interface.
    -- Only one private IPv4 address can be designated as primary. You cannot
    -- specify this option if you\'re launching more than one instance in a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
    -- request.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | One or more IPv6 addresses to assign to the network interface. You
    -- cannot specify this option and the option to assign a number of IPv6
    -- addresses in the same request. You cannot specify this option if you\'ve
    -- specified a minimum number of instances to launch.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | The type of network interface.
    --
    -- To create an Elastic Fabric Adapter (EFA), specify @efa@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- If you are not creating an EFA, specify @interface@ or omit this
    -- parameter.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to assign a public IPv4 address to an instance you
    -- launch in a VPC. The public IP address can only be assigned to a network
    -- interface for eth0, and can only be assigned to a new network interface,
    -- not an existing one. You cannot specify more than one network interface
    -- in the request. If launching into a default subnet, the default value is
    -- @true@.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to assign a carrier IP address to the network
    -- interface.
    --
    -- You can only assign a carrier IP address to a network interface that is
    -- in a subnet in a Wavelength Zone. For more information about carrier IP
    -- addresses, see Carrier IP addresses in the AWS Wavelength Developer
    -- Guide.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | A number of IPv6 addresses to assign to the network interface. Amazon
    -- EC2 chooses the IPv6 addresses from the range of the subnet. You cannot
    -- specify this option and the option to assign specific IPv6 addresses in
    -- the same request. You can specify this option if you\'ve specified a
    -- minimum number of instances to launch.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index 0. The default is network card index 0.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | If set to @true@, the interface is deleted when the instance is
    -- terminated. You can specify @true@ only if creating a new network
    -- interface when launching an instance.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    --
    -- If you are creating a Spot Fleet, omit this parameter because you can’t
    -- specify a network interface ID in a launch specification.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet associated with the network interface. Applies only
    -- if creating a network interface when launching an instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The description of the network interface. Applies only if creating a
    -- network interface when launching an instance.
    description :: Prelude.Maybe Prelude.Text,
    -- | The position of the network interface in the attachment order. A primary
    -- network interface has a device index of 0.
    --
    -- If you specify a network interface when launching an instance, you must
    -- specify the device index.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses. You can\'t specify this
    -- option and specify more than one private IP address using the private IP
    -- addresses option. You cannot specify this option if you\'re launching
    -- more than one instance in a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
    -- request.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The private IPv4 address of the network interface. Applies only if
    -- creating a network interface when launching an instance. You cannot
    -- specify this option if you\'re launching more than one instance in a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
    -- request.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworkInterfaceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'instanceNetworkInterfaceSpecification_groups' - The IDs of the security groups for the network interface. Applies only
-- if creating a network interface when launching an instance.
--
-- 'privateIpAddresses', 'instanceNetworkInterfaceSpecification_privateIpAddresses' - One or more private IPv4 addresses to assign to the network interface.
-- Only one private IPv4 address can be designated as primary. You cannot
-- specify this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
--
-- 'ipv6Addresses', 'instanceNetworkInterfaceSpecification_ipv6Addresses' - One or more IPv6 addresses to assign to the network interface. You
-- cannot specify this option and the option to assign a number of IPv6
-- addresses in the same request. You cannot specify this option if you\'ve
-- specified a minimum number of instances to launch.
--
-- 'interfaceType', 'instanceNetworkInterfaceSpecification_interfaceType' - The type of network interface.
--
-- To create an Elastic Fabric Adapter (EFA), specify @efa@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are not creating an EFA, specify @interface@ or omit this
-- parameter.
--
-- Valid values: @interface@ | @efa@
--
-- 'associatePublicIpAddress', 'instanceNetworkInterfaceSpecification_associatePublicIpAddress' - Indicates whether to assign a public IPv4 address to an instance you
-- launch in a VPC. The public IP address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
--
-- 'associateCarrierIpAddress', 'instanceNetworkInterfaceSpecification_associateCarrierIpAddress' - Indicates whether to assign a carrier IP address to the network
-- interface.
--
-- You can only assign a carrier IP address to a network interface that is
-- in a subnet in a Wavelength Zone. For more information about carrier IP
-- addresses, see Carrier IP addresses in the AWS Wavelength Developer
-- Guide.
--
-- 'ipv6AddressCount', 'instanceNetworkInterfaceSpecification_ipv6AddressCount' - A number of IPv6 addresses to assign to the network interface. Amazon
-- EC2 chooses the IPv6 addresses from the range of the subnet. You cannot
-- specify this option and the option to assign specific IPv6 addresses in
-- the same request. You can specify this option if you\'ve specified a
-- minimum number of instances to launch.
--
-- 'networkCardIndex', 'instanceNetworkInterfaceSpecification_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- 'deleteOnTermination', 'instanceNetworkInterfaceSpecification_deleteOnTermination' - If set to @true@, the interface is deleted when the instance is
-- terminated. You can specify @true@ only if creating a new network
-- interface when launching an instance.
--
-- 'networkInterfaceId', 'instanceNetworkInterfaceSpecification_networkInterfaceId' - The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t
-- specify a network interface ID in a launch specification.
--
-- 'subnetId', 'instanceNetworkInterfaceSpecification_subnetId' - The ID of the subnet associated with the network interface. Applies only
-- if creating a network interface when launching an instance.
--
-- 'description', 'instanceNetworkInterfaceSpecification_description' - The description of the network interface. Applies only if creating a
-- network interface when launching an instance.
--
-- 'deviceIndex', 'instanceNetworkInterfaceSpecification_deviceIndex' - The position of the network interface in the attachment order. A primary
-- network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must
-- specify the device index.
--
-- 'secondaryPrivateIpAddressCount', 'instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses. You can\'t specify this
-- option and specify more than one private IP address using the private IP
-- addresses option. You cannot specify this option if you\'re launching
-- more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
--
-- 'privateIpAddress', 'instanceNetworkInterfaceSpecification_privateIpAddress' - The private IPv4 address of the network interface. Applies only if
-- creating a network interface when launching an instance. You cannot
-- specify this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
newInstanceNetworkInterfaceSpecification ::
  InstanceNetworkInterfaceSpecification
newInstanceNetworkInterfaceSpecification =
  InstanceNetworkInterfaceSpecification'
    { groups =
        Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      associateCarrierIpAddress =
        Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The IDs of the security groups for the network interface. Applies only
-- if creating a network interface when launching an instance.
instanceNetworkInterfaceSpecification_groups :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [Prelude.Text])
instanceNetworkInterfaceSpecification_groups = Lens.lens (\InstanceNetworkInterfaceSpecification' {groups} -> groups) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {groups = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more private IPv4 addresses to assign to the network interface.
-- Only one private IPv4 address can be designated as primary. You cannot
-- specify this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
instanceNetworkInterfaceSpecification_privateIpAddresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [PrivateIpAddressSpecification])
instanceNetworkInterfaceSpecification_privateIpAddresses = Lens.lens (\InstanceNetworkInterfaceSpecification' {privateIpAddresses} -> privateIpAddresses) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddresses = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more IPv6 addresses to assign to the network interface. You
-- cannot specify this option and the option to assign a number of IPv6
-- addresses in the same request. You cannot specify this option if you\'ve
-- specified a minimum number of instances to launch.
instanceNetworkInterfaceSpecification_ipv6Addresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe [InstanceIpv6Address])
instanceNetworkInterfaceSpecification_ipv6Addresses = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv6Addresses} -> ipv6Addresses) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv6Addresses = a} :: InstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of network interface.
--
-- To create an Elastic Fabric Adapter (EFA), specify @efa@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you are not creating an EFA, specify @interface@ or omit this
-- parameter.
--
-- Valid values: @interface@ | @efa@
instanceNetworkInterfaceSpecification_interfaceType :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_interfaceType = Lens.lens (\InstanceNetworkInterfaceSpecification' {interfaceType} -> interfaceType) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {interfaceType = a} :: InstanceNetworkInterfaceSpecification)

-- | Indicates whether to assign a public IPv4 address to an instance you
-- launch in a VPC. The public IP address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
instanceNetworkInterfaceSpecification_associatePublicIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceSpecification_associatePublicIpAddress = Lens.lens (\InstanceNetworkInterfaceSpecification' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {associatePublicIpAddress = a} :: InstanceNetworkInterfaceSpecification)

-- | Indicates whether to assign a carrier IP address to the network
-- interface.
--
-- You can only assign a carrier IP address to a network interface that is
-- in a subnet in a Wavelength Zone. For more information about carrier IP
-- addresses, see Carrier IP addresses in the AWS Wavelength Developer
-- Guide.
instanceNetworkInterfaceSpecification_associateCarrierIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceSpecification_associateCarrierIpAddress = Lens.lens (\InstanceNetworkInterfaceSpecification' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {associateCarrierIpAddress = a} :: InstanceNetworkInterfaceSpecification)

-- | A number of IPv6 addresses to assign to the network interface. Amazon
-- EC2 chooses the IPv6 addresses from the range of the subnet. You cannot
-- specify this option and the option to assign specific IPv6 addresses in
-- the same request. You can specify this option if you\'ve specified a
-- minimum number of instances to launch.
instanceNetworkInterfaceSpecification_ipv6AddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_ipv6AddressCount = Lens.lens (\InstanceNetworkInterfaceSpecification' {ipv6AddressCount} -> ipv6AddressCount) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {ipv6AddressCount = a} :: InstanceNetworkInterfaceSpecification)

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
instanceNetworkInterfaceSpecification_networkCardIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_networkCardIndex = Lens.lens (\InstanceNetworkInterfaceSpecification' {networkCardIndex} -> networkCardIndex) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceSpecification)

-- | If set to @true@, the interface is deleted when the instance is
-- terminated. You can specify @true@ only if creating a new network
-- interface when launching an instance.
instanceNetworkInterfaceSpecification_deleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceSpecification_deleteOnTermination = Lens.lens (\InstanceNetworkInterfaceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceSpecification)

-- | The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t
-- specify a network interface ID in a launch specification.
instanceNetworkInterfaceSpecification_networkInterfaceId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_networkInterfaceId = Lens.lens (\InstanceNetworkInterfaceSpecification' {networkInterfaceId} -> networkInterfaceId) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {networkInterfaceId = a} :: InstanceNetworkInterfaceSpecification)

-- | The ID of the subnet associated with the network interface. Applies only
-- if creating a network interface when launching an instance.
instanceNetworkInterfaceSpecification_subnetId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_subnetId = Lens.lens (\InstanceNetworkInterfaceSpecification' {subnetId} -> subnetId) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {subnetId = a} :: InstanceNetworkInterfaceSpecification)

-- | The description of the network interface. Applies only if creating a
-- network interface when launching an instance.
instanceNetworkInterfaceSpecification_description :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_description = Lens.lens (\InstanceNetworkInterfaceSpecification' {description} -> description) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {description = a} :: InstanceNetworkInterfaceSpecification)

-- | The position of the network interface in the attachment order. A primary
-- network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must
-- specify the device index.
instanceNetworkInterfaceSpecification_deviceIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_deviceIndex = Lens.lens (\InstanceNetworkInterfaceSpecification' {deviceIndex} -> deviceIndex) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {deviceIndex = a} :: InstanceNetworkInterfaceSpecification)

-- | The number of secondary private IPv4 addresses. You can\'t specify this
-- option and specify more than one private IP address using the private IP
-- addresses option. You cannot specify this option if you\'re launching
-- more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount = Lens.lens (\InstanceNetworkInterfaceSpecification' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {secondaryPrivateIpAddressCount = a} :: InstanceNetworkInterfaceSpecification)

-- | The private IPv4 address of the network interface. Applies only if
-- creating a network interface when launching an instance. You cannot
-- specify this option if you\'re launching more than one instance in a
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances>
-- request.
instanceNetworkInterfaceSpecification_privateIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceSpecification_privateIpAddress = Lens.lens (\InstanceNetworkInterfaceSpecification' {privateIpAddress} -> privateIpAddress) (\s@InstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddress = a} :: InstanceNetworkInterfaceSpecification)

instance
  Prelude.FromXML
    InstanceNetworkInterfaceSpecification
  where
  parseXML x =
    InstanceNetworkInterfaceSpecification'
      Prelude.<$> ( x Prelude..@? "SecurityGroupId"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "SecurityGroupId")
                  )
      Prelude.<*> ( x Prelude..@? "privateIpAddressesSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "ipv6AddressesSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "InterfaceType")
      Prelude.<*> (x Prelude..@? "associatePublicIpAddress")
      Prelude.<*> (x Prelude..@? "AssociateCarrierIpAddress")
      Prelude.<*> (x Prelude..@? "ipv6AddressCount")
      Prelude.<*> (x Prelude..@? "NetworkCardIndex")
      Prelude.<*> (x Prelude..@? "deleteOnTermination")
      Prelude.<*> (x Prelude..@? "networkInterfaceId")
      Prelude.<*> (x Prelude..@? "subnetId")
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "deviceIndex")
      Prelude.<*> (x Prelude..@? "secondaryPrivateIpAddressCount")
      Prelude.<*> (x Prelude..@? "privateIpAddress")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceSpecification

instance
  Prelude.NFData
    InstanceNetworkInterfaceSpecification

instance
  Prelude.ToQuery
    InstanceNetworkInterfaceSpecification
  where
  toQuery InstanceNetworkInterfaceSpecification' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "SecurityGroupId"
              Prelude.<$> groups
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "PrivateIpAddresses"
              Prelude.<$> privateIpAddresses
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        "InterfaceType" Prelude.=: interfaceType,
        "AssociatePublicIpAddress"
          Prelude.=: associatePublicIpAddress,
        "AssociateCarrierIpAddress"
          Prelude.=: associateCarrierIpAddress,
        "Ipv6AddressCount" Prelude.=: ipv6AddressCount,
        "NetworkCardIndex" Prelude.=: networkCardIndex,
        "DeleteOnTermination" Prelude.=: deleteOnTermination,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId,
        "SubnetId" Prelude.=: subnetId,
        "Description" Prelude.=: description,
        "DeviceIndex" Prelude.=: deviceIndex,
        "SecondaryPrivateIpAddressCount"
          Prelude.=: secondaryPrivateIpAddressCount,
        "PrivateIpAddress" Prelude.=: privateIpAddress
      ]
