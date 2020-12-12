{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
  ( InstanceNetworkInterfaceSpecification (..),

    -- * Smart constructor
    mkInstanceNetworkInterfaceSpecification,

    -- * Lenses
    inisGroups,
    inisPrivateIPAddresses,
    inisDeleteOnTermination,
    inisAssociateCarrierIPAddress,
    inisAssociatePublicIPAddress,
    inisInterfaceType,
    inisNetworkInterfaceId,
    inisSubnetId,
    inisIPv6AddressCount,
    inisNetworkCardIndex,
    inisPrivateIPAddress,
    inisSecondaryPrivateIPAddressCount,
    inisDescription,
    inisDeviceIndex,
    inisIPv6Addresses,
  )
where

import Network.AWS.EC2.Types.InstanceIPv6Address
import Network.AWS.EC2.Types.PrivateIPAddressSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface.
--
-- /See:/ 'mkInstanceNetworkInterfaceSpecification' smart constructor.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'
  { groups ::
      Lude.Maybe
        [Lude.Text],
    privateIPAddresses ::
      Lude.Maybe
        [PrivateIPAddressSpecification],
    deleteOnTermination ::
      Lude.Maybe
        Lude.Bool,
    associateCarrierIPAddress ::
      Lude.Maybe
        Lude.Bool,
    associatePublicIPAddress ::
      Lude.Maybe
        Lude.Bool,
    interfaceType ::
      Lude.Maybe
        Lude.Text,
    networkInterfaceId ::
      Lude.Maybe
        Lude.Text,
    subnetId ::
      Lude.Maybe
        Lude.Text,
    ipv6AddressCount ::
      Lude.Maybe
        Lude.Int,
    networkCardIndex ::
      Lude.Maybe
        Lude.Int,
    privateIPAddress ::
      Lude.Maybe
        Lude.Text,
    secondaryPrivateIPAddressCount ::
      Lude.Maybe
        Lude.Int,
    description ::
      Lude.Maybe
        Lude.Text,
    deviceIndex ::
      Lude.Maybe
        Lude.Int,
    ipv6Addresses ::
      Lude.Maybe
        [InstanceIPv6Address]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceNetworkInterfaceSpecification' with the minimum fields required to make a request.
--
-- * 'associateCarrierIPAddress' - Indicates whether to assign a carrier IP address to the network interface.
--
-- You can only assign a carrier IP address to a network interface that is in a subnet in a Wavelength Zone. For more information about carrier IP addresses, see Carrier IP addresses in the AWS Wavelength Developer Guide.
-- * 'associatePublicIPAddress' - Indicates whether to assign a public IPv4 address to an instance you launch in a VPC. The public IP address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
-- * 'deleteOnTermination' - If set to @true@ , the interface is deleted when the instance is terminated. You can specify @true@ only if creating a new network interface when launching an instance.
-- * 'description' - The description of the network interface. Applies only if creating a network interface when launching an instance.
-- * 'deviceIndex' - The position of the network interface in the attachment order. A primary network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must specify the device index.
-- * 'groups' - The IDs of the security groups for the network interface. Applies only if creating a network interface when launching an instance.
-- * 'interfaceType' - The type of network interface.
--
-- To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@
-- * 'ipv6AddressCount' - A number of IPv6 addresses to assign to the network interface. Amazon EC2 chooses the IPv6 addresses from the range of the subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
-- * 'ipv6Addresses' - One or more IPv6 addresses to assign to the network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
-- * 'networkCardIndex' - The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
-- * 'networkInterfaceId' - The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t specify a network interface ID in a launch specification.
-- * 'privateIPAddress' - The private IPv4 address of the network interface. Applies only if creating a network interface when launching an instance. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
-- * 'privateIPAddresses' - One or more private IPv4 addresses to assign to the network interface. Only one private IPv4 address can be designated as primary. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
-- * 'secondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses. You can't specify this option and specify more than one private IP address using the private IP addresses option. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
-- * 'subnetId' - The ID of the subnet associated with the network interface. Applies only if creating a network interface when launching an instance.
mkInstanceNetworkInterfaceSpecification ::
  InstanceNetworkInterfaceSpecification
mkInstanceNetworkInterfaceSpecification =
  InstanceNetworkInterfaceSpecification'
    { groups = Lude.Nothing,
      privateIPAddresses = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      associateCarrierIPAddress = Lude.Nothing,
      associatePublicIPAddress = Lude.Nothing,
      interfaceType = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      ipv6AddressCount = Lude.Nothing,
      networkCardIndex = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      secondaryPrivateIPAddressCount = Lude.Nothing,
      description = Lude.Nothing,
      deviceIndex = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | The IDs of the security groups for the network interface. Applies only if creating a network interface when launching an instance.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisGroups :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe [Lude.Text])
inisGroups = Lens.lens (groups :: InstanceNetworkInterfaceSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | One or more private IPv4 addresses to assign to the network interface. Only one private IPv4 address can be designated as primary. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisPrivateIPAddresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe [PrivateIPAddressSpecification])
inisPrivateIPAddresses = Lens.lens (privateIPAddresses :: InstanceNetworkInterfaceSpecification -> Lude.Maybe [PrivateIPAddressSpecification]) (\s a -> s {privateIPAddresses = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | If set to @true@ , the interface is deleted when the instance is terminated. You can specify @true@ only if creating a new network interface when launching an instance.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisDeleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Bool)
inisDeleteOnTermination = Lens.lens (deleteOnTermination :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | Indicates whether to assign a carrier IP address to the network interface.
--
-- You can only assign a carrier IP address to a network interface that is in a subnet in a Wavelength Zone. For more information about carrier IP addresses, see Carrier IP addresses in the AWS Wavelength Developer Guide.
--
-- /Note:/ Consider using 'associateCarrierIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisAssociateCarrierIPAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Bool)
inisAssociateCarrierIPAddress = Lens.lens (associateCarrierIPAddress :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {associateCarrierIPAddress = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisAssociateCarrierIPAddress "Use generic-lens or generic-optics with 'associateCarrierIPAddress' instead." #-}

-- | Indicates whether to assign a public IPv4 address to an instance you launch in a VPC. The public IP address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisAssociatePublicIPAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Bool)
inisAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | The type of network interface.
--
-- To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisInterfaceType :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
inisInterfaceType = Lens.lens (interfaceType :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {interfaceType = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t specify a network interface ID in a launch specification.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisNetworkInterfaceId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
inisNetworkInterfaceId = Lens.lens (networkInterfaceId :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet associated with the network interface. Applies only if creating a network interface when launching an instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisSubnetId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
inisSubnetId = Lens.lens (subnetId :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | A number of IPv6 addresses to assign to the network interface. Amazon EC2 chooses the IPv6 addresses from the range of the subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisIPv6AddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
inisIPv6AddressCount = Lens.lens (ipv6AddressCount :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisNetworkCardIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
inisNetworkCardIndex = Lens.lens (networkCardIndex :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The private IPv4 address of the network interface. Applies only if creating a network interface when launching an instance. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisPrivateIPAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
inisPrivateIPAddress = Lens.lens (privateIPAddress :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The number of secondary private IPv4 addresses. You can't specify this option and specify more than one private IP address using the private IP addresses option. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
--
-- /Note:/ Consider using 'secondaryPrivateIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisSecondaryPrivateIPAddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
inisSecondaryPrivateIPAddressCount = Lens.lens (secondaryPrivateIPAddressCount :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {secondaryPrivateIPAddressCount = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisSecondaryPrivateIPAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIPAddressCount' instead." #-}

-- | The description of the network interface. Applies only if creating a network interface when launching an instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisDescription :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
inisDescription = Lens.lens (description :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The position of the network interface in the attachment order. A primary network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must specify the device index.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisDeviceIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
inisDeviceIndex = Lens.lens (deviceIndex :: InstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {deviceIndex = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | One or more IPv6 addresses to assign to the network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisIPv6Addresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Lude.Maybe [InstanceIPv6Address])
inisIPv6Addresses = Lens.lens (ipv6Addresses :: InstanceNetworkInterfaceSpecification -> Lude.Maybe [InstanceIPv6Address]) (\s a -> s {ipv6Addresses = a} :: InstanceNetworkInterfaceSpecification)
{-# DEPRECATED inisIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.FromXML InstanceNetworkInterfaceSpecification where
  parseXML x =
    InstanceNetworkInterfaceSpecification'
      Lude.<$> ( x Lude..@? "SecurityGroupId" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "SecurityGroupId")
               )
      Lude.<*> ( x Lude..@? "privateIpAddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "AssociateCarrierIpAddress")
      Lude.<*> (x Lude..@? "associatePublicIpAddress")
      Lude.<*> (x Lude..@? "InterfaceType")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "ipv6AddressCount")
      Lude.<*> (x Lude..@? "NetworkCardIndex")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "secondaryPrivateIpAddressCount")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "deviceIndex")
      Lude.<*> ( x Lude..@? "ipv6AddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )

instance Lude.ToQuery InstanceNetworkInterfaceSpecification where
  toQuery InstanceNetworkInterfaceSpecification' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "SecurityGroupId" Lude.<$> groups),
        Lude.toQuery
          ( Lude.toQueryList "PrivateIpAddresses"
              Lude.<$> privateIPAddresses
          ),
        "DeleteOnTermination" Lude.=: deleteOnTermination,
        "AssociateCarrierIpAddress" Lude.=: associateCarrierIPAddress,
        "AssociatePublicIpAddress" Lude.=: associatePublicIPAddress,
        "InterfaceType" Lude.=: interfaceType,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "SubnetId" Lude.=: subnetId,
        "Ipv6AddressCount" Lude.=: ipv6AddressCount,
        "NetworkCardIndex" Lude.=: networkCardIndex,
        "PrivateIpAddress" Lude.=: privateIPAddress,
        "SecondaryPrivateIpAddressCount"
          Lude.=: secondaryPrivateIPAddressCount,
        "Description" Lude.=: description,
        "DeviceIndex" Lude.=: deviceIndex,
        Lude.toQuery
          (Lude.toQueryList "Ipv6Addresses" Lude.<$> ipv6Addresses)
      ]
