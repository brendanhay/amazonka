{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  ( LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,

    -- * Lenses
    ltinisrGroups,
    ltinisrPrivateIPAddresses,
    ltinisrDeleteOnTermination,
    ltinisrAssociateCarrierIPAddress,
    ltinisrAssociatePublicIPAddress,
    ltinisrInterfaceType,
    ltinisrNetworkInterfaceId,
    ltinisrSubnetId,
    ltinisrIPv6AddressCount,
    ltinisrNetworkCardIndex,
    ltinisrPrivateIPAddress,
    ltinisrSecondaryPrivateIPAddressCount,
    ltinisrDescription,
    ltinisrDeviceIndex,
    ltinisrIPv6Addresses,
  )
where

import Network.AWS.EC2.Types.InstanceIPv6AddressRequest
import Network.AWS.EC2.Types.PrivateIPAddressSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The parameters for a network interface.
--
-- /See:/ 'mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { -- | The IDs of one or more security groups.
    groups :: Lude.Maybe [Lude.Text],
    -- | One or more private IPv4 addresses.
    privateIPAddresses :: Lude.Maybe [PrivateIPAddressSpecification],
    -- | Indicates whether the network interface is deleted when the instance is terminated.
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    -- | Associates a Carrier IP address with eth0 for a new network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
    associateCarrierIPAddress :: Lude.Maybe Lude.Bool,
    -- | Associates a public IPv4 address with eth0 for a new network interface.
    associatePublicIPAddress :: Lude.Maybe Lude.Bool,
    -- | The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- If you are not creating an EFA, specify @interface@ or omit this parameter.
    -- Valid values: @interface@ | @efa@
    interfaceType :: Lude.Maybe Lude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet for the network interface.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
    ipv6AddressCount :: Lude.Maybe Lude.Int,
    -- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
    networkCardIndex :: Lude.Maybe Lude.Int,
    -- | The primary private IPv4 address of the network interface.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The number of secondary private IPv4 addresses to assign to a network interface.
    secondaryPrivateIPAddressCount :: Lude.Maybe Lude.Int,
    -- | A description for the network interface.
    description :: Lude.Maybe Lude.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Lude.Maybe Lude.Int,
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
    ipv6Addresses :: Lude.Maybe [InstanceIPv6AddressRequest]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' with the minimum fields required to make a request.
--
-- * 'groups' - The IDs of one or more security groups.
-- * 'privateIPAddresses' - One or more private IPv4 addresses.
-- * 'deleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
-- * 'associateCarrierIPAddress' - Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
-- * 'associatePublicIPAddress' - Associates a public IPv4 address with eth0 for a new network interface.
-- * 'interfaceType' - The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'subnetId' - The ID of the subnet for the network interface.
-- * 'ipv6AddressCount' - The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
-- * 'networkCardIndex' - The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
-- * 'privateIPAddress' - The primary private IPv4 address of the network interface.
-- * 'secondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses to assign to a network interface.
-- * 'description' - A description for the network interface.
-- * 'deviceIndex' - The device index for the network interface attachment.
-- * 'ipv6Addresses' - One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest ::
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { groups =
        Lude.Nothing,
      privateIPAddresses = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      associateCarrierIPAddress =
        Lude.Nothing,
      associatePublicIPAddress =
        Lude.Nothing,
      interfaceType = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      ipv6AddressCount = Lude.Nothing,
      networkCardIndex = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      secondaryPrivateIPAddressCount =
        Lude.Nothing,
      description = Lude.Nothing,
      deviceIndex = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrGroups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe [Lude.Text])
ltinisrGroups = Lens.lens (groups :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrPrivateIPAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe [PrivateIPAddressSpecification])
ltinisrPrivateIPAddresses = Lens.lens (privateIPAddresses :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe [PrivateIPAddressSpecification]) (\s a -> s {privateIPAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDeleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Bool)
ltinisrDeleteOnTermination = Lens.lens (deleteOnTermination :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
--
-- /Note:/ Consider using 'associateCarrierIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrAssociateCarrierIPAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Bool)
ltinisrAssociateCarrierIPAddress = Lens.lens (associateCarrierIPAddress :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Bool) (\s a -> s {associateCarrierIPAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrAssociateCarrierIPAddress "Use generic-lens or generic-optics with 'associateCarrierIPAddress' instead." #-}

-- | Associates a public IPv4 address with eth0 for a new network interface.
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrAssociatePublicIPAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Bool)
ltinisrAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrInterfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Text)
ltinisrInterfaceType = Lens.lens (interfaceType :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrNetworkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Text)
ltinisrNetworkInterfaceId = Lens.lens (networkInterfaceId :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet for the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrSubnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Text)
ltinisrSubnetId = Lens.lens (subnetId :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrIPv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Int)
ltinisrIPv6AddressCount = Lens.lens (ipv6AddressCount :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrNetworkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Int)
ltinisrNetworkCardIndex = Lens.lens (networkCardIndex :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrPrivateIPAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Text)
ltinisrPrivateIPAddress = Lens.lens (privateIPAddress :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The number of secondary private IPv4 addresses to assign to a network interface.
--
-- /Note:/ Consider using 'secondaryPrivateIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrSecondaryPrivateIPAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Int)
ltinisrSecondaryPrivateIPAddressCount = Lens.lens (secondaryPrivateIPAddressCount :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {secondaryPrivateIPAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrSecondaryPrivateIPAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIPAddressCount' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDescription :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Text)
ltinisrDescription = Lens.lens (description :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The device index for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDeviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe Lude.Int)
ltinisrDeviceIndex = Lens.lens (deviceIndex :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrIPv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Lude.Maybe [InstanceIPv6AddressRequest])
ltinisrIPv6Addresses = Lens.lens (ipv6Addresses :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest -> Lude.Maybe [InstanceIPv6AddressRequest]) (\s a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest)
{-# DEPRECATED ltinisrIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance
  Lude.ToQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  where
  toQuery
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' {..} =
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
