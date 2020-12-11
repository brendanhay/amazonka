-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
  ( ScheduledInstancesNetworkInterface (..),

    -- * Smart constructor
    mkScheduledInstancesNetworkInterface,

    -- * Lenses
    siniGroups,
    siniDeleteOnTermination,
    siniAssociatePublicIPAddress,
    siniPrivateIPAddressConfigs,
    siniNetworkInterfaceId,
    siniSubnetId,
    siniIPv6AddressCount,
    siniPrivateIPAddress,
    siniSecondaryPrivateIPAddressCount,
    siniDescription,
    siniDeviceIndex,
    siniIPv6Addresses,
  )
where

import Network.AWS.EC2.Types.ScheduledInstancesIPv6Address
import Network.AWS.EC2.Types.ScheduledInstancesPrivateIPAddressConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { groups ::
      Lude.Maybe
        [Lude.Text],
    deleteOnTermination ::
      Lude.Maybe Lude.Bool,
    associatePublicIPAddress ::
      Lude.Maybe Lude.Bool,
    privateIPAddressConfigs ::
      Lude.Maybe
        [ScheduledInstancesPrivateIPAddressConfig],
    networkInterfaceId ::
      Lude.Maybe Lude.Text,
    subnetId ::
      Lude.Maybe Lude.Text,
    ipv6AddressCount ::
      Lude.Maybe Lude.Int,
    privateIPAddress ::
      Lude.Maybe Lude.Text,
    secondaryPrivateIPAddressCount ::
      Lude.Maybe Lude.Int,
    description ::
      Lude.Maybe Lude.Text,
    deviceIndex ::
      Lude.Maybe Lude.Int,
    ipv6Addresses ::
      Lude.Maybe
        [ScheduledInstancesIPv6Address]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduledInstancesNetworkInterface' with the minimum fields required to make a request.
--
-- * 'associatePublicIPAddress' - Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
-- * 'deleteOnTermination' - Indicates whether to delete the interface when the instance is terminated.
-- * 'description' - The description.
-- * 'deviceIndex' - The index of the device for the network interface attachment.
-- * 'groups' - The IDs of the security groups.
-- * 'ipv6AddressCount' - The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
-- * 'ipv6Addresses' - The specific IPv6 addresses from the subnet range.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'privateIPAddress' - The IPv4 address of the network interface within the subnet.
-- * 'privateIPAddressConfigs' - The private IPv4 addresses.
-- * 'secondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses.
-- * 'subnetId' - The ID of the subnet.
mkScheduledInstancesNetworkInterface ::
  ScheduledInstancesNetworkInterface
mkScheduledInstancesNetworkInterface =
  ScheduledInstancesNetworkInterface'
    { groups = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      associatePublicIPAddress = Lude.Nothing,
      privateIPAddressConfigs = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      ipv6AddressCount = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      secondaryPrivateIPAddressCount = Lude.Nothing,
      description = Lude.Nothing,
      deviceIndex = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | The IDs of the security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniGroups :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe [Lude.Text])
siniGroups = Lens.lens (groups :: ScheduledInstancesNetworkInterface -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Indicates whether to delete the interface when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniDeleteOnTermination :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Bool)
siniDeleteOnTermination = Lens.lens (deleteOnTermination :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniAssociatePublicIPAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Bool)
siniAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | The private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIPAddressConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniPrivateIPAddressConfigs :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe [ScheduledInstancesPrivateIPAddressConfig])
siniPrivateIPAddressConfigs = Lens.lens (privateIPAddressConfigs :: ScheduledInstancesNetworkInterface -> Lude.Maybe [ScheduledInstancesPrivateIPAddressConfig]) (\s a -> s {privateIPAddressConfigs = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniPrivateIPAddressConfigs "Use generic-lens or generic-optics with 'privateIPAddressConfigs' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniNetworkInterfaceId :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Text)
siniNetworkInterfaceId = Lens.lens (networkInterfaceId :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniSubnetId :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Text)
siniSubnetId = Lens.lens (subnetId :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniIPv6AddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Int)
siniIPv6AddressCount = Lens.lens (ipv6AddressCount :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniPrivateIPAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Text)
siniPrivateIPAddress = Lens.lens (privateIPAddress :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The number of secondary private IPv4 addresses.
--
-- /Note:/ Consider using 'secondaryPrivateIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniSecondaryPrivateIPAddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Int)
siniSecondaryPrivateIPAddressCount = Lens.lens (secondaryPrivateIPAddressCount :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Int) (\s a -> s {secondaryPrivateIPAddressCount = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniSecondaryPrivateIPAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIPAddressCount' instead." #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniDescription :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Text)
siniDescription = Lens.lens (description :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The index of the device for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniDeviceIndex :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe Lude.Int)
siniDeviceIndex = Lens.lens (deviceIndex :: ScheduledInstancesNetworkInterface -> Lude.Maybe Lude.Int) (\s a -> s {deviceIndex = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | The specific IPv6 addresses from the subnet range.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniIPv6Addresses :: Lens.Lens' ScheduledInstancesNetworkInterface (Lude.Maybe [ScheduledInstancesIPv6Address])
siniIPv6Addresses = Lens.lens (ipv6Addresses :: ScheduledInstancesNetworkInterface -> Lude.Maybe [ScheduledInstancesIPv6Address]) (\s a -> s {ipv6Addresses = a} :: ScheduledInstancesNetworkInterface)
{-# DEPRECATED siniIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.ToQuery ScheduledInstancesNetworkInterface where
  toQuery ScheduledInstancesNetworkInterface' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Group" Lude.<$> groups),
        "DeleteOnTermination" Lude.=: deleteOnTermination,
        "AssociatePublicIpAddress" Lude.=: associatePublicIPAddress,
        Lude.toQuery
          ( Lude.toQueryList "PrivateIpAddressConfig"
              Lude.<$> privateIPAddressConfigs
          ),
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "SubnetId" Lude.=: subnetId,
        "Ipv6AddressCount" Lude.=: ipv6AddressCount,
        "PrivateIpAddress" Lude.=: privateIPAddress,
        "SecondaryPrivateIpAddressCount"
          Lude.=: secondaryPrivateIPAddressCount,
        "Description" Lude.=: description,
        "DeviceIndex" Lude.=: deviceIndex,
        Lude.toQuery
          (Lude.toQueryList "Ipv6Address" Lude.<$> ipv6Addresses)
      ]
