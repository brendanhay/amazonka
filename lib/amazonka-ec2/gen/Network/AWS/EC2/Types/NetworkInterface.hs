{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niGroups,
    niStatus,
    niPrivateIPAddresses,
    niSourceDestCheck,
    niInterfaceType,
    niVPCId,
    niTagSet,
    niRequesterManaged,
    niOutpostARN,
    niNetworkInterfaceId,
    niSubnetId,
    niMACAddress,
    niAttachment,
    niOwnerId,
    niAvailabilityZone,
    niPrivateIPAddress,
    niPrivateDNSName,
    niRequesterId,
    niDescription,
    niAssociation,
    niIPv6Addresses,
  )
where

import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceIPv6Address
import Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { groups ::
      Lude.Maybe [GroupIdentifier],
    status :: Lude.Maybe NetworkInterfaceStatus,
    privateIPAddresses ::
      Lude.Maybe [NetworkInterfacePrivateIPAddress],
    sourceDestCheck :: Lude.Maybe Lude.Bool,
    interfaceType :: Lude.Maybe NetworkInterfaceType,
    vpcId :: Lude.Maybe Lude.Text,
    tagSet :: Lude.Maybe [Tag],
    requesterManaged :: Lude.Maybe Lude.Bool,
    outpostARN :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    mACAddress :: Lude.Maybe Lude.Text,
    attachment :: Lude.Maybe NetworkInterfaceAttachment,
    ownerId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    privateIPAddress :: Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text,
    requesterId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    association :: Lude.Maybe NetworkInterfaceAssociation,
    ipv6Addresses :: Lude.Maybe [NetworkInterfaceIPv6Address]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- * 'association' - The association information for an Elastic IP address (IPv4) associated with the network interface.
-- * 'attachment' - The network interface attachment.
-- * 'availabilityZone' - The Availability Zone.
-- * 'description' - A description.
-- * 'groups' - Any security groups for the network interface.
-- * 'interfaceType' - The type of network interface.
-- * 'ipv6Addresses' - The IPv6 addresses associated with the network interface.
-- * 'mACAddress' - The MAC address.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'ownerId' - The AWS account ID of the owner of the network interface.
-- * 'privateDNSName' - The private DNS name.
-- * 'privateIPAddress' - The IPv4 address of the network interface within the subnet.
-- * 'privateIPAddresses' - The private IPv4 addresses associated with the network interface.
-- * 'requesterId' - The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
-- * 'requesterManaged' - Indicates whether the network interface is being managed by AWS.
-- * 'sourceDestCheck' - Indicates whether traffic to or from the instance is validated.
-- * 'status' - The status of the network interface.
-- * 'subnetId' - The ID of the subnet.
-- * 'tagSet' - Any tags assigned to the network interface.
-- * 'vpcId' - The ID of the VPC.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { groups = Lude.Nothing,
      status = Lude.Nothing,
      privateIPAddresses = Lude.Nothing,
      sourceDestCheck = Lude.Nothing,
      interfaceType = Lude.Nothing,
      vpcId = Lude.Nothing,
      tagSet = Lude.Nothing,
      requesterManaged = Lude.Nothing,
      outpostARN = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      mACAddress = Lude.Nothing,
      attachment = Lude.Nothing,
      ownerId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      requesterId = Lude.Nothing,
      description = Lude.Nothing,
      association = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | Any security groups for the network interface.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niGroups :: Lens.Lens' NetworkInterface (Lude.Maybe [GroupIdentifier])
niGroups = Lens.lens (groups :: NetworkInterface -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: NetworkInterface)
{-# DEPRECATED niGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The status of the network interface.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niStatus :: Lens.Lens' NetworkInterface (Lude.Maybe NetworkInterfaceStatus)
niStatus = Lens.lens (status :: NetworkInterface -> Lude.Maybe NetworkInterfaceStatus) (\s a -> s {status = a} :: NetworkInterface)
{-# DEPRECATED niStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The private IPv4 addresses associated with the network interface.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPAddresses :: Lens.Lens' NetworkInterface (Lude.Maybe [NetworkInterfacePrivateIPAddress])
niPrivateIPAddresses = Lens.lens (privateIPAddresses :: NetworkInterface -> Lude.Maybe [NetworkInterfacePrivateIPAddress]) (\s a -> s {privateIPAddresses = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | Indicates whether traffic to or from the instance is validated.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSourceDestCheck :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Bool)
niSourceDestCheck = Lens.lens (sourceDestCheck :: NetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {sourceDestCheck = a} :: NetworkInterface)
{-# DEPRECATED niSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | The type of network interface.
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niInterfaceType :: Lens.Lens' NetworkInterface (Lude.Maybe NetworkInterfaceType)
niInterfaceType = Lens.lens (interfaceType :: NetworkInterface -> Lude.Maybe NetworkInterfaceType) (\s a -> s {interfaceType = a} :: NetworkInterface)
{-# DEPRECATED niInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niVPCId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niVPCId = Lens.lens (vpcId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: NetworkInterface)
{-# DEPRECATED niVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Any tags assigned to the network interface.
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niTagSet :: Lens.Lens' NetworkInterface (Lude.Maybe [Tag])
niTagSet = Lens.lens (tagSet :: NetworkInterface -> Lude.Maybe [Tag]) (\s a -> s {tagSet = a} :: NetworkInterface)
{-# DEPRECATED niTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

-- | Indicates whether the network interface is being managed by AWS.
--
-- /Note:/ Consider using 'requesterManaged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niRequesterManaged :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Bool)
niRequesterManaged = Lens.lens (requesterManaged :: NetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {requesterManaged = a} :: NetworkInterface)
{-# DEPRECATED niRequesterManaged "Use generic-lens or generic-optics with 'requesterManaged' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niOutpostARN :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niOutpostARN = Lens.lens (outpostARN :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: NetworkInterface)
{-# DEPRECATED niOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niNetworkInterfaceId = Lens.lens (networkInterfaceId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: NetworkInterface)
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niSubnetId = Lens.lens (subnetId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: NetworkInterface)
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The MAC address.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMACAddress :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niMACAddress = Lens.lens (mACAddress :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: NetworkInterface)
{-# DEPRECATED niMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | The network interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAttachment :: Lens.Lens' NetworkInterface (Lude.Maybe NetworkInterfaceAttachment)
niAttachment = Lens.lens (attachment :: NetworkInterface -> Lude.Maybe NetworkInterfaceAttachment) (\s a -> s {attachment = a} :: NetworkInterface)
{-# DEPRECATED niAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | The AWS account ID of the owner of the network interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niOwnerId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niOwnerId = Lens.lens (ownerId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: NetworkInterface)
{-# DEPRECATED niOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAvailabilityZone :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niAvailabilityZone = Lens.lens (availabilityZone :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: NetworkInterface)
{-# DEPRECATED niAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPAddress :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateIPAddress = Lens.lens (privateIPAddress :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateDNSName :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateDNSName = Lens.lens (privateDNSName :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: NetworkInterface)
{-# DEPRECATED niPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- /Note:/ Consider using 'requesterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niRequesterId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niRequesterId = Lens.lens (requesterId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {requesterId = a} :: NetworkInterface)
{-# DEPRECATED niRequesterId "Use generic-lens or generic-optics with 'requesterId' instead." #-}

-- | A description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niDescription :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niDescription = Lens.lens (description :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: NetworkInterface)
{-# DEPRECATED niDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAssociation :: Lens.Lens' NetworkInterface (Lude.Maybe NetworkInterfaceAssociation)
niAssociation = Lens.lens (association :: NetworkInterface -> Lude.Maybe NetworkInterfaceAssociation) (\s a -> s {association = a} :: NetworkInterface)
{-# DEPRECATED niAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The IPv6 addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv6Addresses :: Lens.Lens' NetworkInterface (Lude.Maybe [NetworkInterfaceIPv6Address])
niIPv6Addresses = Lens.lens (ipv6Addresses :: NetworkInterface -> Lude.Maybe [NetworkInterfaceIPv6Address]) (\s a -> s {ipv6Addresses = a} :: NetworkInterface)
{-# DEPRECATED niIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
      Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "status")
      Lude.<*> ( x Lude..@? "privateIpAddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "sourceDestCheck")
      Lude.<*> (x Lude..@? "interfaceType")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "requesterManaged")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "macAddress")
      Lude.<*> (x Lude..@? "attachment")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> (x Lude..@? "requesterId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "association")
      Lude.<*> ( x Lude..@? "ipv6AddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
