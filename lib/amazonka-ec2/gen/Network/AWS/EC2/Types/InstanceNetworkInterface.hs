{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterface
  ( InstanceNetworkInterface (..),

    -- * Smart constructor
    mkInstanceNetworkInterface,

    -- * Lenses
    iniGroups,
    iniStatus,
    iniPrivateIPAddresses,
    iniSourceDestCheck,
    iniInterfaceType,
    iniVPCId,
    iniNetworkInterfaceId,
    iniSubnetId,
    iniMACAddress,
    iniAttachment,
    iniOwnerId,
    iniPrivateIPAddress,
    iniPrivateDNSName,
    iniDescription,
    iniAssociation,
    iniIPv6Addresses,
  )
where

import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.InstanceIPv6Address
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
import Network.AWS.EC2.Types.InstancePrivateIPAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface.
--
-- /See:/ 'mkInstanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { -- | One or more security groups.
    groups :: Lude.Maybe [GroupIdentifier],
    -- | The status of the network interface.
    status :: Lude.Maybe NetworkInterfaceStatus,
    -- | One or more private IPv4 addresses associated with the network interface.
    privateIPAddresses :: Lude.Maybe [InstancePrivateIPAddress],
    -- | Indicates whether to validate network traffic to or from this network interface.
    sourceDestCheck :: Lude.Maybe Lude.Bool,
    -- | Describes the type of network interface.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Lude.Maybe Lude.Text,
    -- | The ID of the VPC.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The MAC address.
    mACAddress :: Lude.Maybe Lude.Text,
    -- | The network interface attachment.
    attachment :: Lude.Maybe InstanceNetworkInterfaceAttachment,
    -- | The ID of the AWS account that created the network interface.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The IPv4 address of the network interface within the subnet.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The private DNS name.
    privateDNSName :: Lude.Maybe Lude.Text,
    -- | The description.
    description :: Lude.Maybe Lude.Text,
    -- | The association information for an Elastic IPv4 associated with the network interface.
    association :: Lude.Maybe InstanceNetworkInterfaceAssociation,
    -- | One or more IPv6 addresses associated with the network interface.
    ipv6Addresses :: Lude.Maybe [InstanceIPv6Address]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceNetworkInterface' with the minimum fields required to make a request.
--
-- * 'groups' - One or more security groups.
-- * 'status' - The status of the network interface.
-- * 'privateIPAddresses' - One or more private IPv4 addresses associated with the network interface.
-- * 'sourceDestCheck' - Indicates whether to validate network traffic to or from this network interface.
-- * 'interfaceType' - Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@
-- * 'vpcId' - The ID of the VPC.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'subnetId' - The ID of the subnet.
-- * 'mACAddress' - The MAC address.
-- * 'attachment' - The network interface attachment.
-- * 'ownerId' - The ID of the AWS account that created the network interface.
-- * 'privateIPAddress' - The IPv4 address of the network interface within the subnet.
-- * 'privateDNSName' - The private DNS name.
-- * 'description' - The description.
-- * 'association' - The association information for an Elastic IPv4 associated with the network interface.
-- * 'ipv6Addresses' - One or more IPv6 addresses associated with the network interface.
mkInstanceNetworkInterface ::
  InstanceNetworkInterface
mkInstanceNetworkInterface =
  InstanceNetworkInterface'
    { groups = Lude.Nothing,
      status = Lude.Nothing,
      privateIPAddresses = Lude.Nothing,
      sourceDestCheck = Lude.Nothing,
      interfaceType = Lude.Nothing,
      vpcId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      mACAddress = Lude.Nothing,
      attachment = Lude.Nothing,
      ownerId = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      description = Lude.Nothing,
      association = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | One or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniGroups :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe [GroupIdentifier])
iniGroups = Lens.lens (groups :: InstanceNetworkInterface -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The status of the network interface.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniStatus :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe NetworkInterfaceStatus)
iniStatus = Lens.lens (status :: InstanceNetworkInterface -> Lude.Maybe NetworkInterfaceStatus) (\s a -> s {status = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | One or more private IPv4 addresses associated with the network interface.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateIPAddresses :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe [InstancePrivateIPAddress])
iniPrivateIPAddresses = Lens.lens (privateIPAddresses :: InstanceNetworkInterface -> Lude.Maybe [InstancePrivateIPAddress]) (\s a -> s {privateIPAddresses = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | Indicates whether to validate network traffic to or from this network interface.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniSourceDestCheck :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Bool)
iniSourceDestCheck = Lens.lens (sourceDestCheck :: InstanceNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {sourceDestCheck = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniInterfaceType :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniInterfaceType = Lens.lens (interfaceType :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {interfaceType = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniVPCId :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniVPCId = Lens.lens (vpcId :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniNetworkInterfaceId :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniNetworkInterfaceId = Lens.lens (networkInterfaceId :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniSubnetId :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniSubnetId = Lens.lens (subnetId :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The MAC address.
--
-- /Note:/ Consider using 'mACAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniMACAddress :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniMACAddress = Lens.lens (mACAddress :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {mACAddress = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniMACAddress "Use generic-lens or generic-optics with 'mACAddress' instead." #-}

-- | The network interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniAttachment :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe InstanceNetworkInterfaceAttachment)
iniAttachment = Lens.lens (attachment :: InstanceNetworkInterface -> Lude.Maybe InstanceNetworkInterfaceAttachment) (\s a -> s {attachment = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | The ID of the AWS account that created the network interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniOwnerId :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniOwnerId = Lens.lens (ownerId :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateIPAddress :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniPrivateIPAddress = Lens.lens (privateIPAddress :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateDNSName :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniPrivateDNSName = Lens.lens (privateDNSName :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniDescription :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe Lude.Text)
iniDescription = Lens.lens (description :: InstanceNetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The association information for an Elastic IPv4 associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniAssociation :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe InstanceNetworkInterfaceAssociation)
iniAssociation = Lens.lens (association :: InstanceNetworkInterface -> Lude.Maybe InstanceNetworkInterfaceAssociation) (\s a -> s {association = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | One or more IPv6 addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniIPv6Addresses :: Lens.Lens' InstanceNetworkInterface (Lude.Maybe [InstanceIPv6Address])
iniIPv6Addresses = Lens.lens (ipv6Addresses :: InstanceNetworkInterface -> Lude.Maybe [InstanceIPv6Address]) (\s a -> s {ipv6Addresses = a} :: InstanceNetworkInterface)
{-# DEPRECATED iniIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.FromXML InstanceNetworkInterface where
  parseXML x =
    InstanceNetworkInterface'
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
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "macAddress")
      Lude.<*> (x Lude..@? "attachment")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "association")
      Lude.<*> ( x Lude..@? "ipv6AddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
