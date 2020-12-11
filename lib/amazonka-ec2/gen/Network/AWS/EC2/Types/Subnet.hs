-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Subnet
  ( Subnet (..),

    -- * Smart constructor
    mkSubnet,

    -- * Lenses
    subIPv6CidrBlockAssociationSet,
    subAvailabilityZoneId,
    subOutpostARN,
    subAssignIPv6AddressOnCreation,
    subSubnetARN,
    subOwnerId,
    subCustomerOwnedIPv4Pool,
    subMapCustomerOwnedIPOnLaunch,
    subMapPublicIPOnLaunch,
    subDefaultForAz,
    subTags,
    subAvailabilityZone,
    subAvailableIPAddressCount,
    subCidrBlock,
    subState,
    subSubnetId,
    subVPCId,
  )
where

import Network.AWS.EC2.Types.SubnetIPv6CidrBlockAssociation
import Network.AWS.EC2.Types.SubnetState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a subnet.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { ipv6CidrBlockAssociationSet ::
      Lude.Maybe [SubnetIPv6CidrBlockAssociation],
    availabilityZoneId :: Lude.Maybe Lude.Text,
    outpostARN :: Lude.Maybe Lude.Text,
    assignIPv6AddressOnCreation :: Lude.Maybe Lude.Bool,
    subnetARN :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    mapCustomerOwnedIPOnLaunch :: Lude.Maybe Lude.Bool,
    mapPublicIPOnLaunch :: Lude.Maybe Lude.Bool,
    defaultForAz :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag],
    availabilityZone :: Lude.Text,
    availableIPAddressCount :: Lude.Int,
    cidrBlock :: Lude.Text,
    state :: SubnetState,
    subnetId :: Lude.Text,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- * 'assignIPv6AddressOnCreation' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
-- * 'availabilityZone' - The Availability Zone of the subnet.
-- * 'availabilityZoneId' - The AZ ID of the subnet.
-- * 'availableIPAddressCount' - The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
-- * 'cidrBlock' - The IPv4 CIDR block assigned to the subnet.
-- * 'customerOwnedIPv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
-- * 'defaultForAz' - Indicates whether this is the default subnet for the Availability Zone.
-- * 'ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the subnet.
-- * 'mapCustomerOwnedIPOnLaunch' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
-- * 'mapPublicIPOnLaunch' - Indicates whether instances launched in this subnet receive a public IPv4 address.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'ownerId' - The ID of the AWS account that owns the subnet.
-- * 'state' - The current state of the subnet.
-- * 'subnetARN' - The Amazon Resource Name (ARN) of the subnet.
-- * 'subnetId' - The ID of the subnet.
-- * 'tags' - Any tags assigned to the subnet.
-- * 'vpcId' - The ID of the VPC the subnet is in.
mkSubnet ::
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'availableIPAddressCount'
  Lude.Int ->
  -- | 'cidrBlock'
  Lude.Text ->
  -- | 'state'
  SubnetState ->
  -- | 'subnetId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  Subnet
mkSubnet
  pAvailabilityZone_
  pAvailableIPAddressCount_
  pCidrBlock_
  pState_
  pSubnetId_
  pVPCId_ =
    Subnet'
      { ipv6CidrBlockAssociationSet = Lude.Nothing,
        availabilityZoneId = Lude.Nothing,
        outpostARN = Lude.Nothing,
        assignIPv6AddressOnCreation = Lude.Nothing,
        subnetARN = Lude.Nothing,
        ownerId = Lude.Nothing,
        customerOwnedIPv4Pool = Lude.Nothing,
        mapCustomerOwnedIPOnLaunch = Lude.Nothing,
        mapPublicIPOnLaunch = Lude.Nothing,
        defaultForAz = Lude.Nothing,
        tags = Lude.Nothing,
        availabilityZone = pAvailabilityZone_,
        availableIPAddressCount = pAvailableIPAddressCount_,
        cidrBlock = pCidrBlock_,
        state = pState_,
        subnetId = pSubnetId_,
        vpcId = pVPCId_
      }

-- | Information about the IPv6 CIDR blocks associated with the subnet.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subIPv6CidrBlockAssociationSet :: Lens.Lens' Subnet (Lude.Maybe [SubnetIPv6CidrBlockAssociation])
subIPv6CidrBlockAssociationSet = Lens.lens (ipv6CidrBlockAssociationSet :: Subnet -> Lude.Maybe [SubnetIPv6CidrBlockAssociation]) (\s a -> s {ipv6CidrBlockAssociationSet = a} :: Subnet)
{-# DEPRECATED subIPv6CidrBlockAssociationSet "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociationSet' instead." #-}

-- | The AZ ID of the subnet.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subAvailabilityZoneId :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
subAvailabilityZoneId = Lens.lens (availabilityZoneId :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: Subnet)
{-# DEPRECATED subAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subOutpostARN :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
subOutpostARN = Lens.lens (outpostARN :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: Subnet)
{-# DEPRECATED subOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
--
-- /Note:/ Consider using 'assignIPv6AddressOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subAssignIPv6AddressOnCreation :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
subAssignIPv6AddressOnCreation = Lens.lens (assignIPv6AddressOnCreation :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {assignIPv6AddressOnCreation = a} :: Subnet)
{-# DEPRECATED subAssignIPv6AddressOnCreation "Use generic-lens or generic-optics with 'assignIPv6AddressOnCreation' instead." #-}

-- | The Amazon Resource Name (ARN) of the subnet.
--
-- /Note:/ Consider using 'subnetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subSubnetARN :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
subSubnetARN = Lens.lens (subnetARN :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {subnetARN = a} :: Subnet)
{-# DEPRECATED subSubnetARN "Use generic-lens or generic-optics with 'subnetARN' instead." #-}

-- | The ID of the AWS account that owns the subnet.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subOwnerId :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
subOwnerId = Lens.lens (ownerId :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: Subnet)
{-# DEPRECATED subOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subCustomerOwnedIPv4Pool :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
subCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: Subnet)
{-# DEPRECATED subCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
--
-- /Note:/ Consider using 'mapCustomerOwnedIPOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subMapCustomerOwnedIPOnLaunch :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
subMapCustomerOwnedIPOnLaunch = Lens.lens (mapCustomerOwnedIPOnLaunch :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {mapCustomerOwnedIPOnLaunch = a} :: Subnet)
{-# DEPRECATED subMapCustomerOwnedIPOnLaunch "Use generic-lens or generic-optics with 'mapCustomerOwnedIPOnLaunch' instead." #-}

-- | Indicates whether instances launched in this subnet receive a public IPv4 address.
--
-- /Note:/ Consider using 'mapPublicIPOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subMapPublicIPOnLaunch :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
subMapPublicIPOnLaunch = Lens.lens (mapPublicIPOnLaunch :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {mapPublicIPOnLaunch = a} :: Subnet)
{-# DEPRECATED subMapPublicIPOnLaunch "Use generic-lens or generic-optics with 'mapPublicIPOnLaunch' instead." #-}

-- | Indicates whether this is the default subnet for the Availability Zone.
--
-- /Note:/ Consider using 'defaultForAz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subDefaultForAz :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
subDefaultForAz = Lens.lens (defaultForAz :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {defaultForAz = a} :: Subnet)
{-# DEPRECATED subDefaultForAz "Use generic-lens or generic-optics with 'defaultForAz' instead." #-}

-- | Any tags assigned to the subnet.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subTags :: Lens.Lens' Subnet (Lude.Maybe [Tag])
subTags = Lens.lens (tags :: Subnet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Subnet)
{-# DEPRECATED subTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Availability Zone of the subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subAvailabilityZone :: Lens.Lens' Subnet Lude.Text
subAvailabilityZone = Lens.lens (availabilityZone :: Subnet -> Lude.Text) (\s a -> s {availabilityZone = a} :: Subnet)
{-# DEPRECATED subAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
--
-- /Note:/ Consider using 'availableIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subAvailableIPAddressCount :: Lens.Lens' Subnet Lude.Int
subAvailableIPAddressCount = Lens.lens (availableIPAddressCount :: Subnet -> Lude.Int) (\s a -> s {availableIPAddressCount = a} :: Subnet)
{-# DEPRECATED subAvailableIPAddressCount "Use generic-lens or generic-optics with 'availableIPAddressCount' instead." #-}

-- | The IPv4 CIDR block assigned to the subnet.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subCidrBlock :: Lens.Lens' Subnet Lude.Text
subCidrBlock = Lens.lens (cidrBlock :: Subnet -> Lude.Text) (\s a -> s {cidrBlock = a} :: Subnet)
{-# DEPRECATED subCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The current state of the subnet.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subState :: Lens.Lens' Subnet SubnetState
subState = Lens.lens (state :: Subnet -> SubnetState) (\s a -> s {state = a} :: Subnet)
{-# DEPRECATED subState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subSubnetId :: Lens.Lens' Subnet Lude.Text
subSubnetId = Lens.lens (subnetId :: Subnet -> Lude.Text) (\s a -> s {subnetId = a} :: Subnet)
{-# DEPRECATED subSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the VPC the subnet is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
subVPCId :: Lens.Lens' Subnet Lude.Text
subVPCId = Lens.lens (vpcId :: Subnet -> Lude.Text) (\s a -> s {vpcId = a} :: Subnet)
{-# DEPRECATED subVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.FromXML Subnet where
  parseXML x =
    Subnet'
      Lude.<$> ( x Lude..@? "ipv6CidrBlockAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "availabilityZoneId")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "assignIpv6AddressOnCreation")
      Lude.<*> (x Lude..@? "subnetArn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "customerOwnedIpv4Pool")
      Lude.<*> (x Lude..@? "mapCustomerOwnedIpOnLaunch")
      Lude.<*> (x Lude..@? "mapPublicIpOnLaunch")
      Lude.<*> (x Lude..@? "defaultForAz")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "availabilityZone")
      Lude.<*> (x Lude..@ "availableIpAddressCount")
      Lude.<*> (x Lude..@ "cidrBlock")
      Lude.<*> (x Lude..@ "state")
      Lude.<*> (x Lude..@ "subnetId")
      Lude.<*> (x Lude..@ "vpcId")
