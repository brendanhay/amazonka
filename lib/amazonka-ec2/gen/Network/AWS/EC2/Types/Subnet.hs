{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    sState,
    sIPv6CidrBlockAssociationSet,
    sAvailabilityZoneId,
    sAvailableIPAddressCount,
    sVPCId,
    sOutpostARN,
    sAssignIPv6AddressOnCreation,
    sSubnetId,
    sSubnetARN,
    sOwnerId,
    sCustomerOwnedIPv4Pool,
    sAvailabilityZone,
    sMapCustomerOwnedIPOnLaunch,
    sCidrBlock,
    sMapPublicIPOnLaunch,
    sDefaultForAz,
    sTags,
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
  { -- | The current state of the subnet.
    state :: SubnetState,
    -- | Information about the IPv6 CIDR blocks associated with the subnet.
    ipv6CidrBlockAssociationSet :: Lude.Maybe [SubnetIPv6CidrBlockAssociation],
    -- | The AZ ID of the subnet.
    availabilityZoneId :: Lude.Maybe Lude.Text,
    -- | The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
    availableIPAddressCount :: Lude.Int,
    -- | The ID of the VPC the subnet is in.
    vpcId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
    assignIPv6AddressOnCreation :: Lude.Maybe Lude.Bool,
    -- | The ID of the subnet.
    subnetId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the subnet.
    subnetARN :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS account that owns the subnet.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    -- | The Availability Zone of the subnet.
    availabilityZone :: Lude.Text,
    -- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
    mapCustomerOwnedIPOnLaunch :: Lude.Maybe Lude.Bool,
    -- | The IPv4 CIDR block assigned to the subnet.
    cidrBlock :: Lude.Text,
    -- | Indicates whether instances launched in this subnet receive a public IPv4 address.
    mapPublicIPOnLaunch :: Lude.Maybe Lude.Bool,
    -- | Indicates whether this is the default subnet for the Availability Zone.
    defaultForAz :: Lude.Maybe Lude.Bool,
    -- | Any tags assigned to the subnet.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the subnet.
-- * 'ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the subnet.
-- * 'availabilityZoneId' - The AZ ID of the subnet.
-- * 'availableIPAddressCount' - The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
-- * 'vpcId' - The ID of the VPC the subnet is in.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'assignIPv6AddressOnCreation' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
-- * 'subnetId' - The ID of the subnet.
-- * 'subnetARN' - The Amazon Resource Name (ARN) of the subnet.
-- * 'ownerId' - The ID of the AWS account that owns the subnet.
-- * 'customerOwnedIPv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
-- * 'availabilityZone' - The Availability Zone of the subnet.
-- * 'mapCustomerOwnedIPOnLaunch' - Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
-- * 'cidrBlock' - The IPv4 CIDR block assigned to the subnet.
-- * 'mapPublicIPOnLaunch' - Indicates whether instances launched in this subnet receive a public IPv4 address.
-- * 'defaultForAz' - Indicates whether this is the default subnet for the Availability Zone.
-- * 'tags' - Any tags assigned to the subnet.
mkSubnet ::
  -- | 'state'
  SubnetState ->
  -- | 'availableIPAddressCount'
  Lude.Int ->
  -- | 'vpcId'
  Lude.Text ->
  -- | 'subnetId'
  Lude.Text ->
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'cidrBlock'
  Lude.Text ->
  Subnet
mkSubnet
  pState_
  pAvailableIPAddressCount_
  pVPCId_
  pSubnetId_
  pAvailabilityZone_
  pCidrBlock_ =
    Subnet'
      { state = pState_,
        ipv6CidrBlockAssociationSet = Lude.Nothing,
        availabilityZoneId = Lude.Nothing,
        availableIPAddressCount = pAvailableIPAddressCount_,
        vpcId = pVPCId_,
        outpostARN = Lude.Nothing,
        assignIPv6AddressOnCreation = Lude.Nothing,
        subnetId = pSubnetId_,
        subnetARN = Lude.Nothing,
        ownerId = Lude.Nothing,
        customerOwnedIPv4Pool = Lude.Nothing,
        availabilityZone = pAvailabilityZone_,
        mapCustomerOwnedIPOnLaunch = Lude.Nothing,
        cidrBlock = pCidrBlock_,
        mapPublicIPOnLaunch = Lude.Nothing,
        defaultForAz = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The current state of the subnet.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sState :: Lens.Lens' Subnet SubnetState
sState = Lens.lens (state :: Subnet -> SubnetState) (\s a -> s {state = a} :: Subnet)
{-# DEPRECATED sState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the IPv6 CIDR blocks associated with the subnet.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sIPv6CidrBlockAssociationSet :: Lens.Lens' Subnet (Lude.Maybe [SubnetIPv6CidrBlockAssociation])
sIPv6CidrBlockAssociationSet = Lens.lens (ipv6CidrBlockAssociationSet :: Subnet -> Lude.Maybe [SubnetIPv6CidrBlockAssociation]) (\s a -> s {ipv6CidrBlockAssociationSet = a} :: Subnet)
{-# DEPRECATED sIPv6CidrBlockAssociationSet "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociationSet' instead." #-}

-- | The AZ ID of the subnet.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailabilityZoneId :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sAvailabilityZoneId = Lens.lens (availabilityZoneId :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: Subnet)
{-# DEPRECATED sAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
--
-- /Note:/ Consider using 'availableIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailableIPAddressCount :: Lens.Lens' Subnet Lude.Int
sAvailableIPAddressCount = Lens.lens (availableIPAddressCount :: Subnet -> Lude.Int) (\s a -> s {availableIPAddressCount = a} :: Subnet)
{-# DEPRECATED sAvailableIPAddressCount "Use generic-lens or generic-optics with 'availableIPAddressCount' instead." #-}

-- | The ID of the VPC the subnet is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVPCId :: Lens.Lens' Subnet Lude.Text
sVPCId = Lens.lens (vpcId :: Subnet -> Lude.Text) (\s a -> s {vpcId = a} :: Subnet)
{-# DEPRECATED sVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutpostARN :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sOutpostARN = Lens.lens (outpostARN :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: Subnet)
{-# DEPRECATED sOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
--
-- /Note:/ Consider using 'assignIPv6AddressOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAssignIPv6AddressOnCreation :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
sAssignIPv6AddressOnCreation = Lens.lens (assignIPv6AddressOnCreation :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {assignIPv6AddressOnCreation = a} :: Subnet)
{-# DEPRECATED sAssignIPv6AddressOnCreation "Use generic-lens or generic-optics with 'assignIPv6AddressOnCreation' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetId :: Lens.Lens' Subnet Lude.Text
sSubnetId = Lens.lens (subnetId :: Subnet -> Lude.Text) (\s a -> s {subnetId = a} :: Subnet)
{-# DEPRECATED sSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The Amazon Resource Name (ARN) of the subnet.
--
-- /Note:/ Consider using 'subnetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetARN :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sSubnetARN = Lens.lens (subnetARN :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {subnetARN = a} :: Subnet)
{-# DEPRECATED sSubnetARN "Use generic-lens or generic-optics with 'subnetARN' instead." #-}

-- | The ID of the AWS account that owns the subnet.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerId :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sOwnerId = Lens.lens (ownerId :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: Subnet)
{-# DEPRECATED sOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCustomerOwnedIPv4Pool :: Lens.Lens' Subnet (Lude.Maybe Lude.Text)
sCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: Subnet -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: Subnet)
{-# DEPRECATED sCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | The Availability Zone of the subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailabilityZone :: Lens.Lens' Subnet Lude.Text
sAvailabilityZone = Lens.lens (availabilityZone :: Subnet -> Lude.Text) (\s a -> s {availabilityZone = a} :: Subnet)
{-# DEPRECATED sAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
--
-- /Note:/ Consider using 'mapCustomerOwnedIPOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMapCustomerOwnedIPOnLaunch :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
sMapCustomerOwnedIPOnLaunch = Lens.lens (mapCustomerOwnedIPOnLaunch :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {mapCustomerOwnedIPOnLaunch = a} :: Subnet)
{-# DEPRECATED sMapCustomerOwnedIPOnLaunch "Use generic-lens or generic-optics with 'mapCustomerOwnedIPOnLaunch' instead." #-}

-- | The IPv4 CIDR block assigned to the subnet.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCidrBlock :: Lens.Lens' Subnet Lude.Text
sCidrBlock = Lens.lens (cidrBlock :: Subnet -> Lude.Text) (\s a -> s {cidrBlock = a} :: Subnet)
{-# DEPRECATED sCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | Indicates whether instances launched in this subnet receive a public IPv4 address.
--
-- /Note:/ Consider using 'mapPublicIPOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMapPublicIPOnLaunch :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
sMapPublicIPOnLaunch = Lens.lens (mapPublicIPOnLaunch :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {mapPublicIPOnLaunch = a} :: Subnet)
{-# DEPRECATED sMapPublicIPOnLaunch "Use generic-lens or generic-optics with 'mapPublicIPOnLaunch' instead." #-}

-- | Indicates whether this is the default subnet for the Availability Zone.
--
-- /Note:/ Consider using 'defaultForAz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDefaultForAz :: Lens.Lens' Subnet (Lude.Maybe Lude.Bool)
sDefaultForAz = Lens.lens (defaultForAz :: Subnet -> Lude.Maybe Lude.Bool) (\s a -> s {defaultForAz = a} :: Subnet)
{-# DEPRECATED sDefaultForAz "Use generic-lens or generic-optics with 'defaultForAz' instead." #-}

-- | Any tags assigned to the subnet.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' Subnet (Lude.Maybe [Tag])
sTags = Lens.lens (tags :: Subnet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Subnet)
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML Subnet where
  parseXML x =
    Subnet'
      Lude.<$> (x Lude..@ "state")
      Lude.<*> ( x Lude..@? "ipv6CidrBlockAssociationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "availabilityZoneId")
      Lude.<*> (x Lude..@ "availableIpAddressCount")
      Lude.<*> (x Lude..@ "vpcId")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "assignIpv6AddressOnCreation")
      Lude.<*> (x Lude..@ "subnetId")
      Lude.<*> (x Lude..@? "subnetArn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "customerOwnedIpv4Pool")
      Lude.<*> (x Lude..@ "availabilityZone")
      Lude.<*> (x Lude..@? "mapCustomerOwnedIpOnLaunch")
      Lude.<*> (x Lude..@ "cidrBlock")
      Lude.<*> (x Lude..@? "mapPublicIpOnLaunch")
      Lude.<*> (x Lude..@? "defaultForAz")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
