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
    sfAssignIpv6AddressOnCreation,
    sfAvailabilityZone,
    sfAvailabilityZoneId,
    sfAvailableIpAddressCount,
    sfCidrBlock,
    sfCustomerOwnedIpv4Pool,
    sfDefaultForAz,
    sfIpv6CidrBlockAssociationSet,
    sfMapCustomerOwnedIpOnLaunch,
    sfMapPublicIpOnLaunch,
    sfOutpostArn,
    sfOwnerId,
    sfState,
    sfSubnetArn,
    sfSubnetId,
    sfTags,
    sfVpcId,
  )
where

import qualified Network.AWS.EC2.Types.CustomerOwnedIpv4Pool as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation as Types
import qualified Network.AWS.EC2.Types.SubnetState as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a subnet.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { -- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
    assignIpv6AddressOnCreation :: Core.Maybe Core.Bool,
    -- | The Availability Zone of the subnet.
    availabilityZone :: Types.String,
    -- | The AZ ID of the subnet.
    availabilityZoneId :: Core.Maybe Types.String,
    -- | The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
    availableIpAddressCount :: Core.Int,
    -- | The IPv4 CIDR block assigned to the subnet.
    cidrBlock :: Types.String,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    customerOwnedIpv4Pool :: Core.Maybe Types.CustomerOwnedIpv4Pool,
    -- | Indicates whether this is the default subnet for the Availability Zone.
    defaultForAz :: Core.Maybe Core.Bool,
    -- | Information about the IPv6 CIDR blocks associated with the subnet.
    ipv6CidrBlockAssociationSet :: Core.Maybe [Types.SubnetIpv6CidrBlockAssociation],
    -- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
    mapCustomerOwnedIpOnLaunch :: Core.Maybe Core.Bool,
    -- | Indicates whether instances launched in this subnet receive a public IPv4 address.
    mapPublicIpOnLaunch :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Types.String,
    -- | The ID of the AWS account that owns the subnet.
    ownerId :: Core.Maybe Types.String,
    -- | The current state of the subnet.
    state :: Types.SubnetState,
    -- | The Amazon Resource Name (ARN) of the subnet.
    subnetArn :: Core.Maybe Types.String,
    -- | The ID of the subnet.
    subnetId :: Types.String,
    -- | Any tags assigned to the subnet.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the VPC the subnet is in.
    vpcId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subnet' value with any optional fields omitted.
mkSubnet ::
  -- | 'availabilityZone'
  Types.String ->
  -- | 'availableIpAddressCount'
  Core.Int ->
  -- | 'cidrBlock'
  Types.String ->
  -- | 'state'
  Types.SubnetState ->
  -- | 'subnetId'
  Types.String ->
  -- | 'vpcId'
  Types.String ->
  Subnet
mkSubnet
  availabilityZone
  availableIpAddressCount
  cidrBlock
  state
  subnetId
  vpcId =
    Subnet'
      { assignIpv6AddressOnCreation = Core.Nothing,
        availabilityZone,
        availabilityZoneId = Core.Nothing,
        availableIpAddressCount,
        cidrBlock,
        customerOwnedIpv4Pool = Core.Nothing,
        defaultForAz = Core.Nothing,
        ipv6CidrBlockAssociationSet = Core.Nothing,
        mapCustomerOwnedIpOnLaunch = Core.Nothing,
        mapPublicIpOnLaunch = Core.Nothing,
        outpostArn = Core.Nothing,
        ownerId = Core.Nothing,
        state,
        subnetArn = Core.Nothing,
        subnetId,
        tags = Core.Nothing,
        vpcId
      }

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives an IPv6 address.
--
-- /Note:/ Consider using 'assignIpv6AddressOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAssignIpv6AddressOnCreation :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
sfAssignIpv6AddressOnCreation = Lens.field @"assignIpv6AddressOnCreation"
{-# DEPRECATED sfAssignIpv6AddressOnCreation "Use generic-lens or generic-optics with 'assignIpv6AddressOnCreation' instead." #-}

-- | The Availability Zone of the subnet.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAvailabilityZone :: Lens.Lens' Subnet Types.String
sfAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED sfAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The AZ ID of the subnet.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAvailabilityZoneId :: Lens.Lens' Subnet (Core.Maybe Types.String)
sfAvailabilityZoneId = Lens.field @"availabilityZoneId"
{-# DEPRECATED sfAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The number of unused private IPv4 addresses in the subnet. The IPv4 addresses for any stopped instances are considered unavailable.
--
-- /Note:/ Consider using 'availableIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAvailableIpAddressCount :: Lens.Lens' Subnet Core.Int
sfAvailableIpAddressCount = Lens.field @"availableIpAddressCount"
{-# DEPRECATED sfAvailableIpAddressCount "Use generic-lens or generic-optics with 'availableIpAddressCount' instead." #-}

-- | The IPv4 CIDR block assigned to the subnet.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCidrBlock :: Lens.Lens' Subnet Types.String
sfCidrBlock = Lens.field @"cidrBlock"
{-# DEPRECATED sfCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCustomerOwnedIpv4Pool :: Lens.Lens' Subnet (Core.Maybe Types.CustomerOwnedIpv4Pool)
sfCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# DEPRECATED sfCustomerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead." #-}

-- | Indicates whether this is the default subnet for the Availability Zone.
--
-- /Note:/ Consider using 'defaultForAz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDefaultForAz :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
sfDefaultForAz = Lens.field @"defaultForAz"
{-# DEPRECATED sfDefaultForAz "Use generic-lens or generic-optics with 'defaultForAz' instead." #-}

-- | Information about the IPv6 CIDR blocks associated with the subnet.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfIpv6CidrBlockAssociationSet :: Lens.Lens' Subnet (Core.Maybe [Types.SubnetIpv6CidrBlockAssociation])
sfIpv6CidrBlockAssociationSet = Lens.field @"ipv6CidrBlockAssociationSet"
{-# DEPRECATED sfIpv6CidrBlockAssociationSet "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociationSet' instead." #-}

-- | Indicates whether a network interface created in this subnet (including a network interface created by 'RunInstances' ) receives a customer-owned IPv4 address.
--
-- /Note:/ Consider using 'mapCustomerOwnedIpOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfMapCustomerOwnedIpOnLaunch :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
sfMapCustomerOwnedIpOnLaunch = Lens.field @"mapCustomerOwnedIpOnLaunch"
{-# DEPRECATED sfMapCustomerOwnedIpOnLaunch "Use generic-lens or generic-optics with 'mapCustomerOwnedIpOnLaunch' instead." #-}

-- | Indicates whether instances launched in this subnet receive a public IPv4 address.
--
-- /Note:/ Consider using 'mapPublicIpOnLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfMapPublicIpOnLaunch :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
sfMapPublicIpOnLaunch = Lens.field @"mapPublicIpOnLaunch"
{-# DEPRECATED sfMapPublicIpOnLaunch "Use generic-lens or generic-optics with 'mapPublicIpOnLaunch' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOutpostArn :: Lens.Lens' Subnet (Core.Maybe Types.String)
sfOutpostArn = Lens.field @"outpostArn"
{-# DEPRECATED sfOutpostArn "Use generic-lens or generic-optics with 'outpostArn' instead." #-}

-- | The ID of the AWS account that owns the subnet.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOwnerId :: Lens.Lens' Subnet (Core.Maybe Types.String)
sfOwnerId = Lens.field @"ownerId"
{-# DEPRECATED sfOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The current state of the subnet.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfState :: Lens.Lens' Subnet Types.SubnetState
sfState = Lens.field @"state"
{-# DEPRECATED sfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the subnet.
--
-- /Note:/ Consider using 'subnetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSubnetArn :: Lens.Lens' Subnet (Core.Maybe Types.String)
sfSubnetArn = Lens.field @"subnetArn"
{-# DEPRECATED sfSubnetArn "Use generic-lens or generic-optics with 'subnetArn' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSubnetId :: Lens.Lens' Subnet Types.String
sfSubnetId = Lens.field @"subnetId"
{-# DEPRECATED sfSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Any tags assigned to the subnet.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTags :: Lens.Lens' Subnet (Core.Maybe [Types.Tag])
sfTags = Lens.field @"tags"
{-# DEPRECATED sfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the VPC the subnet is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfVpcId :: Lens.Lens' Subnet Types.String
sfVpcId = Lens.field @"vpcId"
{-# DEPRECATED sfVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Core.<$> (x Core..@? "assignIpv6AddressOnCreation")
      Core.<*> (x Core..@ "availabilityZone")
      Core.<*> (x Core..@? "availabilityZoneId")
      Core.<*> (x Core..@ "availableIpAddressCount")
      Core.<*> (x Core..@ "cidrBlock")
      Core.<*> (x Core..@? "customerOwnedIpv4Pool")
      Core.<*> (x Core..@? "defaultForAz")
      Core.<*> ( x Core..@? "ipv6CidrBlockAssociationSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "mapCustomerOwnedIpOnLaunch")
      Core.<*> (x Core..@? "mapPublicIpOnLaunch")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@? "subnetArn")
      Core.<*> (x Core..@ "subnetId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@ "vpcId")
