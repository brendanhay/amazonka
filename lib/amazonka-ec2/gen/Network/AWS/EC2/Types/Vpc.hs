{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Vpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Vpc
  ( Vpc (..)
  -- * Smart constructor
  , mkVpc
  -- * Lenses
  , vfCidrBlock
  , vfCidrBlockAssociationSet
  , vfDhcpOptionsId
  , vfInstanceTenancy
  , vfIpv6CidrBlockAssociationSet
  , vfIsDefault
  , vfOwnerId
  , vfState
  , vfTags
  , vfVpcId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.EC2.Types.VpcCidrBlockAssociation as Types
import qualified Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation as Types
import qualified Network.AWS.EC2.Types.VpcState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC.
--
-- /See:/ 'mkVpc' smart constructor.
data Vpc = Vpc'
  { cidrBlock :: Core.Text
    -- ^ The primary IPv4 CIDR block for the VPC.
  , cidrBlockAssociationSet :: Core.Maybe [Types.VpcCidrBlockAssociation]
    -- ^ Information about the IPv4 CIDR blocks associated with the VPC.
  , dhcpOptionsId :: Core.Text
    -- ^ The ID of the set of DHCP options you've associated with the VPC.
  , instanceTenancy :: Types.Tenancy
    -- ^ The allowed tenancy of instances launched into the VPC.
  , ipv6CidrBlockAssociationSet :: Core.Maybe [Types.VpcIpv6CidrBlockAssociation]
    -- ^ Information about the IPv6 CIDR blocks associated with the VPC.
  , isDefault :: Core.Maybe Core.Bool
    -- ^ Indicates whether the VPC is the default VPC.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the VPC.
  , state :: Types.VpcState
    -- ^ The current state of the VPC.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the VPC.
  , vpcId :: Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Vpc' value with any optional fields omitted.
mkVpc
    :: Core.Text -- ^ 'cidrBlock'
    -> Core.Text -- ^ 'dhcpOptionsId'
    -> Types.Tenancy -- ^ 'instanceTenancy'
    -> Types.VpcState -- ^ 'state'
    -> Core.Text -- ^ 'vpcId'
    -> Vpc
mkVpc cidrBlock dhcpOptionsId instanceTenancy state vpcId
  = Vpc'{cidrBlock, cidrBlockAssociationSet = Core.Nothing,
         dhcpOptionsId, instanceTenancy,
         ipv6CidrBlockAssociationSet = Core.Nothing,
         isDefault = Core.Nothing, ownerId = Core.Nothing, state,
         tags = Core.Nothing, vpcId}

-- | The primary IPv4 CIDR block for the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfCidrBlock :: Lens.Lens' Vpc Core.Text
vfCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE vfCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Information about the IPv4 CIDR blocks associated with the VPC.
--
-- /Note:/ Consider using 'cidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfCidrBlockAssociationSet :: Lens.Lens' Vpc (Core.Maybe [Types.VpcCidrBlockAssociation])
vfCidrBlockAssociationSet = Lens.field @"cidrBlockAssociationSet"
{-# INLINEABLE vfCidrBlockAssociationSet #-}
{-# DEPRECATED cidrBlockAssociationSet "Use generic-lens or generic-optics with 'cidrBlockAssociationSet' instead"  #-}

-- | The ID of the set of DHCP options you've associated with the VPC.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfDhcpOptionsId :: Lens.Lens' Vpc Core.Text
vfDhcpOptionsId = Lens.field @"dhcpOptionsId"
{-# INLINEABLE vfDhcpOptionsId #-}
{-# DEPRECATED dhcpOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead"  #-}

-- | The allowed tenancy of instances launched into the VPC.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfInstanceTenancy :: Lens.Lens' Vpc Types.Tenancy
vfInstanceTenancy = Lens.field @"instanceTenancy"
{-# INLINEABLE vfInstanceTenancy #-}
{-# DEPRECATED instanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead"  #-}

-- | Information about the IPv6 CIDR blocks associated with the VPC.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfIpv6CidrBlockAssociationSet :: Lens.Lens' Vpc (Core.Maybe [Types.VpcIpv6CidrBlockAssociation])
vfIpv6CidrBlockAssociationSet = Lens.field @"ipv6CidrBlockAssociationSet"
{-# INLINEABLE vfIpv6CidrBlockAssociationSet #-}
{-# DEPRECATED ipv6CidrBlockAssociationSet "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociationSet' instead"  #-}

-- | Indicates whether the VPC is the default VPC.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfIsDefault :: Lens.Lens' Vpc (Core.Maybe Core.Bool)
vfIsDefault = Lens.field @"isDefault"
{-# INLINEABLE vfIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

-- | The ID of the AWS account that owns the VPC.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfOwnerId :: Lens.Lens' Vpc (Core.Maybe Core.Text)
vfOwnerId = Lens.field @"ownerId"
{-# INLINEABLE vfOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The current state of the VPC.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfState :: Lens.Lens' Vpc Types.VpcState
vfState = Lens.field @"state"
{-# INLINEABLE vfState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the VPC.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfTags :: Lens.Lens' Vpc (Core.Maybe [Types.Tag])
vfTags = Lens.field @"tags"
{-# INLINEABLE vfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vfVpcId :: Lens.Lens' Vpc Core.Text
vfVpcId = Lens.field @"vpcId"
{-# INLINEABLE vfVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML Vpc where
        parseXML x
          = Vpc' Core.<$>
              (x Core..@ "cidrBlock") Core.<*>
                x Core..@? "cidrBlockAssociationSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@ "dhcpOptionsId"
                Core.<*> x Core..@ "instanceTenancy"
                Core.<*>
                x Core..@? "ipv6CidrBlockAssociationSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "isDefault"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@ "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "vpcId"
