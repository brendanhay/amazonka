{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
  ( VpcPeeringConnectionVpcInfo (..)
  -- * Smart constructor
  , mkVpcPeeringConnectionVpcInfo
  -- * Lenses
  , vpcviCidrBlock
  , vpcviCidrBlockSet
  , vpcviIpv6CidrBlockSet
  , vpcviOwnerId
  , vpcviPeeringOptions
  , vpcviRegion
  , vpcviVpcId
  ) where

import qualified Network.AWS.EC2.Types.CidrBlock as Types
import qualified Network.AWS.EC2.Types.Ipv6CidrBlock as Types
import qualified Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC in a VPC peering connection.
--
-- /See:/ 'mkVpcPeeringConnectionVpcInfo' smart constructor.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo'
  { cidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv4 CIDR block for the VPC.
  , cidrBlockSet :: Core.Maybe [Types.CidrBlock]
    -- ^ Information about the IPv4 CIDR blocks for the VPC.
  , ipv6CidrBlockSet :: Core.Maybe [Types.Ipv6CidrBlock]
    -- ^ The IPv6 CIDR block for the VPC.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the VPC owner.
  , peeringOptions :: Core.Maybe Types.VpcPeeringConnectionOptionsDescription
    -- ^ Information about the VPC peering connection options for the accepter or requester VPC.
  , region :: Core.Maybe Core.Text
    -- ^ The Region in which the VPC is located.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcPeeringConnectionVpcInfo' value with any optional fields omitted.
mkVpcPeeringConnectionVpcInfo
    :: VpcPeeringConnectionVpcInfo
mkVpcPeeringConnectionVpcInfo
  = VpcPeeringConnectionVpcInfo'{cidrBlock = Core.Nothing,
                                 cidrBlockSet = Core.Nothing, ipv6CidrBlockSet = Core.Nothing,
                                 ownerId = Core.Nothing, peeringOptions = Core.Nothing,
                                 region = Core.Nothing, vpcId = Core.Nothing}

-- | The IPv4 CIDR block for the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviCidrBlock :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcviCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE vpcviCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Information about the IPv4 CIDR blocks for the VPC.
--
-- /Note:/ Consider using 'cidrBlockSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviCidrBlockSet :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe [Types.CidrBlock])
vpcviCidrBlockSet = Lens.field @"cidrBlockSet"
{-# INLINEABLE vpcviCidrBlockSet #-}
{-# DEPRECATED cidrBlockSet "Use generic-lens or generic-optics with 'cidrBlockSet' instead"  #-}

-- | The IPv6 CIDR block for the VPC.
--
-- /Note:/ Consider using 'ipv6CidrBlockSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviIpv6CidrBlockSet :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe [Types.Ipv6CidrBlock])
vpcviIpv6CidrBlockSet = Lens.field @"ipv6CidrBlockSet"
{-# INLINEABLE vpcviIpv6CidrBlockSet #-}
{-# DEPRECATED ipv6CidrBlockSet "Use generic-lens or generic-optics with 'ipv6CidrBlockSet' instead"  #-}

-- | The AWS account ID of the VPC owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviOwnerId :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcviOwnerId = Lens.field @"ownerId"
{-# INLINEABLE vpcviOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Information about the VPC peering connection options for the accepter or requester VPC.
--
-- /Note:/ Consider using 'peeringOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviPeeringOptions :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Types.VpcPeeringConnectionOptionsDescription)
vpcviPeeringOptions = Lens.field @"peeringOptions"
{-# INLINEABLE vpcviPeeringOptions #-}
{-# DEPRECATED peeringOptions "Use generic-lens or generic-optics with 'peeringOptions' instead"  #-}

-- | The Region in which the VPC is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviRegion :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcviRegion = Lens.field @"region"
{-# INLINEABLE vpcviRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcviVpcId :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcviVpcId = Lens.field @"vpcId"
{-# INLINEABLE vpcviVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML VpcPeeringConnectionVpcInfo where
        parseXML x
          = VpcPeeringConnectionVpcInfo' Core.<$>
              (x Core..@? "cidrBlock") Core.<*>
                x Core..@? "cidrBlockSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "ipv6CidrBlockSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "peeringOptions"
                Core.<*> x Core..@? "region"
                Core.<*> x Core..@? "vpcId"
