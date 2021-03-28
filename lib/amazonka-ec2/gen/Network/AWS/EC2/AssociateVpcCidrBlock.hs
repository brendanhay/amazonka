{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateVpcCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your VPC. You can associate a secondary IPv4 CIDR block, an Amazon-provided IPv6 CIDR block, or an IPv6 CIDR block from an IPv6 address pool that you provisioned through bring your own IP addresses (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html BYOIP> ). The IPv6 CIDR block size is fixed at /56.
--
-- You must specify one of the following in the request: an IPv4 CIDR block, an IPv6 pool, or an Amazon-provided IPv6 CIDR block.
-- For more information about associating CIDR blocks with your VPC and applicable restrictions, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#VPC_Sizing VPC and Subnet Sizing> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.AssociateVpcCidrBlock
    (
    -- * Creating a request
      AssociateVpcCidrBlock (..)
    , mkAssociateVpcCidrBlock
    -- ** Request lenses
    , avcbVpcId
    , avcbAmazonProvidedIpv6CidrBlock
    , avcbCidrBlock
    , avcbIpv6CidrBlock
    , avcbIpv6CidrBlockNetworkBorderGroup
    , avcbIpv6Pool

    -- * Destructuring the response
    , AssociateVpcCidrBlockResponse (..)
    , mkAssociateVpcCidrBlockResponse
    -- ** Response lenses
    , avcbrrsCidrBlockAssociation
    , avcbrrsIpv6CidrBlockAssociation
    , avcbrrsVpcId
    , avcbrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateVpcCidrBlock' smart constructor.
data AssociateVpcCidrBlock = AssociateVpcCidrBlock'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , amazonProvidedIpv6CidrBlock :: Core.Maybe Core.Bool
    -- ^ Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
  , cidrBlock :: Core.Maybe Core.Text
    -- ^ An IPv4 CIDR block to associate with the VPC.
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ An IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
  , ipv6CidrBlockNetworkBorderGroup :: Core.Maybe Core.Text
    -- ^ The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
-- You can have one IPv6 CIDR block association per network border group.
  , ipv6Pool :: Core.Maybe Types.Ipv6PoolEc2Id
    -- ^ The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateVpcCidrBlock' value with any optional fields omitted.
mkAssociateVpcCidrBlock
    :: Types.VpcId -- ^ 'vpcId'
    -> AssociateVpcCidrBlock
mkAssociateVpcCidrBlock vpcId
  = AssociateVpcCidrBlock'{vpcId,
                           amazonProvidedIpv6CidrBlock = Core.Nothing,
                           cidrBlock = Core.Nothing, ipv6CidrBlock = Core.Nothing,
                           ipv6CidrBlockNetworkBorderGroup = Core.Nothing,
                           ipv6Pool = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbVpcId :: Lens.Lens' AssociateVpcCidrBlock Types.VpcId
avcbVpcId = Lens.field @"vpcId"
{-# INLINEABLE avcbVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
--
-- /Note:/ Consider using 'amazonProvidedIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbAmazonProvidedIpv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Core.Maybe Core.Bool)
avcbAmazonProvidedIpv6CidrBlock = Lens.field @"amazonProvidedIpv6CidrBlock"
{-# INLINEABLE avcbAmazonProvidedIpv6CidrBlock #-}
{-# DEPRECATED amazonProvidedIpv6CidrBlock "Use generic-lens or generic-optics with 'amazonProvidedIpv6CidrBlock' instead"  #-}

-- | An IPv4 CIDR block to associate with the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbCidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Core.Maybe Core.Text)
avcbCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE avcbCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | An IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbIpv6CidrBlock :: Lens.Lens' AssociateVpcCidrBlock (Core.Maybe Core.Text)
avcbIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE avcbIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
-- You can have one IPv6 CIDR block association per network border group.
--
-- /Note:/ Consider using 'ipv6CidrBlockNetworkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbIpv6CidrBlockNetworkBorderGroup :: Lens.Lens' AssociateVpcCidrBlock (Core.Maybe Core.Text)
avcbIpv6CidrBlockNetworkBorderGroup = Lens.field @"ipv6CidrBlockNetworkBorderGroup"
{-# INLINEABLE avcbIpv6CidrBlockNetworkBorderGroup #-}
{-# DEPRECATED ipv6CidrBlockNetworkBorderGroup "Use generic-lens or generic-optics with 'ipv6CidrBlockNetworkBorderGroup' instead"  #-}

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbIpv6Pool :: Lens.Lens' AssociateVpcCidrBlock (Core.Maybe Types.Ipv6PoolEc2Id)
avcbIpv6Pool = Lens.field @"ipv6Pool"
{-# INLINEABLE avcbIpv6Pool #-}
{-# DEPRECATED ipv6Pool "Use generic-lens or generic-optics with 'ipv6Pool' instead"  #-}

instance Core.ToQuery AssociateVpcCidrBlock where
        toQuery AssociateVpcCidrBlock{..}
          = Core.toQueryPair "Action" ("AssociateVpcCidrBlock" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AmazonProvidedIpv6CidrBlock")
                amazonProvidedIpv6CidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CidrBlock") cidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6CidrBlock")
                ipv6CidrBlock
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "Ipv6CidrBlockNetworkBorderGroup")
                ipv6CidrBlockNetworkBorderGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6Pool") ipv6Pool

instance Core.ToHeaders AssociateVpcCidrBlock where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssociateVpcCidrBlock where
        type Rs AssociateVpcCidrBlock = AssociateVpcCidrBlockResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 AssociateVpcCidrBlockResponse' Core.<$>
                   (x Core..@? "cidrBlockAssociation") Core.<*>
                     x Core..@? "ipv6CidrBlockAssociation"
                     Core.<*> x Core..@? "vpcId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateVpcCidrBlockResponse' smart constructor.
data AssociateVpcCidrBlockResponse = AssociateVpcCidrBlockResponse'
  { cidrBlockAssociation :: Core.Maybe Types.VpcCidrBlockAssociation
    -- ^ Information about the IPv4 CIDR block association.
  , ipv6CidrBlockAssociation :: Core.Maybe Types.VpcIpv6CidrBlockAssociation
    -- ^ Information about the IPv6 CIDR block association.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateVpcCidrBlockResponse' value with any optional fields omitted.
mkAssociateVpcCidrBlockResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateVpcCidrBlockResponse
mkAssociateVpcCidrBlockResponse responseStatus
  = AssociateVpcCidrBlockResponse'{cidrBlockAssociation =
                                     Core.Nothing,
                                   ipv6CidrBlockAssociation = Core.Nothing, vpcId = Core.Nothing,
                                   responseStatus}

-- | Information about the IPv4 CIDR block association.
--
-- /Note:/ Consider using 'cidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrrsCidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Core.Maybe Types.VpcCidrBlockAssociation)
avcbrrsCidrBlockAssociation = Lens.field @"cidrBlockAssociation"
{-# INLINEABLE avcbrrsCidrBlockAssociation #-}
{-# DEPRECATED cidrBlockAssociation "Use generic-lens or generic-optics with 'cidrBlockAssociation' instead"  #-}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrrsIpv6CidrBlockAssociation :: Lens.Lens' AssociateVpcCidrBlockResponse (Core.Maybe Types.VpcIpv6CidrBlockAssociation)
avcbrrsIpv6CidrBlockAssociation = Lens.field @"ipv6CidrBlockAssociation"
{-# INLINEABLE avcbrrsIpv6CidrBlockAssociation #-}
{-# DEPRECATED ipv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrrsVpcId :: Lens.Lens' AssociateVpcCidrBlockResponse (Core.Maybe Core.Text)
avcbrrsVpcId = Lens.field @"vpcId"
{-# INLINEABLE avcbrrsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrrsResponseStatus :: Lens.Lens' AssociateVpcCidrBlockResponse Core.Int
avcbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE avcbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
