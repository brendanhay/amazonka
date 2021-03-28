{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateVpcCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a VPC. To disassociate the CIDR block, you must specify its association ID. You can get the association ID by using 'DescribeVpcs' . You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it. 
--
-- You cannot disassociate the CIDR block with which you originally created the VPC (the primary CIDR block).
module Network.AWS.EC2.DisassociateVpcCidrBlock
    (
    -- * Creating a request
      DisassociateVpcCidrBlock (..)
    , mkDisassociateVpcCidrBlock
    -- ** Request lenses
    , dvcbAssociationId

    -- * Destructuring the response
    , DisassociateVpcCidrBlockResponse (..)
    , mkDisassociateVpcCidrBlockResponse
    -- ** Response lenses
    , dvcbrrsCidrBlockAssociation
    , dvcbrrsIpv6CidrBlockAssociation
    , dvcbrrsVpcId
    , dvcbrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateVpcCidrBlock' smart constructor.
newtype DisassociateVpcCidrBlock = DisassociateVpcCidrBlock'
  { associationId :: Types.AssociationId
    -- ^ The association ID for the CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateVpcCidrBlock' value with any optional fields omitted.
mkDisassociateVpcCidrBlock
    :: Types.AssociationId -- ^ 'associationId'
    -> DisassociateVpcCidrBlock
mkDisassociateVpcCidrBlock associationId
  = DisassociateVpcCidrBlock'{associationId}

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbAssociationId :: Lens.Lens' DisassociateVpcCidrBlock Types.AssociationId
dvcbAssociationId = Lens.field @"associationId"
{-# INLINEABLE dvcbAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

instance Core.ToQuery DisassociateVpcCidrBlock where
        toQuery DisassociateVpcCidrBlock{..}
          = Core.toQueryPair "Action"
              ("DisassociateVpcCidrBlock" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AssociationId" associationId

instance Core.ToHeaders DisassociateVpcCidrBlock where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateVpcCidrBlock where
        type Rs DisassociateVpcCidrBlock = DisassociateVpcCidrBlockResponse
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
                 DisassociateVpcCidrBlockResponse' Core.<$>
                   (x Core..@? "cidrBlockAssociation") Core.<*>
                     x Core..@? "ipv6CidrBlockAssociation"
                     Core.<*> x Core..@? "vpcId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateVpcCidrBlockResponse' smart constructor.
data DisassociateVpcCidrBlockResponse = DisassociateVpcCidrBlockResponse'
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

-- | Creates a 'DisassociateVpcCidrBlockResponse' value with any optional fields omitted.
mkDisassociateVpcCidrBlockResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateVpcCidrBlockResponse
mkDisassociateVpcCidrBlockResponse responseStatus
  = DisassociateVpcCidrBlockResponse'{cidrBlockAssociation =
                                        Core.Nothing,
                                      ipv6CidrBlockAssociation = Core.Nothing, vpcId = Core.Nothing,
                                      responseStatus}

-- | Information about the IPv4 CIDR block association.
--
-- /Note:/ Consider using 'cidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrrsCidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Core.Maybe Types.VpcCidrBlockAssociation)
dvcbrrsCidrBlockAssociation = Lens.field @"cidrBlockAssociation"
{-# INLINEABLE dvcbrrsCidrBlockAssociation #-}
{-# DEPRECATED cidrBlockAssociation "Use generic-lens or generic-optics with 'cidrBlockAssociation' instead"  #-}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrrsIpv6CidrBlockAssociation :: Lens.Lens' DisassociateVpcCidrBlockResponse (Core.Maybe Types.VpcIpv6CidrBlockAssociation)
dvcbrrsIpv6CidrBlockAssociation = Lens.field @"ipv6CidrBlockAssociation"
{-# INLINEABLE dvcbrrsIpv6CidrBlockAssociation #-}
{-# DEPRECATED ipv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrrsVpcId :: Lens.Lens' DisassociateVpcCidrBlockResponse (Core.Maybe Core.Text)
dvcbrrsVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvcbrrsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrrsResponseStatus :: Lens.Lens' DisassociateVpcCidrBlockResponse Core.Int
dvcbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvcbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
