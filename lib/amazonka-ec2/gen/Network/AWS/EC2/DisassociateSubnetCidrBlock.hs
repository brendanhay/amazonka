{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateSubnetCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a subnet. Currently, you can disassociate an IPv6 CIDR block only. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it. 
module Network.AWS.EC2.DisassociateSubnetCidrBlock
    (
    -- * Creating a request
      DisassociateSubnetCidrBlock (..)
    , mkDisassociateSubnetCidrBlock
    -- ** Request lenses
    , dscbAssociationId

    -- * Destructuring the response
    , DisassociateSubnetCidrBlockResponse (..)
    , mkDisassociateSubnetCidrBlockResponse
    -- ** Response lenses
    , dscbrrsIpv6CidrBlockAssociation
    , dscbrrsSubnetId
    , dscbrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSubnetCidrBlock' smart constructor.
newtype DisassociateSubnetCidrBlock = DisassociateSubnetCidrBlock'
  { associationId :: Types.SubnetCidrAssociationId
    -- ^ The association ID for the CIDR block.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSubnetCidrBlock' value with any optional fields omitted.
mkDisassociateSubnetCidrBlock
    :: Types.SubnetCidrAssociationId -- ^ 'associationId'
    -> DisassociateSubnetCidrBlock
mkDisassociateSubnetCidrBlock associationId
  = DisassociateSubnetCidrBlock'{associationId}

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbAssociationId :: Lens.Lens' DisassociateSubnetCidrBlock Types.SubnetCidrAssociationId
dscbAssociationId = Lens.field @"associationId"
{-# INLINEABLE dscbAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

instance Core.ToQuery DisassociateSubnetCidrBlock where
        toQuery DisassociateSubnetCidrBlock{..}
          = Core.toQueryPair "Action"
              ("DisassociateSubnetCidrBlock" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AssociationId" associationId

instance Core.ToHeaders DisassociateSubnetCidrBlock where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateSubnetCidrBlock where
        type Rs DisassociateSubnetCidrBlock =
             DisassociateSubnetCidrBlockResponse
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
                 DisassociateSubnetCidrBlockResponse' Core.<$>
                   (x Core..@? "ipv6CidrBlockAssociation") Core.<*>
                     x Core..@? "subnetId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateSubnetCidrBlockResponse' smart constructor.
data DisassociateSubnetCidrBlockResponse = DisassociateSubnetCidrBlockResponse'
  { ipv6CidrBlockAssociation :: Core.Maybe Types.SubnetIpv6CidrBlockAssociation
    -- ^ Information about the IPv6 CIDR block association.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSubnetCidrBlockResponse' value with any optional fields omitted.
mkDisassociateSubnetCidrBlockResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateSubnetCidrBlockResponse
mkDisassociateSubnetCidrBlockResponse responseStatus
  = DisassociateSubnetCidrBlockResponse'{ipv6CidrBlockAssociation =
                                           Core.Nothing,
                                         subnetId = Core.Nothing, responseStatus}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrrsIpv6CidrBlockAssociation :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Core.Maybe Types.SubnetIpv6CidrBlockAssociation)
dscbrrsIpv6CidrBlockAssociation = Lens.field @"ipv6CidrBlockAssociation"
{-# INLINEABLE dscbrrsIpv6CidrBlockAssociation #-}
{-# DEPRECATED ipv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead"  #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrrsSubnetId :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Core.Maybe Core.Text)
dscbrrsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE dscbrrsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrrsResponseStatus :: Lens.Lens' DisassociateSubnetCidrBlockResponse Core.Int
dscbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
