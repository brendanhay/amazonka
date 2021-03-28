{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
module Network.AWS.EC2.DisassociateTransitGatewayRouteTable
    (
    -- * Creating a request
      DisassociateTransitGatewayRouteTable (..)
    , mkDisassociateTransitGatewayRouteTable
    -- ** Request lenses
    , dtgrtfTransitGatewayRouteTableId
    , dtgrtfTransitGatewayAttachmentId
    , dtgrtfDryRun

    -- * Destructuring the response
    , DisassociateTransitGatewayRouteTableResponse (..)
    , mkDisassociateTransitGatewayRouteTableResponse
    -- ** Response lenses
    , dtgrtrfrsAssociation
    , dtgrtrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayRouteTable' value with any optional fields omitted.
mkDisassociateTransitGatewayRouteTable
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> DisassociateTransitGatewayRouteTable
mkDisassociateTransitGatewayRouteTable transitGatewayRouteTableId
  transitGatewayAttachmentId
  = DisassociateTransitGatewayRouteTable'{transitGatewayRouteTableId,
                                          transitGatewayAttachmentId, dryRun = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtfTransitGatewayRouteTableId :: Lens.Lens' DisassociateTransitGatewayRouteTable Types.TransitGatewayRouteTableId
dtgrtfTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE dtgrtfTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtfTransitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayRouteTable Types.TransitGatewayAttachmentId
dtgrtfTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE dtgrtfTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtfDryRun :: Lens.Lens' DisassociateTransitGatewayRouteTable (Core.Maybe Core.Bool)
dtgrtfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgrtfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisassociateTransitGatewayRouteTable where
        toQuery DisassociateTransitGatewayRouteTable{..}
          = Core.toQueryPair "Action"
              ("DisassociateTransitGatewayRouteTable" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisassociateTransitGatewayRouteTable where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisassociateTransitGatewayRouteTable where
        type Rs DisassociateTransitGatewayRouteTable =
             DisassociateTransitGatewayRouteTableResponse
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
                 DisassociateTransitGatewayRouteTableResponse' Core.<$>
                   (x Core..@? "association") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'
  { association :: Core.Maybe Types.TransitGatewayAssociation
    -- ^ Information about the association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkDisassociateTransitGatewayRouteTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateTransitGatewayRouteTableResponse
mkDisassociateTransitGatewayRouteTableResponse responseStatus
  = DisassociateTransitGatewayRouteTableResponse'{association =
                                                    Core.Nothing,
                                                  responseStatus}

-- | Information about the association.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrfrsAssociation :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayAssociation)
dtgrtrfrsAssociation = Lens.field @"association"
{-# INLINEABLE dtgrtrfrsAssociation #-}
{-# DEPRECATED association "Use generic-lens or generic-optics with 'association' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrfrsResponseStatus :: Lens.Lens' DisassociateTransitGatewayRouteTableResponse Core.Int
dtgrtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
