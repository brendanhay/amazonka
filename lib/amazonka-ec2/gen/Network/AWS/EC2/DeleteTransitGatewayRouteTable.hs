{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway route table. You must disassociate the route table from any transit gateway route tables before you can delete it.
module Network.AWS.EC2.DeleteTransitGatewayRouteTable
    (
    -- * Creating a request
      DeleteTransitGatewayRouteTable (..)
    , mkDeleteTransitGatewayRouteTable
    -- ** Request lenses
    , dtgrtTransitGatewayRouteTableId
    , dtgrtDryRun

    -- * Destructuring the response
    , DeleteTransitGatewayRouteTableResponse (..)
    , mkDeleteTransitGatewayRouteTableResponse
    -- ** Response lenses
    , dtgrtrrsTransitGatewayRouteTable
    , dtgrtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayRouteTable' smart constructor.
data DeleteTransitGatewayRouteTable = DeleteTransitGatewayRouteTable'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayRouteTable' value with any optional fields omitted.
mkDeleteTransitGatewayRouteTable
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> DeleteTransitGatewayRouteTable
mkDeleteTransitGatewayRouteTable transitGatewayRouteTableId
  = DeleteTransitGatewayRouteTable'{transitGatewayRouteTableId,
                                    dryRun = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtTransitGatewayRouteTableId :: Lens.Lens' DeleteTransitGatewayRouteTable Types.TransitGatewayRouteTableId
dtgrtTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE dtgrtTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtDryRun :: Lens.Lens' DeleteTransitGatewayRouteTable (Core.Maybe Core.Bool)
dtgrtDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgrtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTransitGatewayRouteTable where
        toQuery DeleteTransitGatewayRouteTable{..}
          = Core.toQueryPair "Action"
              ("DeleteTransitGatewayRouteTable" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTransitGatewayRouteTable where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTransitGatewayRouteTable where
        type Rs DeleteTransitGatewayRouteTable =
             DeleteTransitGatewayRouteTableResponse
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
                 DeleteTransitGatewayRouteTableResponse' Core.<$>
                   (x Core..@? "transitGatewayRouteTable") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTransitGatewayRouteTableResponse' smart constructor.
data DeleteTransitGatewayRouteTableResponse = DeleteTransitGatewayRouteTableResponse'
  { transitGatewayRouteTable :: Core.Maybe Types.TransitGatewayRouteTable
    -- ^ Information about the deleted transit gateway route table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkDeleteTransitGatewayRouteTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTransitGatewayRouteTableResponse
mkDeleteTransitGatewayRouteTableResponse responseStatus
  = DeleteTransitGatewayRouteTableResponse'{transitGatewayRouteTable
                                              = Core.Nothing,
                                            responseStatus}

-- | Information about the deleted transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrrsTransitGatewayRouteTable :: Lens.Lens' DeleteTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayRouteTable)
dtgrtrrsTransitGatewayRouteTable = Lens.field @"transitGatewayRouteTable"
{-# INLINEABLE dtgrtrrsTransitGatewayRouteTable #-}
{-# DEPRECATED transitGatewayRouteTable "Use generic-lens or generic-optics with 'transitGatewayRouteTable' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrtrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayRouteTableResponse Core.Int
dtgrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
