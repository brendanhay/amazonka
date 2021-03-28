{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a route table for the specified transit gateway.
module Network.AWS.EC2.CreateTransitGatewayRouteTable
    (
    -- * Creating a request
      CreateTransitGatewayRouteTable (..)
    , mkCreateTransitGatewayRouteTable
    -- ** Request lenses
    , ctgrtTransitGatewayId
    , ctgrtDryRun
    , ctgrtTagSpecifications

    -- * Destructuring the response
    , CreateTransitGatewayRouteTableResponse (..)
    , mkCreateTransitGatewayRouteTableResponse
    -- ** Response lenses
    , ctgrtrrsTransitGatewayRouteTable
    , ctgrtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayRouteTable' smart constructor.
data CreateTransitGatewayRouteTable = CreateTransitGatewayRouteTable'
  { transitGatewayId :: Types.TransitGatewayId
    -- ^ The ID of the transit gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the transit gateway route table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayRouteTable' value with any optional fields omitted.
mkCreateTransitGatewayRouteTable
    :: Types.TransitGatewayId -- ^ 'transitGatewayId'
    -> CreateTransitGatewayRouteTable
mkCreateTransitGatewayRouteTable transitGatewayId
  = CreateTransitGatewayRouteTable'{transitGatewayId,
                                    dryRun = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtTransitGatewayId :: Lens.Lens' CreateTransitGatewayRouteTable Types.TransitGatewayId
ctgrtTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE ctgrtTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtDryRun :: Lens.Lens' CreateTransitGatewayRouteTable (Core.Maybe Core.Bool)
ctgrtDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctgrtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to apply to the transit gateway route table.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtTagSpecifications :: Lens.Lens' CreateTransitGatewayRouteTable (Core.Maybe [Types.TagSpecification])
ctgrtTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctgrtTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateTransitGatewayRouteTable where
        toQuery CreateTransitGatewayRouteTable{..}
          = Core.toQueryPair "Action"
              ("CreateTransitGatewayRouteTable" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "TransitGatewayId" transitGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecifications")
                tagSpecifications

instance Core.ToHeaders CreateTransitGatewayRouteTable where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTransitGatewayRouteTable where
        type Rs CreateTransitGatewayRouteTable =
             CreateTransitGatewayRouteTableResponse
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
                 CreateTransitGatewayRouteTableResponse' Core.<$>
                   (x Core..@? "transitGatewayRouteTable") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTransitGatewayRouteTableResponse' smart constructor.
data CreateTransitGatewayRouteTableResponse = CreateTransitGatewayRouteTableResponse'
  { transitGatewayRouteTable :: Core.Maybe Types.TransitGatewayRouteTable
    -- ^ Information about the transit gateway route table.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTransitGatewayRouteTableResponse' value with any optional fields omitted.
mkCreateTransitGatewayRouteTableResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTransitGatewayRouteTableResponse
mkCreateTransitGatewayRouteTableResponse responseStatus
  = CreateTransitGatewayRouteTableResponse'{transitGatewayRouteTable
                                              = Core.Nothing,
                                            responseStatus}

-- | Information about the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtrrsTransitGatewayRouteTable :: Lens.Lens' CreateTransitGatewayRouteTableResponse (Core.Maybe Types.TransitGatewayRouteTable)
ctgrtrrsTransitGatewayRouteTable = Lens.field @"transitGatewayRouteTable"
{-# INLINEABLE ctgrtrrsTransitGatewayRouteTable #-}
{-# DEPRECATED transitGatewayRouteTable "Use generic-lens or generic-optics with 'transitGatewayRouteTable' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrtrrsResponseStatus :: Lens.Lens' CreateTransitGatewayRouteTableResponse Core.Int
ctgrtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgrtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
