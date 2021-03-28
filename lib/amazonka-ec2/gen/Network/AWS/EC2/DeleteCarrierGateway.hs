{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteCarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a carrier gateway.
--
-- /Important:/ If you do not delete the route that contains the carrier gateway as the Target, the route is a blackhole route. For information about how to delete a route, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteRoute.html DeleteRoute> .
module Network.AWS.EC2.DeleteCarrierGateway
    (
    -- * Creating a request
      DeleteCarrierGateway (..)
    , mkDeleteCarrierGateway
    -- ** Request lenses
    , dcgfCarrierGatewayId
    , dcgfDryRun

    -- * Destructuring the response
    , DeleteCarrierGatewayResponse (..)
    , mkDeleteCarrierGatewayResponse
    -- ** Response lenses
    , dcgrgrsCarrierGateway
    , dcgrgrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCarrierGateway' smart constructor.
data DeleteCarrierGateway = DeleteCarrierGateway'
  { carrierGatewayId :: Types.CarrierGatewayId
    -- ^ The ID of the carrier gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCarrierGateway' value with any optional fields omitted.
mkDeleteCarrierGateway
    :: Types.CarrierGatewayId -- ^ 'carrierGatewayId'
    -> DeleteCarrierGateway
mkDeleteCarrierGateway carrierGatewayId
  = DeleteCarrierGateway'{carrierGatewayId, dryRun = Core.Nothing}

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgfCarrierGatewayId :: Lens.Lens' DeleteCarrierGateway Types.CarrierGatewayId
dcgfCarrierGatewayId = Lens.field @"carrierGatewayId"
{-# INLINEABLE dcgfCarrierGatewayId #-}
{-# DEPRECATED carrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgfDryRun :: Lens.Lens' DeleteCarrierGateway (Core.Maybe Core.Bool)
dcgfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcgfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteCarrierGateway where
        toQuery DeleteCarrierGateway{..}
          = Core.toQueryPair "Action" ("DeleteCarrierGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "CarrierGatewayId" carrierGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteCarrierGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCarrierGateway where
        type Rs DeleteCarrierGateway = DeleteCarrierGatewayResponse
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
                 DeleteCarrierGatewayResponse' Core.<$>
                   (x Core..@? "carrierGateway") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCarrierGatewayResponse' smart constructor.
data DeleteCarrierGatewayResponse = DeleteCarrierGatewayResponse'
  { carrierGateway :: Core.Maybe Types.CarrierGateway
    -- ^ Information about the carrier gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCarrierGatewayResponse' value with any optional fields omitted.
mkDeleteCarrierGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCarrierGatewayResponse
mkDeleteCarrierGatewayResponse responseStatus
  = DeleteCarrierGatewayResponse'{carrierGateway = Core.Nothing,
                                  responseStatus}

-- | Information about the carrier gateway.
--
-- /Note:/ Consider using 'carrierGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrgrsCarrierGateway :: Lens.Lens' DeleteCarrierGatewayResponse (Core.Maybe Types.CarrierGateway)
dcgrgrsCarrierGateway = Lens.field @"carrierGateway"
{-# INLINEABLE dcgrgrsCarrierGateway #-}
{-# DEPRECATED carrierGateway "Use generic-lens or generic-optics with 'carrierGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrgrsResponseStatus :: Lens.Lens' DeleteCarrierGatewayResponse Core.Int
dcgrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcgrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
