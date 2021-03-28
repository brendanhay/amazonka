{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisableVgwRoutePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a virtual private gateway (VGW) from propagating routes to a specified route table of a VPC.
module Network.AWS.EC2.DisableVgwRoutePropagation
    (
    -- * Creating a request
      DisableVgwRoutePropagation (..)
    , mkDisableVgwRoutePropagation
    -- ** Request lenses
    , dvrpGatewayId
    , dvrpRouteTableId
    , dvrpDryRun

    -- * Destructuring the response
    , DisableVgwRoutePropagationResponse (..)
    , mkDisableVgwRoutePropagationResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DisableVgwRoutePropagation.
--
-- /See:/ 'mkDisableVgwRoutePropagation' smart constructor.
data DisableVgwRoutePropagation = DisableVgwRoutePropagation'
  { gatewayId :: Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway.
  , routeTableId :: Types.RouteTableId
    -- ^ The ID of the route table.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableVgwRoutePropagation' value with any optional fields omitted.
mkDisableVgwRoutePropagation
    :: Types.VpnGatewayId -- ^ 'gatewayId'
    -> Types.RouteTableId -- ^ 'routeTableId'
    -> DisableVgwRoutePropagation
mkDisableVgwRoutePropagation gatewayId routeTableId
  = DisableVgwRoutePropagation'{gatewayId, routeTableId,
                                dryRun = Core.Nothing}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrpGatewayId :: Lens.Lens' DisableVgwRoutePropagation Types.VpnGatewayId
dvrpGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE dvrpGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The ID of the route table.
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrpRouteTableId :: Lens.Lens' DisableVgwRoutePropagation Types.RouteTableId
dvrpRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE dvrpRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrpDryRun :: Lens.Lens' DisableVgwRoutePropagation (Core.Maybe Core.Bool)
dvrpDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvrpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DisableVgwRoutePropagation where
        toQuery DisableVgwRoutePropagation{..}
          = Core.toQueryPair "Action"
              ("DisableVgwRoutePropagation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "GatewayId" gatewayId
              Core.<> Core.toQueryPair "RouteTableId" routeTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DisableVgwRoutePropagation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableVgwRoutePropagation where
        type Rs DisableVgwRoutePropagation =
             DisableVgwRoutePropagationResponse
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
          = Response.receiveNull DisableVgwRoutePropagationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableVgwRoutePropagationResponse' smart constructor.
data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableVgwRoutePropagationResponse' value with any optional fields omitted.
mkDisableVgwRoutePropagationResponse
    :: DisableVgwRoutePropagationResponse
mkDisableVgwRoutePropagationResponse
  = DisableVgwRoutePropagationResponse'
