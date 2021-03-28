{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVgwRoutePropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a virtual private gateway (VGW) to propagate routes to the specified route table of a VPC.
module Network.AWS.EC2.EnableVgwRoutePropagation
    (
    -- * Creating a request
      EnableVgwRoutePropagation (..)
    , mkEnableVgwRoutePropagation
    -- ** Request lenses
    , evrpGatewayId
    , evrpRouteTableId
    , evrpDryRun

    -- * Destructuring the response
    , EnableVgwRoutePropagationResponse (..)
    , mkEnableVgwRoutePropagationResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for EnableVgwRoutePropagation.
--
-- /See:/ 'mkEnableVgwRoutePropagation' smart constructor.
data EnableVgwRoutePropagation = EnableVgwRoutePropagation'
  { gatewayId :: Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway that is attached to a VPC. The virtual private gateway must be attached to the same VPC that the routing tables are associated with. 
  , routeTableId :: Types.RouteTableId
    -- ^ The ID of the route table. The routing table must be associated with the same VPC that the virtual private gateway is attached to. 
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVgwRoutePropagation' value with any optional fields omitted.
mkEnableVgwRoutePropagation
    :: Types.VpnGatewayId -- ^ 'gatewayId'
    -> Types.RouteTableId -- ^ 'routeTableId'
    -> EnableVgwRoutePropagation
mkEnableVgwRoutePropagation gatewayId routeTableId
  = EnableVgwRoutePropagation'{gatewayId, routeTableId,
                               dryRun = Core.Nothing}

-- | The ID of the virtual private gateway that is attached to a VPC. The virtual private gateway must be attached to the same VPC that the routing tables are associated with. 
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evrpGatewayId :: Lens.Lens' EnableVgwRoutePropagation Types.VpnGatewayId
evrpGatewayId = Lens.field @"gatewayId"
{-# INLINEABLE evrpGatewayId #-}
{-# DEPRECATED gatewayId "Use generic-lens or generic-optics with 'gatewayId' instead"  #-}

-- | The ID of the route table. The routing table must be associated with the same VPC that the virtual private gateway is attached to. 
--
-- /Note:/ Consider using 'routeTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evrpRouteTableId :: Lens.Lens' EnableVgwRoutePropagation Types.RouteTableId
evrpRouteTableId = Lens.field @"routeTableId"
{-# INLINEABLE evrpRouteTableId #-}
{-# DEPRECATED routeTableId "Use generic-lens or generic-optics with 'routeTableId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evrpDryRun :: Lens.Lens' EnableVgwRoutePropagation (Core.Maybe Core.Bool)
evrpDryRun = Lens.field @"dryRun"
{-# INLINEABLE evrpDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery EnableVgwRoutePropagation where
        toQuery EnableVgwRoutePropagation{..}
          = Core.toQueryPair "Action"
              ("EnableVgwRoutePropagation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "GatewayId" gatewayId
              Core.<> Core.toQueryPair "RouteTableId" routeTableId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders EnableVgwRoutePropagation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableVgwRoutePropagation where
        type Rs EnableVgwRoutePropagation =
             EnableVgwRoutePropagationResponse
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
          = Response.receiveNull EnableVgwRoutePropagationResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableVgwRoutePropagationResponse' smart constructor.
data EnableVgwRoutePropagationResponse = EnableVgwRoutePropagationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVgwRoutePropagationResponse' value with any optional fields omitted.
mkEnableVgwRoutePropagationResponse
    :: EnableVgwRoutePropagationResponse
mkEnableVgwRoutePropagationResponse
  = EnableVgwRoutePropagationResponse'
