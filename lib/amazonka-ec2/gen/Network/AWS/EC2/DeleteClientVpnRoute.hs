{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteClientVpnRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a route from a Client VPN endpoint. You can only delete routes that you manually added using the __CreateClientVpnRoute__ action. You cannot delete routes that were automatically added when associating a subnet. To remove routes that have been automatically added, disassociate the target subnet from the Client VPN endpoint.
module Network.AWS.EC2.DeleteClientVpnRoute
    (
    -- * Creating a request
      DeleteClientVpnRoute (..)
    , mkDeleteClientVpnRoute
    -- ** Request lenses
    , dcvrfClientVpnEndpointId
    , dcvrfDestinationCidrBlock
    , dcvrfDryRun
    , dcvrfTargetVpcSubnetId

    -- * Destructuring the response
    , DeleteClientVpnRouteResponse (..)
    , mkDeleteClientVpnRouteResponse
    -- ** Response lenses
    , dcvrrfrsStatus
    , dcvrrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteClientVpnRoute' smart constructor.
data DeleteClientVpnRoute = DeleteClientVpnRoute'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint from which the route is to be deleted.
  , destinationCidrBlock :: Core.Text
    -- ^ The IPv4 address range, in CIDR notation, of the route to be deleted.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , targetVpcSubnetId :: Core.Maybe Types.SubnetId
    -- ^ The ID of the target subnet used by the route.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClientVpnRoute' value with any optional fields omitted.
mkDeleteClientVpnRoute
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Core.Text -- ^ 'destinationCidrBlock'
    -> DeleteClientVpnRoute
mkDeleteClientVpnRoute clientVpnEndpointId destinationCidrBlock
  = DeleteClientVpnRoute'{clientVpnEndpointId, destinationCidrBlock,
                          dryRun = Core.Nothing, targetVpcSubnetId = Core.Nothing}

-- | The ID of the Client VPN endpoint from which the route is to be deleted.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrfClientVpnEndpointId :: Lens.Lens' DeleteClientVpnRoute Types.ClientVpnEndpointId
dcvrfClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvrfClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The IPv4 address range, in CIDR notation, of the route to be deleted.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrfDestinationCidrBlock :: Lens.Lens' DeleteClientVpnRoute Core.Text
dcvrfDestinationCidrBlock = Lens.field @"destinationCidrBlock"
{-# INLINEABLE dcvrfDestinationCidrBlock #-}
{-# DEPRECATED destinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrfDryRun :: Lens.Lens' DeleteClientVpnRoute (Core.Maybe Core.Bool)
dcvrfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvrfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the target subnet used by the route.
--
-- /Note:/ Consider using 'targetVpcSubnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrfTargetVpcSubnetId :: Lens.Lens' DeleteClientVpnRoute (Core.Maybe Types.SubnetId)
dcvrfTargetVpcSubnetId = Lens.field @"targetVpcSubnetId"
{-# INLINEABLE dcvrfTargetVpcSubnetId #-}
{-# DEPRECATED targetVpcSubnetId "Use generic-lens or generic-optics with 'targetVpcSubnetId' instead"  #-}

instance Core.ToQuery DeleteClientVpnRoute where
        toQuery DeleteClientVpnRoute{..}
          = Core.toQueryPair "Action" ("DeleteClientVpnRoute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<>
              Core.toQueryPair "DestinationCidrBlock" destinationCidrBlock
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetVpcSubnetId")
                targetVpcSubnetId

instance Core.ToHeaders DeleteClientVpnRoute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteClientVpnRoute where
        type Rs DeleteClientVpnRoute = DeleteClientVpnRouteResponse
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
                 DeleteClientVpnRouteResponse' Core.<$>
                   (x Core..@? "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClientVpnRouteResponse' smart constructor.
data DeleteClientVpnRouteResponse = DeleteClientVpnRouteResponse'
  { status :: Core.Maybe Types.ClientVpnRouteStatus
    -- ^ The current state of the route.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClientVpnRouteResponse' value with any optional fields omitted.
mkDeleteClientVpnRouteResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClientVpnRouteResponse
mkDeleteClientVpnRouteResponse responseStatus
  = DeleteClientVpnRouteResponse'{status = Core.Nothing,
                                  responseStatus}

-- | The current state of the route.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrfrsStatus :: Lens.Lens' DeleteClientVpnRouteResponse (Core.Maybe Types.ClientVpnRouteStatus)
dcvrrfrsStatus = Lens.field @"status"
{-# INLINEABLE dcvrrfrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvrrfrsResponseStatus :: Lens.Lens' DeleteClientVpnRouteResponse Core.Int
dcvrrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcvrrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
