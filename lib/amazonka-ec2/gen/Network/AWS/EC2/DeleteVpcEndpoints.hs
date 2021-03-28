{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpcEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more specified VPC endpoints. Deleting a gateway endpoint also deletes the endpoint routes in the route tables that were associated with the endpoint. Deleting an interface endpoint or a Gateway Load Balancer endpoint deletes the endpoint network interfaces. Gateway Load Balancer endpoints can only be deleted if the routes that are associated with the endpoint are deleted.
module Network.AWS.EC2.DeleteVpcEndpoints
    (
    -- * Creating a request
      DeleteVpcEndpoints (..)
    , mkDeleteVpcEndpoints
    -- ** Request lenses
    , dveVpcEndpointIds
    , dveDryRun

    -- * Destructuring the response
    , DeleteVpcEndpointsResponse (..)
    , mkDeleteVpcEndpointsResponse
    -- ** Response lenses
    , dverrsUnsuccessful
    , dverrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpcEndpoints.
--
-- /See:/ 'mkDeleteVpcEndpoints' smart constructor.
data DeleteVpcEndpoints = DeleteVpcEndpoints'
  { vpcEndpointIds :: [Types.VpcEndpointId]
    -- ^ One or more VPC endpoint IDs.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcEndpoints' value with any optional fields omitted.
mkDeleteVpcEndpoints
    :: DeleteVpcEndpoints
mkDeleteVpcEndpoints
  = DeleteVpcEndpoints'{vpcEndpointIds = Core.mempty,
                        dryRun = Core.Nothing}

-- | One or more VPC endpoint IDs.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dveVpcEndpointIds :: Lens.Lens' DeleteVpcEndpoints [Types.VpcEndpointId]
dveVpcEndpointIds = Lens.field @"vpcEndpointIds"
{-# INLINEABLE dveVpcEndpointIds #-}
{-# DEPRECATED vpcEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dveDryRun :: Lens.Lens' DeleteVpcEndpoints (Core.Maybe Core.Bool)
dveDryRun = Lens.field @"dryRun"
{-# INLINEABLE dveDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpcEndpoints where
        toQuery DeleteVpcEndpoints{..}
          = Core.toQueryPair "Action" ("DeleteVpcEndpoints" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "VpcEndpointId" vpcEndpointIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpcEndpoints where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpcEndpoints where
        type Rs DeleteVpcEndpoints = DeleteVpcEndpointsResponse
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
                 DeleteVpcEndpointsResponse' Core.<$>
                   (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DeleteVpcEndpoints.
--
-- /See:/ 'mkDeleteVpcEndpointsResponse' smart constructor.
data DeleteVpcEndpointsResponse = DeleteVpcEndpointsResponse'
  { unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the VPC endpoints that were not successfully deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcEndpointsResponse' value with any optional fields omitted.
mkDeleteVpcEndpointsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVpcEndpointsResponse
mkDeleteVpcEndpointsResponse responseStatus
  = DeleteVpcEndpointsResponse'{unsuccessful = Core.Nothing,
                                responseStatus}

-- | Information about the VPC endpoints that were not successfully deleted.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dverrsUnsuccessful :: Lens.Lens' DeleteVpcEndpointsResponse (Core.Maybe [Types.UnsuccessfulItem])
dverrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE dverrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dverrsResponseStatus :: Lens.Lens' DeleteVpcEndpointsResponse Core.Int
dverrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dverrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
