{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RejectVpcEndpointConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects one or more VPC endpoint connection requests to your VPC endpoint service.
module Network.AWS.EC2.RejectVpcEndpointConnections
    (
    -- * Creating a request
      RejectVpcEndpointConnections (..)
    , mkRejectVpcEndpointConnections
    -- ** Request lenses
    , rvecServiceId
    , rvecVpcEndpointIds
    , rvecDryRun

    -- * Destructuring the response
    , RejectVpcEndpointConnectionsResponse (..)
    , mkRejectVpcEndpointConnectionsResponse
    -- ** Response lenses
    , rvecrrsUnsuccessful
    , rvecrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectVpcEndpointConnections' smart constructor.
data RejectVpcEndpointConnections = RejectVpcEndpointConnections'
  { serviceId :: Types.VpcEndpointServiceId
    -- ^ The ID of the service.
  , vpcEndpointIds :: [Types.VpcEndpointId]
    -- ^ The IDs of one or more VPC endpoints.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectVpcEndpointConnections' value with any optional fields omitted.
mkRejectVpcEndpointConnections
    :: Types.VpcEndpointServiceId -- ^ 'serviceId'
    -> RejectVpcEndpointConnections
mkRejectVpcEndpointConnections serviceId
  = RejectVpcEndpointConnections'{serviceId,
                                  vpcEndpointIds = Core.mempty, dryRun = Core.Nothing}

-- | The ID of the service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecServiceId :: Lens.Lens' RejectVpcEndpointConnections Types.VpcEndpointServiceId
rvecServiceId = Lens.field @"serviceId"
{-# INLINEABLE rvecServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The IDs of one or more VPC endpoints.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecVpcEndpointIds :: Lens.Lens' RejectVpcEndpointConnections [Types.VpcEndpointId]
rvecVpcEndpointIds = Lens.field @"vpcEndpointIds"
{-# INLINEABLE rvecVpcEndpointIds #-}
{-# DEPRECATED vpcEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecDryRun :: Lens.Lens' RejectVpcEndpointConnections (Core.Maybe Core.Bool)
rvecDryRun = Lens.field @"dryRun"
{-# INLINEABLE rvecDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery RejectVpcEndpointConnections where
        toQuery RejectVpcEndpointConnections{..}
          = Core.toQueryPair "Action"
              ("RejectVpcEndpointConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceId" serviceId
              Core.<> Core.toQueryList "VpcEndpointId" vpcEndpointIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders RejectVpcEndpointConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RejectVpcEndpointConnections where
        type Rs RejectVpcEndpointConnections =
             RejectVpcEndpointConnectionsResponse
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
                 RejectVpcEndpointConnectionsResponse' Core.<$>
                   (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectVpcEndpointConnectionsResponse' smart constructor.
data RejectVpcEndpointConnectionsResponse = RejectVpcEndpointConnectionsResponse'
  { unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the endpoints that were not rejected, if applicable.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectVpcEndpointConnectionsResponse' value with any optional fields omitted.
mkRejectVpcEndpointConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectVpcEndpointConnectionsResponse
mkRejectVpcEndpointConnectionsResponse responseStatus
  = RejectVpcEndpointConnectionsResponse'{unsuccessful =
                                            Core.Nothing,
                                          responseStatus}

-- | Information about the endpoints that were not rejected, if applicable.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecrrsUnsuccessful :: Lens.Lens' RejectVpcEndpointConnectionsResponse (Core.Maybe [Types.UnsuccessfulItem])
rvecrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE rvecrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvecrrsResponseStatus :: Lens.Lens' RejectVpcEndpointConnectionsResponse Core.Int
rvecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rvecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
