{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AcceptVpcEndpointConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts one or more interface VPC endpoint connection requests to your VPC endpoint service.
module Network.AWS.EC2.AcceptVpcEndpointConnections
    (
    -- * Creating a request
      AcceptVpcEndpointConnections (..)
    , mkAcceptVpcEndpointConnections
    -- ** Request lenses
    , avecServiceId
    , avecVpcEndpointIds
    , avecDryRun

    -- * Destructuring the response
    , AcceptVpcEndpointConnectionsResponse (..)
    , mkAcceptVpcEndpointConnectionsResponse
    -- ** Response lenses
    , avecrrsUnsuccessful
    , avecrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAcceptVpcEndpointConnections' smart constructor.
data AcceptVpcEndpointConnections = AcceptVpcEndpointConnections'
  { serviceId :: Types.ServiceId
    -- ^ The ID of the VPC endpoint service.
  , vpcEndpointIds :: [Types.VpcEndpointId]
    -- ^ The IDs of one or more interface VPC endpoints.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptVpcEndpointConnections' value with any optional fields omitted.
mkAcceptVpcEndpointConnections
    :: Types.ServiceId -- ^ 'serviceId'
    -> AcceptVpcEndpointConnections
mkAcceptVpcEndpointConnections serviceId
  = AcceptVpcEndpointConnections'{serviceId,
                                  vpcEndpointIds = Core.mempty, dryRun = Core.Nothing}

-- | The ID of the VPC endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecServiceId :: Lens.Lens' AcceptVpcEndpointConnections Types.ServiceId
avecServiceId = Lens.field @"serviceId"
{-# INLINEABLE avecServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The IDs of one or more interface VPC endpoints.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecVpcEndpointIds :: Lens.Lens' AcceptVpcEndpointConnections [Types.VpcEndpointId]
avecVpcEndpointIds = Lens.field @"vpcEndpointIds"
{-# INLINEABLE avecVpcEndpointIds #-}
{-# DEPRECATED vpcEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecDryRun :: Lens.Lens' AcceptVpcEndpointConnections (Core.Maybe Core.Bool)
avecDryRun = Lens.field @"dryRun"
{-# INLINEABLE avecDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AcceptVpcEndpointConnections where
        toQuery AcceptVpcEndpointConnections{..}
          = Core.toQueryPair "Action"
              ("AcceptVpcEndpointConnections" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceId" serviceId
              Core.<> Core.toQueryList "VpcEndpointId" vpcEndpointIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AcceptVpcEndpointConnections where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AcceptVpcEndpointConnections where
        type Rs AcceptVpcEndpointConnections =
             AcceptVpcEndpointConnectionsResponse
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
                 AcceptVpcEndpointConnectionsResponse' Core.<$>
                   (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptVpcEndpointConnectionsResponse' smart constructor.
data AcceptVpcEndpointConnectionsResponse = AcceptVpcEndpointConnectionsResponse'
  { unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the interface endpoints that were not accepted, if applicable.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptVpcEndpointConnectionsResponse' value with any optional fields omitted.
mkAcceptVpcEndpointConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptVpcEndpointConnectionsResponse
mkAcceptVpcEndpointConnectionsResponse responseStatus
  = AcceptVpcEndpointConnectionsResponse'{unsuccessful =
                                            Core.Nothing,
                                          responseStatus}

-- | Information about the interface endpoints that were not accepted, if applicable.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecrrsUnsuccessful :: Lens.Lens' AcceptVpcEndpointConnectionsResponse (Core.Maybe [Types.UnsuccessfulItem])
avecrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE avecrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avecrrsResponseStatus :: Lens.Lens' AcceptVpcEndpointConnectionsResponse Core.Int
avecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE avecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
