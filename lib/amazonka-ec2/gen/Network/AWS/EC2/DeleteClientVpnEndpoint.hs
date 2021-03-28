{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteClientVpnEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Client VPN endpoint. You must disassociate all target networks before you can delete a Client VPN endpoint.
module Network.AWS.EC2.DeleteClientVpnEndpoint
    (
    -- * Creating a request
      DeleteClientVpnEndpoint (..)
    , mkDeleteClientVpnEndpoint
    -- ** Request lenses
    , dcvefClientVpnEndpointId
    , dcvefDryRun

    -- * Destructuring the response
    , DeleteClientVpnEndpointResponse (..)
    , mkDeleteClientVpnEndpointResponse
    -- ** Response lenses
    , dcverfrsStatus
    , dcverfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteClientVpnEndpoint' smart constructor.
data DeleteClientVpnEndpoint = DeleteClientVpnEndpoint'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN to be deleted.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClientVpnEndpoint' value with any optional fields omitted.
mkDeleteClientVpnEndpoint
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> DeleteClientVpnEndpoint
mkDeleteClientVpnEndpoint clientVpnEndpointId
  = DeleteClientVpnEndpoint'{clientVpnEndpointId,
                             dryRun = Core.Nothing}

-- | The ID of the Client VPN to be deleted.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvefClientVpnEndpointId :: Lens.Lens' DeleteClientVpnEndpoint Types.ClientVpnEndpointId
dcvefClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE dcvefClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvefDryRun :: Lens.Lens' DeleteClientVpnEndpoint (Core.Maybe Core.Bool)
dcvefDryRun = Lens.field @"dryRun"
{-# INLINEABLE dcvefDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteClientVpnEndpoint where
        toQuery DeleteClientVpnEndpoint{..}
          = Core.toQueryPair "Action"
              ("DeleteClientVpnEndpoint" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteClientVpnEndpoint where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteClientVpnEndpoint where
        type Rs DeleteClientVpnEndpoint = DeleteClientVpnEndpointResponse
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
                 DeleteClientVpnEndpointResponse' Core.<$>
                   (x Core..@? "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteClientVpnEndpointResponse' smart constructor.
data DeleteClientVpnEndpointResponse = DeleteClientVpnEndpointResponse'
  { status :: Core.Maybe Types.ClientVpnEndpointStatus
    -- ^ The current state of the Client VPN endpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClientVpnEndpointResponse' value with any optional fields omitted.
mkDeleteClientVpnEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteClientVpnEndpointResponse
mkDeleteClientVpnEndpointResponse responseStatus
  = DeleteClientVpnEndpointResponse'{status = Core.Nothing,
                                     responseStatus}

-- | The current state of the Client VPN endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcverfrsStatus :: Lens.Lens' DeleteClientVpnEndpointResponse (Core.Maybe Types.ClientVpnEndpointStatus)
dcverfrsStatus = Lens.field @"status"
{-# INLINEABLE dcverfrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcverfrsResponseStatus :: Lens.Lens' DeleteClientVpnEndpointResponse Core.Int
dcverfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcverfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
