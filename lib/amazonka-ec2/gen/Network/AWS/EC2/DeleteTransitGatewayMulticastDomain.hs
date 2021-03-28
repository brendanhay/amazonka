{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
    (
    -- * Creating a request
      DeleteTransitGatewayMulticastDomain (..)
    , mkDeleteTransitGatewayMulticastDomain
    -- ** Request lenses
    , dtgmdTransitGatewayMulticastDomainId
    , dtgmdDryRun

    -- * Destructuring the response
    , DeleteTransitGatewayMulticastDomainResponse (..)
    , mkDeleteTransitGatewayMulticastDomainResponse
    -- ** Response lenses
    , dtgmdrrsTransitGatewayMulticastDomain
    , dtgmdrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { transitGatewayMulticastDomainId :: Types.TransitGatewayMulticastDomainId
    -- ^ The ID of the transit gateway multicast domain.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTransitGatewayMulticastDomain' value with any optional fields omitted.
mkDeleteTransitGatewayMulticastDomain
    :: Types.TransitGatewayMulticastDomainId -- ^ 'transitGatewayMulticastDomainId'
    -> DeleteTransitGatewayMulticastDomain
mkDeleteTransitGatewayMulticastDomain
  transitGatewayMulticastDomainId
  = DeleteTransitGatewayMulticastDomain'{transitGatewayMulticastDomainId,
                                         dryRun = Core.Nothing}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdTransitGatewayMulticastDomainId :: Lens.Lens' DeleteTransitGatewayMulticastDomain Types.TransitGatewayMulticastDomainId
dtgmdTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE dtgmdTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdDryRun :: Lens.Lens' DeleteTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
dtgmdDryRun = Lens.field @"dryRun"
{-# INLINEABLE dtgmdDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteTransitGatewayMulticastDomain where
        toQuery DeleteTransitGatewayMulticastDomain{..}
          = Core.toQueryPair "Action"
              ("DeleteTransitGatewayMulticastDomain" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayMulticastDomainId"
                transitGatewayMulticastDomainId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteTransitGatewayMulticastDomain where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteTransitGatewayMulticastDomain where
        type Rs DeleteTransitGatewayMulticastDomain =
             DeleteTransitGatewayMulticastDomainResponse
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
                 DeleteTransitGatewayMulticastDomainResponse' Core.<$>
                   (x Core..@? "transitGatewayMulticastDomain") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { transitGatewayMulticastDomain :: Core.Maybe Types.TransitGatewayMulticastDomain
    -- ^ Information about the deleted transit gateway multicast domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteTransitGatewayMulticastDomainResponse' value with any optional fields omitted.
mkDeleteTransitGatewayMulticastDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTransitGatewayMulticastDomainResponse
mkDeleteTransitGatewayMulticastDomainResponse responseStatus
  = DeleteTransitGatewayMulticastDomainResponse'{transitGatewayMulticastDomain
                                                   = Core.Nothing,
                                                 responseStatus}

-- | Information about the deleted transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrrsTransitGatewayMulticastDomain :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse (Core.Maybe Types.TransitGatewayMulticastDomain)
dtgmdrrsTransitGatewayMulticastDomain = Lens.field @"transitGatewayMulticastDomain"
{-# INLINEABLE dtgmdrrsTransitGatewayMulticastDomain #-}
{-# DEPRECATED transitGatewayMulticastDomain "Use generic-lens or generic-optics with 'transitGatewayMulticastDomain' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrrsResponseStatus :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse Core.Int
dtgmdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgmdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
