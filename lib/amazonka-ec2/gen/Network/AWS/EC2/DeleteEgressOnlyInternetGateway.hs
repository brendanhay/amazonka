{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
    (
    -- * Creating a request
      DeleteEgressOnlyInternetGateway (..)
    , mkDeleteEgressOnlyInternetGateway
    -- ** Request lenses
    , deoigfEgressOnlyInternetGatewayId
    , deoigfDryRun

    -- * Destructuring the response
    , DeleteEgressOnlyInternetGatewayResponse (..)
    , mkDeleteEgressOnlyInternetGatewayResponse
    -- ** Response lenses
    , deoigrfrsReturnCode
    , deoigrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { egressOnlyInternetGatewayId :: Types.EgressOnlyInternetGatewayId
    -- ^ The ID of the egress-only internet gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEgressOnlyInternetGateway' value with any optional fields omitted.
mkDeleteEgressOnlyInternetGateway
    :: Types.EgressOnlyInternetGatewayId -- ^ 'egressOnlyInternetGatewayId'
    -> DeleteEgressOnlyInternetGateway
mkDeleteEgressOnlyInternetGateway egressOnlyInternetGatewayId
  = DeleteEgressOnlyInternetGateway'{egressOnlyInternetGatewayId,
                                     dryRun = Core.Nothing}

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigfEgressOnlyInternetGatewayId :: Lens.Lens' DeleteEgressOnlyInternetGateway Types.EgressOnlyInternetGatewayId
deoigfEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# INLINEABLE deoigfEgressOnlyInternetGatewayId #-}
{-# DEPRECATED egressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigfDryRun :: Lens.Lens' DeleteEgressOnlyInternetGateway (Core.Maybe Core.Bool)
deoigfDryRun = Lens.field @"dryRun"
{-# INLINEABLE deoigfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteEgressOnlyInternetGateway where
        toQuery DeleteEgressOnlyInternetGateway{..}
          = Core.toQueryPair "Action"
              ("DeleteEgressOnlyInternetGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "EgressOnlyInternetGatewayId"
                egressOnlyInternetGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteEgressOnlyInternetGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteEgressOnlyInternetGateway where
        type Rs DeleteEgressOnlyInternetGateway =
             DeleteEgressOnlyInternetGatewayResponse
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
                 DeleteEgressOnlyInternetGatewayResponse' Core.<$>
                   (x Core..@? "returnCode") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'
  { returnCode :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEgressOnlyInternetGatewayResponse' value with any optional fields omitted.
mkDeleteEgressOnlyInternetGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteEgressOnlyInternetGatewayResponse
mkDeleteEgressOnlyInternetGatewayResponse responseStatus
  = DeleteEgressOnlyInternetGatewayResponse'{returnCode =
                                               Core.Nothing,
                                             responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrfrsReturnCode :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse (Core.Maybe Core.Bool)
deoigrfrsReturnCode = Lens.field @"returnCode"
{-# INLINEABLE deoigrfrsReturnCode #-}
{-# DEPRECATED returnCode "Use generic-lens or generic-optics with 'returnCode' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrfrsResponseStatus :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse Core.Int
deoigrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE deoigrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
