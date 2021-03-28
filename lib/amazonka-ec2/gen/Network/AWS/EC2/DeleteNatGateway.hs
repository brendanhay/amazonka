{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates its Elastic IP address, but does not release the address from your account. Deleting a NAT gateway does not delete any NAT gateway routes in your route tables.
module Network.AWS.EC2.DeleteNatGateway
    (
    -- * Creating a request
      DeleteNatGateway (..)
    , mkDeleteNatGateway
    -- ** Request lenses
    , dngfNatGatewayId
    , dngfDryRun

    -- * Destructuring the response
    , DeleteNatGatewayResponse (..)
    , mkDeleteNatGatewayResponse
    -- ** Response lenses
    , dngrfrsNatGatewayId
    , dngrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'
  { natGatewayId :: Types.NatGatewayId
    -- ^ The ID of the NAT gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNatGateway' value with any optional fields omitted.
mkDeleteNatGateway
    :: Types.NatGatewayId -- ^ 'natGatewayId'
    -> DeleteNatGateway
mkDeleteNatGateway natGatewayId
  = DeleteNatGateway'{natGatewayId, dryRun = Core.Nothing}

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngfNatGatewayId :: Lens.Lens' DeleteNatGateway Types.NatGatewayId
dngfNatGatewayId = Lens.field @"natGatewayId"
{-# INLINEABLE dngfNatGatewayId #-}
{-# DEPRECATED natGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngfDryRun :: Lens.Lens' DeleteNatGateway (Core.Maybe Core.Bool)
dngfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dngfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteNatGateway where
        toQuery DeleteNatGateway{..}
          = Core.toQueryPair "Action" ("DeleteNatGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NatGatewayId" natGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteNatGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteNatGateway where
        type Rs DeleteNatGateway = DeleteNatGatewayResponse
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
                 DeleteNatGatewayResponse' Core.<$>
                   (x Core..@? "natGatewayId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
  { natGatewayId :: Core.Maybe Core.Text
    -- ^ The ID of the NAT gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNatGatewayResponse' value with any optional fields omitted.
mkDeleteNatGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteNatGatewayResponse
mkDeleteNatGatewayResponse responseStatus
  = DeleteNatGatewayResponse'{natGatewayId = Core.Nothing,
                              responseStatus}

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrfrsNatGatewayId :: Lens.Lens' DeleteNatGatewayResponse (Core.Maybe Core.Text)
dngrfrsNatGatewayId = Lens.field @"natGatewayId"
{-# INLINEABLE dngrfrsNatGatewayId #-}
{-# DEPRECATED natGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrfrsResponseStatus :: Lens.Lens' DeleteNatGatewayResponse Core.Int
dngrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dngrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
