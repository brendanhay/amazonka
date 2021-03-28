{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpnGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual private gateway. You must first detach the virtual private gateway from the VPC. Note that you don't need to delete the virtual private gateway if you plan to delete and recreate the VPN connection between your VPC and your network.
module Network.AWS.EC2.DeleteVpnGateway
    (
    -- * Creating a request
      DeleteVpnGateway (..)
    , mkDeleteVpnGateway
    -- ** Request lenses
    , dvgVpnGatewayId
    , dvgDryRun

    -- * Destructuring the response
    , DeleteVpnGatewayResponse (..)
    , mkDeleteVpnGatewayResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpnGateway.
--
-- /See:/ 'mkDeleteVpnGateway' smart constructor.
data DeleteVpnGateway = DeleteVpnGateway'
  { vpnGatewayId :: Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpnGateway' value with any optional fields omitted.
mkDeleteVpnGateway
    :: Types.VpnGatewayId -- ^ 'vpnGatewayId'
    -> DeleteVpnGateway
mkDeleteVpnGateway vpnGatewayId
  = DeleteVpnGateway'{vpnGatewayId, dryRun = Core.Nothing}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgVpnGatewayId :: Lens.Lens' DeleteVpnGateway Types.VpnGatewayId
dvgVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE dvgVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgDryRun :: Lens.Lens' DeleteVpnGateway (Core.Maybe Core.Bool)
dvgDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpnGateway where
        toQuery DeleteVpnGateway{..}
          = Core.toQueryPair "Action" ("DeleteVpnGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpnGatewayId" vpnGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpnGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpnGateway where
        type Rs DeleteVpnGateway = DeleteVpnGatewayResponse
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
        parseResponse = Response.receiveNull DeleteVpnGatewayResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpnGatewayResponse' smart constructor.
data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpnGatewayResponse' value with any optional fields omitted.
mkDeleteVpnGatewayResponse
    :: DeleteVpnGatewayResponse
mkDeleteVpnGatewayResponse = DeleteVpnGatewayResponse'
