{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachVpnGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a virtual private gateway from a VPC. You do this if you're planning to turn off the VPC and not use it anymore. You can confirm a virtual private gateway has been completely detached from a VPC by describing the virtual private gateway (any attachments to the virtual private gateway are also described).
--
-- You must wait for the attachment's state to switch to @detached@ before you can delete the VPC or attach a different VPC to the virtual private gateway.
module Network.AWS.EC2.DetachVpnGateway
    (
    -- * Creating a request
      DetachVpnGateway (..)
    , mkDetachVpnGateway
    -- ** Request lenses
    , dvgfVpcId
    , dvgfVpnGatewayId
    , dvgfDryRun

    -- * Destructuring the response
    , DetachVpnGatewayResponse (..)
    , mkDetachVpnGatewayResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DetachVpnGateway.
--
-- /See:/ 'mkDetachVpnGateway' smart constructor.
data DetachVpnGateway = DetachVpnGateway'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , vpnGatewayId :: Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachVpnGateway' value with any optional fields omitted.
mkDetachVpnGateway
    :: Types.VpcId -- ^ 'vpcId'
    -> Types.VpnGatewayId -- ^ 'vpnGatewayId'
    -> DetachVpnGateway
mkDetachVpnGateway vpcId vpnGatewayId
  = DetachVpnGateway'{vpcId, vpnGatewayId, dryRun = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgfVpcId :: Lens.Lens' DetachVpnGateway Types.VpcId
dvgfVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvgfVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgfVpnGatewayId :: Lens.Lens' DetachVpnGateway Types.VpnGatewayId
dvgfVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE dvgfVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgfDryRun :: Lens.Lens' DetachVpnGateway (Core.Maybe Core.Bool)
dvgfDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvgfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DetachVpnGateway where
        toQuery DetachVpnGateway{..}
          = Core.toQueryPair "Action" ("DetachVpnGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.toQueryPair "VpnGatewayId" vpnGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DetachVpnGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachVpnGateway where
        type Rs DetachVpnGateway = DetachVpnGatewayResponse
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
        parseResponse = Response.receiveNull DetachVpnGatewayResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachVpnGatewayResponse' smart constructor.
data DetachVpnGatewayResponse = DetachVpnGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachVpnGatewayResponse' value with any optional fields omitted.
mkDetachVpnGatewayResponse
    :: DetachVpnGatewayResponse
mkDetachVpnGatewayResponse = DetachVpnGatewayResponse'
