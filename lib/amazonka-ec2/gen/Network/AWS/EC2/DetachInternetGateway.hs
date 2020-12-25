{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an internet gateway from a VPC, disabling connectivity between the internet and the VPC. The VPC must not contain any running instances with Elastic IP addresses or public IPv4 addresses.
module Network.AWS.EC2.DetachInternetGateway
  ( -- * Creating a request
    DetachInternetGateway (..),
    mkDetachInternetGateway,

    -- ** Request lenses
    digInternetGatewayId,
    digVpcId,
    digDryRun,

    -- * Destructuring the response
    DetachInternetGatewayResponse (..),
    mkDetachInternetGatewayResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachInternetGateway' smart constructor.
data DetachInternetGateway = DetachInternetGateway'
  { -- | The ID of the internet gateway.
    internetGatewayId :: Types.InternetGatewayId,
    -- | The ID of the VPC.
    vpcId :: Types.VpcId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachInternetGateway' value with any optional fields omitted.
mkDetachInternetGateway ::
  -- | 'internetGatewayId'
  Types.InternetGatewayId ->
  -- | 'vpcId'
  Types.VpcId ->
  DetachInternetGateway
mkDetachInternetGateway internetGatewayId vpcId =
  DetachInternetGateway'
    { internetGatewayId,
      vpcId,
      dryRun = Core.Nothing
    }

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digInternetGatewayId :: Lens.Lens' DetachInternetGateway Types.InternetGatewayId
digInternetGatewayId = Lens.field @"internetGatewayId"
{-# DEPRECATED digInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digVpcId :: Lens.Lens' DetachInternetGateway Types.VpcId
digVpcId = Lens.field @"vpcId"
{-# DEPRECATED digVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digDryRun :: Lens.Lens' DetachInternetGateway (Core.Maybe Core.Bool)
digDryRun = Lens.field @"dryRun"
{-# DEPRECATED digDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DetachInternetGateway where
  type Rs DetachInternetGateway = DetachInternetGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DetachInternetGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InternetGatewayId" internetGatewayId)
                Core.<> (Core.toQueryValue "VpcId" vpcId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DetachInternetGatewayResponse'

-- | /See:/ 'mkDetachInternetGatewayResponse' smart constructor.
data DetachInternetGatewayResponse = DetachInternetGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachInternetGatewayResponse' value with any optional fields omitted.
mkDetachInternetGatewayResponse ::
  DetachInternetGatewayResponse
mkDetachInternetGatewayResponse = DetachInternetGatewayResponse'
