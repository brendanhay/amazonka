{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an internet gateway or a virtual private gateway to a VPC, enabling connectivity between the internet and the VPC. For more information about your VPC and internet gateway, see the <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide> .
module Network.AWS.EC2.AttachInternetGateway
  ( -- * Creating a request
    AttachInternetGateway (..),
    mkAttachInternetGateway,

    -- ** Request lenses
    aigInternetGatewayId,
    aigVpcId,
    aigDryRun,

    -- * Destructuring the response
    AttachInternetGatewayResponse (..),
    mkAttachInternetGatewayResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachInternetGateway' smart constructor.
data AttachInternetGateway = AttachInternetGateway'
  { -- | The ID of the internet gateway.
    internetGatewayId :: Types.InternetGatewayId,
    -- | The ID of the VPC.
    vpcId :: Types.VpcId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInternetGateway' value with any optional fields omitted.
mkAttachInternetGateway ::
  -- | 'internetGatewayId'
  Types.InternetGatewayId ->
  -- | 'vpcId'
  Types.VpcId ->
  AttachInternetGateway
mkAttachInternetGateway internetGatewayId vpcId =
  AttachInternetGateway'
    { internetGatewayId,
      vpcId,
      dryRun = Core.Nothing
    }

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigInternetGatewayId :: Lens.Lens' AttachInternetGateway Types.InternetGatewayId
aigInternetGatewayId = Lens.field @"internetGatewayId"
{-# DEPRECATED aigInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigVpcId :: Lens.Lens' AttachInternetGateway Types.VpcId
aigVpcId = Lens.field @"vpcId"
{-# DEPRECATED aigVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigDryRun :: Lens.Lens' AttachInternetGateway (Core.Maybe Core.Bool)
aigDryRun = Lens.field @"dryRun"
{-# DEPRECATED aigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest AttachInternetGateway where
  type Rs AttachInternetGateway = AttachInternetGatewayResponse
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
            ( Core.pure ("Action", "AttachInternetGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InternetGatewayId" internetGatewayId)
                Core.<> (Core.toQueryValue "VpcId" vpcId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull AttachInternetGatewayResponse'

-- | /See:/ 'mkAttachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse = AttachInternetGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInternetGatewayResponse' value with any optional fields omitted.
mkAttachInternetGatewayResponse ::
  AttachInternetGatewayResponse
mkAttachInternetGatewayResponse = AttachInternetGatewayResponse'
