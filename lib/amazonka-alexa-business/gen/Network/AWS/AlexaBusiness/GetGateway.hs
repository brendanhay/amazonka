{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway.
module Network.AWS.AlexaBusiness.GetGateway
  ( -- * Creating a request
    GetGateway (..),
    mkGetGateway,

    -- ** Request lenses
    ggGatewayArn,

    -- * Destructuring the response
    GetGatewayResponse (..),
    mkGetGatewayResponse,

    -- ** Response lenses
    ggrrsGateway,
    ggrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGateway' smart constructor.
newtype GetGateway = GetGateway'
  { -- | The ARN of the gateway to get.
    gatewayArn :: Types.GatewayArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGateway' value with any optional fields omitted.
mkGetGateway ::
  -- | 'gatewayArn'
  Types.GatewayArn ->
  GetGateway
mkGetGateway gatewayArn = GetGateway' {gatewayArn}

-- | The ARN of the gateway to get.
--
-- /Note:/ Consider using 'gatewayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGatewayArn :: Lens.Lens' GetGateway Types.GatewayArn
ggGatewayArn = Lens.field @"gatewayArn"
{-# DEPRECATED ggGatewayArn "Use generic-lens or generic-optics with 'gatewayArn' instead." #-}

instance Core.FromJSON GetGateway where
  toJSON GetGateway {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayArn" Core..= gatewayArn)])

instance Core.AWSRequest GetGateway where
  type Rs GetGateway = GetGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetGateway")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayResponse'
            Core.<$> (x Core..:? "Gateway") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGatewayResponse' smart constructor.
data GetGatewayResponse = GetGatewayResponse'
  { -- | The details of the gateway.
    gateway :: Core.Maybe Types.Gateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayResponse' value with any optional fields omitted.
mkGetGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGatewayResponse
mkGetGatewayResponse responseStatus =
  GetGatewayResponse' {gateway = Core.Nothing, responseStatus}

-- | The details of the gateway.
--
-- /Note:/ Consider using 'gateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsGateway :: Lens.Lens' GetGatewayResponse (Core.Maybe Types.Gateway)
ggrrsGateway = Lens.field @"gateway"
{-# DEPRECATED ggrrsGateway "Use generic-lens or generic-optics with 'gateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrrsResponseStatus :: Lens.Lens' GetGatewayResponse Core.Int
ggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
