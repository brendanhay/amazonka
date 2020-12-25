{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a gateway that you previously shut down (see 'ShutdownGateway' ). After the gateway starts, you can then make other API calls, your applications can read from or write to the gateway's storage volumes and you will be able to take snapshot backups.
--
-- To specify which gateway to start, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.StartGateway
  ( -- * Creating a request
    StartGateway (..),
    mkStartGateway,

    -- ** Request lenses
    sgGatewayARN,

    -- * Destructuring the response
    StartGatewayResponse (..),
    mkStartGatewayResponse,

    -- ** Response lenses
    sgrrsGatewayARN,
    sgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway to start.
--
-- /See:/ 'mkStartGateway' smart constructor.
newtype StartGateway = StartGateway'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartGateway' value with any optional fields omitted.
mkStartGateway ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  StartGateway
mkStartGateway gatewayARN = StartGateway' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGatewayARN :: Lens.Lens' StartGateway Types.GatewayARN
sgGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED sgGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON StartGateway where
  toJSON StartGateway {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest StartGateway where
  type Rs StartGateway = StartGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.StartGateway")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGatewayResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was restarted.
--
-- /See:/ 'mkStartGatewayResponse' smart constructor.
data StartGatewayResponse = StartGatewayResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartGatewayResponse' value with any optional fields omitted.
mkStartGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartGatewayResponse
mkStartGatewayResponse responseStatus =
  StartGatewayResponse' {gatewayARN = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrrsGatewayARN :: Lens.Lens' StartGatewayResponse (Core.Maybe Types.GatewayARN)
sgrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED sgrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrrsResponseStatus :: Lens.Lens' StartGatewayResponse Core.Int
sgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
