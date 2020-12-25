{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down a gateway. To specify which gateway to shut down, use the Amazon Resource Name (ARN) of the gateway in the body of your request.
--
-- The operation shuts down the gateway service component running in the gateway's virtual machine (VM) and not the host VM.
-- After the gateway is shutdown, you cannot call any other API except 'StartGateway' , 'DescribeGatewayInformation' , and 'ListGateways' . For more information, see 'ActivateGateway' . Your applications cannot read from or write to the gateway's storage volumes, and there are no snapshots taken.
-- If do not intend to use the gateway again, you must delete the gateway (using 'DeleteGateway' ) to no longer pay software charges associated with the gateway.
module Network.AWS.StorageGateway.ShutdownGateway
  ( -- * Creating a request
    ShutdownGateway (..),
    mkShutdownGateway,

    -- ** Request lenses
    sGatewayARN,

    -- * Destructuring the response
    ShutdownGatewayResponse (..),
    mkShutdownGatewayResponse,

    -- ** Response lenses
    srsGatewayARN,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway to shut down.
--
-- /See:/ 'mkShutdownGateway' smart constructor.
newtype ShutdownGateway = ShutdownGateway'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ShutdownGateway' value with any optional fields omitted.
mkShutdownGateway ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  ShutdownGateway
mkShutdownGateway gatewayARN = ShutdownGateway' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sGatewayARN :: Lens.Lens' ShutdownGateway Types.GatewayARN
sGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED sGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON ShutdownGateway where
  toJSON ShutdownGateway {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest ShutdownGateway where
  type Rs ShutdownGateway = ShutdownGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.ShutdownGateway")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ShutdownGatewayResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway that was shut down.
--
-- /See:/ 'mkShutdownGatewayResponse' smart constructor.
data ShutdownGatewayResponse = ShutdownGatewayResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShutdownGatewayResponse' value with any optional fields omitted.
mkShutdownGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ShutdownGatewayResponse
mkShutdownGatewayResponse responseStatus =
  ShutdownGatewayResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsGatewayARN :: Lens.Lens' ShutdownGatewayResponse (Core.Maybe Types.GatewayARN)
srsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED srsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' ShutdownGatewayResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
