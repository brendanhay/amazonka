{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DisableGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a tape gateway when the gateway is no longer functioning. For example, if your gateway VM is damaged, you can disable the gateway so you can recover virtual tapes.
--
-- Use this operation for a tape gateway that is not reachable or not functioning. This operation is only supported in the tape gateway type.
-- /Important:/ After a gateway is disabled, it cannot be enabled.
module Network.AWS.StorageGateway.DisableGateway
  ( -- * Creating a request
    DisableGateway (..),
    mkDisableGateway,

    -- ** Request lenses
    dGatewayARN,

    -- * Destructuring the response
    DisableGatewayResponse (..),
    mkDisableGatewayResponse,

    -- ** Response lenses
    dgrfrsGatewayARN,
    dgrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | DisableGatewayInput
--
-- /See:/ 'mkDisableGateway' smart constructor.
newtype DisableGateway = DisableGateway'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableGateway' value with any optional fields omitted.
mkDisableGateway ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DisableGateway
mkDisableGateway gatewayARN = DisableGateway' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGatewayARN :: Lens.Lens' DisableGateway Types.GatewayARN
dGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DisableGateway where
  toJSON DisableGateway {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DisableGateway where
  type Rs DisableGateway = DisableGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.DisableGateway")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableGatewayResponse'
            Core.<$> (x Core..:? "GatewayARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | DisableGatewayOutput
--
-- /See:/ 'mkDisableGatewayResponse' smart constructor.
data DisableGatewayResponse = DisableGatewayResponse'
  { -- | The unique Amazon Resource Name (ARN) of the disabled gateway.
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableGatewayResponse' value with any optional fields omitted.
mkDisableGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableGatewayResponse
mkDisableGatewayResponse responseStatus =
  DisableGatewayResponse'
    { gatewayARN = Core.Nothing,
      responseStatus
    }

-- | The unique Amazon Resource Name (ARN) of the disabled gateway.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsGatewayARN :: Lens.Lens' DisableGatewayResponse (Core.Maybe Types.GatewayARN)
dgrfrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dgrfrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsResponseStatus :: Lens.Lens' DisableGatewayResponse Core.Int
dgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
