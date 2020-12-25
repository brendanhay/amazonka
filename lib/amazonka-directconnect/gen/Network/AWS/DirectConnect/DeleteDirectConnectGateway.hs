{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Direct Connect gateway. You must first delete all virtual interfaces that are attached to the Direct Connect gateway and disassociate all virtual private gateways associated with the Direct Connect gateway.
module Network.AWS.DirectConnect.DeleteDirectConnectGateway
  ( -- * Creating a request
    DeleteDirectConnectGateway (..),
    mkDeleteDirectConnectGateway,

    -- ** Request lenses
    ddcgfDirectConnectGatewayId,

    -- * Destructuring the response
    DeleteDirectConnectGatewayResponse (..),
    mkDeleteDirectConnectGatewayResponse,

    -- ** Response lenses
    ddcgrfrsDirectConnectGateway,
    ddcgrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDirectConnectGateway' smart constructor.
newtype DeleteDirectConnectGateway = DeleteDirectConnectGateway'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Types.DirectConnectGatewayId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGateway' value with any optional fields omitted.
mkDeleteDirectConnectGateway ::
  -- | 'directConnectGatewayId'
  Types.DirectConnectGatewayId ->
  DeleteDirectConnectGateway
mkDeleteDirectConnectGateway directConnectGatewayId =
  DeleteDirectConnectGateway' {directConnectGatewayId}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgfDirectConnectGatewayId :: Lens.Lens' DeleteDirectConnectGateway Types.DirectConnectGatewayId
ddcgfDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# DEPRECATED ddcgfDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

instance Core.FromJSON DeleteDirectConnectGateway where
  toJSON DeleteDirectConnectGateway {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("directConnectGatewayId" Core..= directConnectGatewayId)
          ]
      )

instance Core.AWSRequest DeleteDirectConnectGateway where
  type
    Rs DeleteDirectConnectGateway =
      DeleteDirectConnectGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.DeleteDirectConnectGateway")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayResponse'
            Core.<$> (x Core..:? "directConnectGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDirectConnectGatewayResponse' smart constructor.
data DeleteDirectConnectGatewayResponse = DeleteDirectConnectGatewayResponse'
  { -- | The Direct Connect gateway.
    directConnectGateway :: Core.Maybe Types.DirectConnectGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDirectConnectGatewayResponse' value with any optional fields omitted.
mkDeleteDirectConnectGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDirectConnectGatewayResponse
mkDeleteDirectConnectGatewayResponse responseStatus =
  DeleteDirectConnectGatewayResponse'
    { directConnectGateway =
        Core.Nothing,
      responseStatus
    }

-- | The Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrfrsDirectConnectGateway :: Lens.Lens' DeleteDirectConnectGatewayResponse (Core.Maybe Types.DirectConnectGateway)
ddcgrfrsDirectConnectGateway = Lens.field @"directConnectGateway"
{-# DEPRECATED ddcgrfrsDirectConnectGateway "Use generic-lens or generic-optics with 'directConnectGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrfrsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayResponse Core.Int
ddcgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
