{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the Direct Connect gateway association.
--
-- Add or remove prefixes from the association.
module Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
  ( -- * Creating a request
    UpdateDirectConnectGatewayAssociation (..),
    mkUpdateDirectConnectGatewayAssociation,

    -- ** Request lenses
    udcgaAddAllowedPrefixesToDirectConnectGateway,
    udcgaAssociationId,
    udcgaRemoveAllowedPrefixesToDirectConnectGateway,

    -- * Destructuring the response
    UpdateDirectConnectGatewayAssociationResponse (..),
    mkUpdateDirectConnectGatewayAssociationResponse,

    -- ** Response lenses
    udcgarrsDirectConnectGatewayAssociation,
    udcgarrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDirectConnectGatewayAssociation' smart constructor.
data UpdateDirectConnectGatewayAssociation = UpdateDirectConnectGatewayAssociation'
  { -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    addAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway association.
    associationId :: Core.Maybe Types.AssociationId,
    -- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
    removeAllowedPrefixesToDirectConnectGateway :: Core.Maybe [Types.RouteFilterPrefix]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDirectConnectGatewayAssociation' value with any optional fields omitted.
mkUpdateDirectConnectGatewayAssociation ::
  UpdateDirectConnectGatewayAssociation
mkUpdateDirectConnectGatewayAssociation =
  UpdateDirectConnectGatewayAssociation'
    { addAllowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      associationId = Core.Nothing,
      removeAllowedPrefixesToDirectConnectGateway =
        Core.Nothing
    }

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe [Types.RouteFilterPrefix])
udcgaAddAllowedPrefixesToDirectConnectGateway = Lens.field @"addAllowedPrefixesToDirectConnectGateway"
{-# DEPRECATED udcgaAddAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaAssociationId :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe Types.AssociationId)
udcgaAssociationId = Lens.field @"associationId"
{-# DEPRECATED udcgaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'removeAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaRemoveAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe [Types.RouteFilterPrefix])
udcgaRemoveAllowedPrefixesToDirectConnectGateway = Lens.field @"removeAllowedPrefixesToDirectConnectGateway"
{-# DEPRECATED udcgaRemoveAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'removeAllowedPrefixesToDirectConnectGateway' instead." #-}

instance Core.FromJSON UpdateDirectConnectGatewayAssociation where
  toJSON UpdateDirectConnectGatewayAssociation {..} =
    Core.object
      ( Core.catMaybes
          [ ("addAllowedPrefixesToDirectConnectGateway" Core..=)
              Core.<$> addAllowedPrefixesToDirectConnectGateway,
            ("associationId" Core..=) Core.<$> associationId,
            ("removeAllowedPrefixesToDirectConnectGateway" Core..=)
              Core.<$> removeAllowedPrefixesToDirectConnectGateway
          ]
      )

instance Core.AWSRequest UpdateDirectConnectGatewayAssociation where
  type
    Rs UpdateDirectConnectGatewayAssociation =
      UpdateDirectConnectGatewayAssociationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "OvertureService.UpdateDirectConnectGatewayAssociation"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDirectConnectGatewayAssociationResponse'
            Core.<$> (x Core..:? "directConnectGatewayAssociation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDirectConnectGatewayAssociationResponse' smart constructor.
data UpdateDirectConnectGatewayAssociationResponse = UpdateDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Core.Maybe Types.DirectConnectGatewayAssociation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDirectConnectGatewayAssociationResponse' value with any optional fields omitted.
mkUpdateDirectConnectGatewayAssociationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDirectConnectGatewayAssociationResponse
mkUpdateDirectConnectGatewayAssociationResponse responseStatus =
  UpdateDirectConnectGatewayAssociationResponse'
    { directConnectGatewayAssociation =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgarrsDirectConnectGatewayAssociation :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse (Core.Maybe Types.DirectConnectGatewayAssociation)
udcgarrsDirectConnectGatewayAssociation = Lens.field @"directConnectGatewayAssociation"
{-# DEPRECATED udcgarrsDirectConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgarrsResponseStatus :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse Core.Int
udcgarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udcgarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
