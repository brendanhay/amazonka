{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified transit gateway. When you modify a transit gateway, the modified options are applied to new transit gateway attachments only. Your existing transit gateway attachments are not modified.
module Network.AWS.EC2.ModifyTransitGateway
  ( -- * Creating a request
    ModifyTransitGateway (..),
    mkModifyTransitGateway,

    -- ** Request lenses
    mtgTransitGatewayId,
    mtgDescription,
    mtgDryRun,
    mtgOptions,

    -- * Destructuring the response
    ModifyTransitGatewayResponse (..),
    mkModifyTransitGatewayResponse,

    -- ** Response lenses
    mtgrrsTransitGateway,
    mtgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTransitGateway' smart constructor.
data ModifyTransitGateway = ModifyTransitGateway'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Types.TransitGatewayId,
    -- | The description for the transit gateway.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The options to modify.
    options :: Core.Maybe Types.ModifyTransitGatewayOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGateway' value with any optional fields omitted.
mkModifyTransitGateway ::
  -- | 'transitGatewayId'
  Types.TransitGatewayId ->
  ModifyTransitGateway
mkModifyTransitGateway transitGatewayId =
  ModifyTransitGateway'
    { transitGatewayId,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      options = Core.Nothing
    }

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgTransitGatewayId :: Lens.Lens' ModifyTransitGateway Types.TransitGatewayId
mtgTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED mtgTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The description for the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgDescription :: Lens.Lens' ModifyTransitGateway (Core.Maybe Types.String)
mtgDescription = Lens.field @"description"
{-# DEPRECATED mtgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgDryRun :: Lens.Lens' ModifyTransitGateway (Core.Maybe Core.Bool)
mtgDryRun = Lens.field @"dryRun"
{-# DEPRECATED mtgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The options to modify.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgOptions :: Lens.Lens' ModifyTransitGateway (Core.Maybe Types.ModifyTransitGatewayOptions)
mtgOptions = Lens.field @"options"
{-# DEPRECATED mtgOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Core.AWSRequest ModifyTransitGateway where
  type Rs ModifyTransitGateway = ModifyTransitGatewayResponse
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
            ( Core.pure ("Action", "ModifyTransitGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TransitGatewayId" transitGatewayId)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Options" Core.<$> options)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayResponse'
            Core.<$> (x Core..@? "transitGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyTransitGatewayResponse' smart constructor.
data ModifyTransitGatewayResponse = ModifyTransitGatewayResponse'
  { transitGateway :: Core.Maybe Types.TransitGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyTransitGatewayResponse' value with any optional fields omitted.
mkModifyTransitGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyTransitGatewayResponse
mkModifyTransitGatewayResponse responseStatus =
  ModifyTransitGatewayResponse'
    { transitGateway = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsTransitGateway :: Lens.Lens' ModifyTransitGatewayResponse (Core.Maybe Types.TransitGateway)
mtgrrsTransitGateway = Lens.field @"transitGateway"
{-# DEPRECATED mtgrrsTransitGateway "Use generic-lens or generic-optics with 'transitGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgrrsResponseStatus :: Lens.Lens' ModifyTransitGatewayResponse Core.Int
mtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
