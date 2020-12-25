{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGateway
  ( -- * Creating a request
    UpdateGateway (..),
    mkUpdateGateway,

    -- ** Request lenses
    ugGatewayArn,
    ugDescription,
    ugName,
    ugSoftwareVersion,

    -- * Destructuring the response
    UpdateGatewayResponse (..),
    mkUpdateGatewayResponse,

    -- ** Response lenses
    ugrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { -- | The ARN of the gateway to update.
    gatewayArn :: Types.Arn,
    -- | The updated description of the gateway.
    description :: Core.Maybe Types.Description,
    -- | The updated name of the gateway.
    name :: Core.Maybe Types.Name,
    -- | The updated software version of the gateway. The gateway automatically updates its software version during normal operation.
    softwareVersion :: Core.Maybe Types.SoftwareVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGateway' value with any optional fields omitted.
mkUpdateGateway ::
  -- | 'gatewayArn'
  Types.Arn ->
  UpdateGateway
mkUpdateGateway gatewayArn =
  UpdateGateway'
    { gatewayArn,
      description = Core.Nothing,
      name = Core.Nothing,
      softwareVersion = Core.Nothing
    }

-- | The ARN of the gateway to update.
--
-- /Note:/ Consider using 'gatewayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGatewayArn :: Lens.Lens' UpdateGateway Types.Arn
ugGatewayArn = Lens.field @"gatewayArn"
{-# DEPRECATED ugGatewayArn "Use generic-lens or generic-optics with 'gatewayArn' instead." #-}

-- | The updated description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGateway (Core.Maybe Types.Description)
ugDescription = Lens.field @"description"
{-# DEPRECATED ugDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugName :: Lens.Lens' UpdateGateway (Core.Maybe Types.Name)
ugName = Lens.field @"name"
{-# DEPRECATED ugName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugSoftwareVersion :: Lens.Lens' UpdateGateway (Core.Maybe Types.SoftwareVersion)
ugSoftwareVersion = Lens.field @"softwareVersion"
{-# DEPRECATED ugSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

instance Core.FromJSON UpdateGateway where
  toJSON UpdateGateway {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayArn" Core..= gatewayArn),
            ("Description" Core..=) Core.<$> description,
            ("Name" Core..=) Core.<$> name,
            ("SoftwareVersion" Core..=) Core.<$> softwareVersion
          ]
      )

instance Core.AWSRequest UpdateGateway where
  type Rs UpdateGateway = UpdateGatewayResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateGateway")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGatewayResponse' smart constructor.
newtype UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayResponse' value with any optional fields omitted.
mkUpdateGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGatewayResponse
mkUpdateGatewayResponse responseStatus =
  UpdateGatewayResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsResponseStatus :: Lens.Lens' UpdateGatewayResponse Core.Int
ugrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ugrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
