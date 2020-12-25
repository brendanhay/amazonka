{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway group. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGatewayGroup
  ( -- * Creating a request
    UpdateGatewayGroup (..),
    mkUpdateGatewayGroup,

    -- ** Request lenses
    uggGatewayGroupArn,
    uggDescription,
    uggName,

    -- * Destructuring the response
    UpdateGatewayGroupResponse (..),
    mkUpdateGatewayGroupResponse,

    -- ** Response lenses
    uggrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGatewayGroup' smart constructor.
data UpdateGatewayGroup = UpdateGatewayGroup'
  { -- | The ARN of the gateway group to update.
    gatewayGroupArn :: Types.GatewayGroupArn,
    -- | The updated description of the gateway group.
    description :: Core.Maybe Types.Description,
    -- | The updated name of the gateway group.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayGroup' value with any optional fields omitted.
mkUpdateGatewayGroup ::
  -- | 'gatewayGroupArn'
  Types.GatewayGroupArn ->
  UpdateGatewayGroup
mkUpdateGatewayGroup gatewayGroupArn =
  UpdateGatewayGroup'
    { gatewayGroupArn,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the gateway group to update.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggGatewayGroupArn :: Lens.Lens' UpdateGatewayGroup Types.GatewayGroupArn
uggGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# DEPRECATED uggGatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead." #-}

-- | The updated description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggDescription :: Lens.Lens' UpdateGatewayGroup (Core.Maybe Types.Description)
uggDescription = Lens.field @"description"
{-# DEPRECATED uggDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggName :: Lens.Lens' UpdateGatewayGroup (Core.Maybe Types.Name)
uggName = Lens.field @"name"
{-# DEPRECATED uggName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateGatewayGroup where
  toJSON UpdateGatewayGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayGroupArn" Core..= gatewayGroupArn),
            ("Description" Core..=) Core.<$> description,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest UpdateGatewayGroup where
  type Rs UpdateGatewayGroup = UpdateGatewayGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateGatewayGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateGatewayGroupResponse' smart constructor.
newtype UpdateGatewayGroupResponse = UpdateGatewayGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayGroupResponse' value with any optional fields omitted.
mkUpdateGatewayGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateGatewayGroupResponse
mkUpdateGatewayGroupResponse responseStatus =
  UpdateGatewayGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggrrsResponseStatus :: Lens.Lens' UpdateGatewayGroupResponse Core.Int
uggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
