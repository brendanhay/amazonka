{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway group.
module Network.AWS.AlexaBusiness.DeleteGatewayGroup
  ( -- * Creating a request
    DeleteGatewayGroup (..),
    mkDeleteGatewayGroup,

    -- ** Request lenses
    dggGatewayGroupArn,

    -- * Destructuring the response
    DeleteGatewayGroupResponse (..),
    mkDeleteGatewayGroupResponse,

    -- ** Response lenses
    dggrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGatewayGroup' smart constructor.
newtype DeleteGatewayGroup = DeleteGatewayGroup'
  { -- | The ARN of the gateway group to delete.
    gatewayGroupArn :: Types.GatewayGroupArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGatewayGroup' value with any optional fields omitted.
mkDeleteGatewayGroup ::
  -- | 'gatewayGroupArn'
  Types.GatewayGroupArn ->
  DeleteGatewayGroup
mkDeleteGatewayGroup gatewayGroupArn =
  DeleteGatewayGroup' {gatewayGroupArn}

-- | The ARN of the gateway group to delete.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dggGatewayGroupArn :: Lens.Lens' DeleteGatewayGroup Types.GatewayGroupArn
dggGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# DEPRECATED dggGatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead." #-}

instance Core.FromJSON DeleteGatewayGroup where
  toJSON DeleteGatewayGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayGroupArn" Core..= gatewayGroupArn)]
      )

instance Core.AWSRequest DeleteGatewayGroup where
  type Rs DeleteGatewayGroup = DeleteGatewayGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteGatewayGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGatewayGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteGatewayGroupResponse' smart constructor.
newtype DeleteGatewayGroupResponse = DeleteGatewayGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGatewayGroupResponse' value with any optional fields omitted.
mkDeleteGatewayGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteGatewayGroupResponse
mkDeleteGatewayGroupResponse responseStatus =
  DeleteGatewayGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dggrrsResponseStatus :: Lens.Lens' DeleteGatewayGroupResponse Core.Int
dggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
