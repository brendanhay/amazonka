{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway group.
module Network.AWS.AlexaBusiness.GetGatewayGroup
  ( -- * Creating a request
    GetGatewayGroup (..),
    mkGetGatewayGroup,

    -- ** Request lenses
    gggGatewayGroupArn,

    -- * Destructuring the response
    GetGatewayGroupResponse (..),
    mkGetGatewayGroupResponse,

    -- ** Response lenses
    gggrrsGatewayGroup,
    gggrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGatewayGroup' smart constructor.
newtype GetGatewayGroup = GetGatewayGroup'
  { -- | The ARN of the gateway group to get.
    gatewayGroupArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayGroup' value with any optional fields omitted.
mkGetGatewayGroup ::
  -- | 'gatewayGroupArn'
  Types.Arn ->
  GetGatewayGroup
mkGetGatewayGroup gatewayGroupArn =
  GetGatewayGroup' {gatewayGroupArn}

-- | The ARN of the gateway group to get.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggGatewayGroupArn :: Lens.Lens' GetGatewayGroup Types.Arn
gggGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# DEPRECATED gggGatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead." #-}

instance Core.FromJSON GetGatewayGroup where
  toJSON GetGatewayGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayGroupArn" Core..= gatewayGroupArn)]
      )

instance Core.AWSRequest GetGatewayGroup where
  type Rs GetGatewayGroup = GetGatewayGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetGatewayGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayGroupResponse'
            Core.<$> (x Core..:? "GatewayGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGatewayGroupResponse' smart constructor.
data GetGatewayGroupResponse = GetGatewayGroupResponse'
  { gatewayGroup :: Core.Maybe Types.GatewayGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayGroupResponse' value with any optional fields omitted.
mkGetGatewayGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGatewayGroupResponse
mkGetGatewayGroupResponse responseStatus =
  GetGatewayGroupResponse'
    { gatewayGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggrrsGatewayGroup :: Lens.Lens' GetGatewayGroupResponse (Core.Maybe Types.GatewayGroup)
gggrrsGatewayGroup = Lens.field @"gatewayGroup"
{-# DEPRECATED gggrrsGatewayGroup "Use generic-lens or generic-optics with 'gatewayGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggrrsResponseStatus :: Lens.Lens' GetGatewayGroupResponse Core.Int
gggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
