{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway group with the specified details.
module Network.AWS.AlexaBusiness.CreateGatewayGroup
  ( -- * Creating a request
    CreateGatewayGroup (..),
    mkCreateGatewayGroup,

    -- ** Request lenses
    cggName,
    cggClientRequestToken,
    cggDescription,

    -- * Destructuring the response
    CreateGatewayGroupResponse (..),
    mkCreateGatewayGroupResponse,

    -- ** Response lenses
    cggrrsGatewayGroupArn,
    cggrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGatewayGroup' smart constructor.
data CreateGatewayGroup = CreateGatewayGroup'
  { -- | The name of the gateway group.
    name :: Types.Name,
    -- | A unique, user-specified identifier for the request that ensures idempotency.
    clientRequestToken :: Types.ClientRequestToken,
    -- | The description of the gateway group.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGatewayGroup' value with any optional fields omitted.
mkCreateGatewayGroup ::
  -- | 'name'
  Types.Name ->
  -- | 'clientRequestToken'
  Types.ClientRequestToken ->
  CreateGatewayGroup
mkCreateGatewayGroup name clientRequestToken =
  CreateGatewayGroup'
    { name,
      clientRequestToken,
      description = Core.Nothing
    }

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggName :: Lens.Lens' CreateGatewayGroup Types.Name
cggName = Lens.field @"name"
{-# DEPRECATED cggName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggClientRequestToken :: Lens.Lens' CreateGatewayGroup Types.ClientRequestToken
cggClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cggClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggDescription :: Lens.Lens' CreateGatewayGroup (Core.Maybe Types.Description)
cggDescription = Lens.field @"description"
{-# DEPRECATED cggDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreateGatewayGroup where
  toJSON CreateGatewayGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("ClientRequestToken" Core..= clientRequestToken),
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreateGatewayGroup where
  type Rs CreateGatewayGroup = CreateGatewayGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateGatewayGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGatewayGroupResponse'
            Core.<$> (x Core..:? "GatewayGroupArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGatewayGroupResponse' smart constructor.
data CreateGatewayGroupResponse = CreateGatewayGroupResponse'
  { -- | The ARN of the created gateway group.
    gatewayGroupArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGatewayGroupResponse' value with any optional fields omitted.
mkCreateGatewayGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGatewayGroupResponse
mkCreateGatewayGroupResponse responseStatus =
  CreateGatewayGroupResponse'
    { gatewayGroupArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the created gateway group.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggrrsGatewayGroupArn :: Lens.Lens' CreateGatewayGroupResponse (Core.Maybe Types.Arn)
cggrrsGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# DEPRECATED cggrrsGatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggrrsResponseStatus :: Lens.Lens' CreateGatewayGroupResponse Core.Int
cggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
