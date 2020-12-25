{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscription definition.
module Network.AWS.Greengrass.UpdateSubscriptionDefinition
  ( -- * Creating a request
    UpdateSubscriptionDefinition (..),
    mkUpdateSubscriptionDefinition,

    -- ** Request lenses
    usdSubscriptionDefinitionId,
    usdName,

    -- * Destructuring the response
    UpdateSubscriptionDefinitionResponse (..),
    mkUpdateSubscriptionDefinitionResponse,

    -- ** Response lenses
    usdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriptionDefinition' value with any optional fields omitted.
mkUpdateSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  UpdateSubscriptionDefinition
mkUpdateSubscriptionDefinition subscriptionDefinitionId =
  UpdateSubscriptionDefinition'
    { subscriptionDefinitionId,
      name = Core.Nothing
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdSubscriptionDefinitionId :: Lens.Lens' UpdateSubscriptionDefinition Core.Text
usdSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# DEPRECATED usdSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdName :: Lens.Lens' UpdateSubscriptionDefinition (Core.Maybe Core.Text)
usdName = Lens.field @"name"
{-# DEPRECATED usdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON UpdateSubscriptionDefinition where
  toJSON UpdateSubscriptionDefinition {..} =
    Core.object (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateSubscriptionDefinition where
  type
    Rs UpdateSubscriptionDefinition =
      UpdateSubscriptionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/subscriptions/"
                Core.<> (Core.toText subscriptionDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSubscriptionDefinitionResponse' smart constructor.
newtype UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubscriptionDefinitionResponse' value with any optional fields omitted.
mkUpdateSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSubscriptionDefinitionResponse
mkUpdateSubscriptionDefinitionResponse responseStatus =
  UpdateSubscriptionDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrrsResponseStatus :: Lens.Lens' UpdateSubscriptionDefinitionResponse Core.Int
usdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
