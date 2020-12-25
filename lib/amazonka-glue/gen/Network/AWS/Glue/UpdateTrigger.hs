{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a trigger definition.
module Network.AWS.Glue.UpdateTrigger
  ( -- * Creating a request
    UpdateTrigger (..),
    mkUpdateTrigger,

    -- ** Request lenses
    utName,
    utTriggerUpdate,

    -- * Destructuring the response
    UpdateTriggerResponse (..),
    mkUpdateTriggerResponse,

    -- ** Response lenses
    utrfrsTrigger,
    utrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTrigger' smart constructor.
data UpdateTrigger = UpdateTrigger'
  { -- | The name of the trigger to update.
    name :: Types.NameString,
    -- | The new values with which to update the trigger.
    triggerUpdate :: Types.TriggerUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrigger' value with any optional fields omitted.
mkUpdateTrigger ::
  -- | 'name'
  Types.NameString ->
  -- | 'triggerUpdate'
  Types.TriggerUpdate ->
  UpdateTrigger
mkUpdateTrigger name triggerUpdate =
  UpdateTrigger' {name, triggerUpdate}

-- | The name of the trigger to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utName :: Lens.Lens' UpdateTrigger Types.NameString
utName = Lens.field @"name"
{-# DEPRECATED utName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new values with which to update the trigger.
--
-- /Note:/ Consider using 'triggerUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTriggerUpdate :: Lens.Lens' UpdateTrigger Types.TriggerUpdate
utTriggerUpdate = Lens.field @"triggerUpdate"
{-# DEPRECATED utTriggerUpdate "Use generic-lens or generic-optics with 'triggerUpdate' instead." #-}

instance Core.FromJSON UpdateTrigger where
  toJSON UpdateTrigger {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("TriggerUpdate" Core..= triggerUpdate)
          ]
      )

instance Core.AWSRequest UpdateTrigger where
  type Rs UpdateTrigger = UpdateTriggerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateTrigger")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTriggerResponse'
            Core.<$> (x Core..:? "Trigger") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTriggerResponse' smart constructor.
data UpdateTriggerResponse = UpdateTriggerResponse'
  { -- | The resulting trigger definition.
    trigger :: Core.Maybe Types.Trigger,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTriggerResponse' value with any optional fields omitted.
mkUpdateTriggerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTriggerResponse
mkUpdateTriggerResponse responseStatus =
  UpdateTriggerResponse' {trigger = Core.Nothing, responseStatus}

-- | The resulting trigger definition.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrfrsTrigger :: Lens.Lens' UpdateTriggerResponse (Core.Maybe Types.Trigger)
utrfrsTrigger = Lens.field @"trigger"
{-# DEPRECATED utrfrsTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrfrsResponseStatus :: Lens.Lens' UpdateTriggerResponse Core.Int
utrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
