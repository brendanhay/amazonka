{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
module Network.AWS.Glue.GetTrigger
  ( -- * Creating a request
    GetTrigger (..),
    mkGetTrigger,

    -- ** Request lenses
    gtName,

    -- * Destructuring the response
    GetTriggerResponse (..),
    mkGetTriggerResponse,

    -- ** Response lenses
    gtrfrsTrigger,
    gtrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTrigger' smart constructor.
newtype GetTrigger = GetTrigger'
  { -- | The name of the trigger to retrieve.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrigger' value with any optional fields omitted.
mkGetTrigger ::
  -- | 'name'
  Types.Name ->
  GetTrigger
mkGetTrigger name = GetTrigger' {name}

-- | The name of the trigger to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTrigger Types.Name
gtName = Lens.field @"name"
{-# DEPRECATED gtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetTrigger where
  toJSON GetTrigger {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetTrigger where
  type Rs GetTrigger = GetTriggerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetTrigger")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTriggerResponse'
            Core.<$> (x Core..:? "Trigger") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
  { -- | The requested trigger definition.
    trigger :: Core.Maybe Types.Trigger,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTriggerResponse' value with any optional fields omitted.
mkGetTriggerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTriggerResponse
mkGetTriggerResponse responseStatus =
  GetTriggerResponse' {trigger = Core.Nothing, responseStatus}

-- | The requested trigger definition.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrfrsTrigger :: Lens.Lens' GetTriggerResponse (Core.Maybe Types.Trigger)
gtrfrsTrigger = Lens.field @"trigger"
{-# DEPRECATED gtrfrsTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrfrsResponseStatus :: Lens.Lens' GetTriggerResponse Core.Int
gtrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
