{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing trigger. See <https://docs.aws.amazon.com/glue/latest/dg/trigger-job.html Triggering Jobs> for information about how different types of trigger are started.
module Network.AWS.Glue.StartTrigger
  ( -- * Creating a request
    StartTrigger (..),
    mkStartTrigger,

    -- ** Request lenses
    stfName,

    -- * Destructuring the response
    StartTriggerResponse (..),
    mkStartTriggerResponse,

    -- ** Response lenses
    strgrsName,
    strgrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartTrigger' smart constructor.
newtype StartTrigger = StartTrigger'
  { -- | The name of the trigger to start.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartTrigger' value with any optional fields omitted.
mkStartTrigger ::
  -- | 'name'
  Types.Name ->
  StartTrigger
mkStartTrigger name = StartTrigger' {name}

-- | The name of the trigger to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfName :: Lens.Lens' StartTrigger Types.Name
stfName = Lens.field @"name"
{-# DEPRECATED stfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON StartTrigger where
  toJSON StartTrigger {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartTrigger where
  type Rs StartTrigger = StartTriggerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartTrigger")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTriggerResponse'
            Core.<$> (x Core..:? "Name") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartTriggerResponse' smart constructor.
data StartTriggerResponse = StartTriggerResponse'
  { -- | The name of the trigger that was started.
    name :: Core.Maybe Types.NameString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTriggerResponse' value with any optional fields omitted.
mkStartTriggerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartTriggerResponse
mkStartTriggerResponse responseStatus =
  StartTriggerResponse' {name = Core.Nothing, responseStatus}

-- | The name of the trigger that was started.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strgrsName :: Lens.Lens' StartTriggerResponse (Core.Maybe Types.NameString)
strgrsName = Lens.field @"name"
{-# DEPRECATED strgrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strgrsResponseStatus :: Lens.Lens' StartTriggerResponse Core.Int
strgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED strgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
