{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StopThingRegistrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bulk thing provisioning task.
module Network.AWS.IoT.StopThingRegistrationTask
  ( -- * Creating a request
    StopThingRegistrationTask (..),
    mkStopThingRegistrationTask,

    -- ** Request lenses
    strtTaskId,

    -- * Destructuring the response
    StopThingRegistrationTaskResponse (..),
    mkStopThingRegistrationTaskResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopThingRegistrationTask' smart constructor.
newtype StopThingRegistrationTask = StopThingRegistrationTask'
  { -- | The bulk thing provisioning task ID.
    taskId :: Types.TaskId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopThingRegistrationTask' value with any optional fields omitted.
mkStopThingRegistrationTask ::
  -- | 'taskId'
  Types.TaskId ->
  StopThingRegistrationTask
mkStopThingRegistrationTask taskId =
  StopThingRegistrationTask' {taskId}

-- | The bulk thing provisioning task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtTaskId :: Lens.Lens' StopThingRegistrationTask Types.TaskId
strtTaskId = Lens.field @"taskId"
{-# DEPRECATED strtTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Core.FromJSON StopThingRegistrationTask where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest StopThingRegistrationTask where
  type
    Rs StopThingRegistrationTask =
      StopThingRegistrationTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/thing-registration-tasks/" Core.<> (Core.toText taskId)
                Core.<> ("/cancel")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopThingRegistrationTaskResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopThingRegistrationTaskResponse' smart constructor.
newtype StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopThingRegistrationTaskResponse' value with any optional fields omitted.
mkStopThingRegistrationTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopThingRegistrationTaskResponse
mkStopThingRegistrationTaskResponse responseStatus =
  StopThingRegistrationTaskResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopThingRegistrationTaskResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
