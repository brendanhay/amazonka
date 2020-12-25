{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing job definition.
module Network.AWS.Glue.UpdateJob
  ( -- * Creating a request
    UpdateJob (..),
    mkUpdateJob,

    -- ** Request lenses
    ujJobName,
    ujJobUpdate,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,

    -- ** Response lenses
    ujrrsJobName,
    ujrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | The name of the job definition to update.
    jobName :: Types.JobName,
    -- | Specifies the values with which to update the job definition.
    jobUpdate :: Types.JobUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJob' value with any optional fields omitted.
mkUpdateJob ::
  -- | 'jobName'
  Types.JobName ->
  -- | 'jobUpdate'
  Types.JobUpdate ->
  UpdateJob
mkUpdateJob jobName jobUpdate = UpdateJob' {jobName, jobUpdate}

-- | The name of the job definition to update.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobName :: Lens.Lens' UpdateJob Types.JobName
ujJobName = Lens.field @"jobName"
{-# DEPRECATED ujJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Specifies the values with which to update the job definition.
--
-- /Note:/ Consider using 'jobUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobUpdate :: Lens.Lens' UpdateJob Types.JobUpdate
ujJobUpdate = Lens.field @"jobUpdate"
{-# DEPRECATED ujJobUpdate "Use generic-lens or generic-optics with 'jobUpdate' instead." #-}

instance Core.FromJSON UpdateJob where
  toJSON UpdateJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            Core.Just ("JobUpdate" Core..= jobUpdate)
          ]
      )

instance Core.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobResponse'
            Core.<$> (x Core..:? "JobName") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { -- | Returns the name of the updated job definition.
    jobName :: Core.Maybe Types.NameString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobResponse' value with any optional fields omitted.
mkUpdateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateJobResponse
mkUpdateJobResponse responseStatus =
  UpdateJobResponse' {jobName = Core.Nothing, responseStatus}

-- | Returns the name of the updated job definition.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsJobName :: Lens.Lens' UpdateJobResponse (Core.Maybe Types.NameString)
ujrrsJobName = Lens.field @"jobName"
{-# DEPRECATED ujrrsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsResponseStatus :: Lens.Lens' UpdateJobResponse Core.Int
ujrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ujrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
