{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified job definition. If the job definition is not found, no exception is thrown.
module Network.AWS.Glue.DeleteJob
  ( -- * Creating a request
    DeleteJob (..),
    mkDeleteJob,

    -- ** Request lenses
    djJobName,

    -- * Destructuring the response
    DeleteJobResponse (..),
    mkDeleteJobResponse,

    -- ** Response lenses
    djrrsJobName,
    djrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteJob' smart constructor.
newtype DeleteJob = DeleteJob'
  { -- | The name of the job definition to delete.
    jobName :: Types.JobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJob' value with any optional fields omitted.
mkDeleteJob ::
  -- | 'jobName'
  Types.JobName ->
  DeleteJob
mkDeleteJob jobName = DeleteJob' {jobName}

-- | The name of the job definition to delete.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobName :: Lens.Lens' DeleteJob Types.JobName
djJobName = Lens.field @"jobName"
{-# DEPRECATED djJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Core.FromJSON DeleteJob where
  toJSON DeleteJob {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobName" Core..= jobName)])

instance Core.AWSRequest DeleteJob where
  type Rs DeleteJob = DeleteJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteJobResponse'
            Core.<$> (x Core..:? "JobName") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  { -- | The name of the job definition that was deleted.
    jobName :: Core.Maybe Types.NameString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJobResponse' value with any optional fields omitted.
mkDeleteJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteJobResponse
mkDeleteJobResponse responseStatus =
  DeleteJobResponse' {jobName = Core.Nothing, responseStatus}

-- | The name of the job definition that was deleted.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsJobName :: Lens.Lens' DeleteJobResponse (Core.Maybe Types.NameString)
djrrsJobName = Lens.field @"jobName"
{-# DEPRECATED djrrsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsResponseStatus :: Lens.Lens' DeleteJobResponse Core.Int
djrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED djrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
