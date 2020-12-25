{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information on a job bookmark entry.
module Network.AWS.Glue.GetJobBookmark
  ( -- * Creating a request
    GetJobBookmark (..),
    mkGetJobBookmark,

    -- ** Request lenses
    gjbJobName,
    gjbRunId,

    -- * Destructuring the response
    GetJobBookmarkResponse (..),
    mkGetJobBookmarkResponse,

    -- ** Response lenses
    gjbrrsJobBookmarkEntry,
    gjbrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobBookmark' smart constructor.
data GetJobBookmark = GetJobBookmark'
  { -- | The name of the job in question.
    jobName :: Types.JobName,
    -- | The unique run identifier associated with this job run.
    runId :: Core.Maybe Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobBookmark' value with any optional fields omitted.
mkGetJobBookmark ::
  -- | 'jobName'
  Types.JobName ->
  GetJobBookmark
mkGetJobBookmark jobName =
  GetJobBookmark' {jobName, runId = Core.Nothing}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbJobName :: Lens.Lens' GetJobBookmark Types.JobName
gjbJobName = Lens.field @"jobName"
{-# DEPRECATED gjbJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The unique run identifier associated with this job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbRunId :: Lens.Lens' GetJobBookmark (Core.Maybe Types.RunId)
gjbRunId = Lens.field @"runId"
{-# DEPRECATED gjbRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Core.FromJSON GetJobBookmark where
  toJSON GetJobBookmark {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            ("RunId" Core..=) Core.<$> runId
          ]
      )

instance Core.AWSRequest GetJobBookmark where
  type Rs GetJobBookmark = GetJobBookmarkResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetJobBookmark")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobBookmarkResponse'
            Core.<$> (x Core..:? "JobBookmarkEntry")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetJobBookmarkResponse' smart constructor.
data GetJobBookmarkResponse = GetJobBookmarkResponse'
  { -- | A structure that defines a point that a job can resume processing.
    jobBookmarkEntry :: Core.Maybe Types.JobBookmarkEntry,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobBookmarkResponse' value with any optional fields omitted.
mkGetJobBookmarkResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetJobBookmarkResponse
mkGetJobBookmarkResponse responseStatus =
  GetJobBookmarkResponse'
    { jobBookmarkEntry = Core.Nothing,
      responseStatus
    }

-- | A structure that defines a point that a job can resume processing.
--
-- /Note:/ Consider using 'jobBookmarkEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbrrsJobBookmarkEntry :: Lens.Lens' GetJobBookmarkResponse (Core.Maybe Types.JobBookmarkEntry)
gjbrrsJobBookmarkEntry = Lens.field @"jobBookmarkEntry"
{-# DEPRECATED gjbrrsJobBookmarkEntry "Use generic-lens or generic-optics with 'jobBookmarkEntry' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjbrrsResponseStatus :: Lens.Lens' GetJobBookmarkResponse Core.Int
gjbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
