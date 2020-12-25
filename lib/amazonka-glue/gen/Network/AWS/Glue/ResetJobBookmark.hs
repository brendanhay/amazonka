{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ResetJobBookmark
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a bookmark entry.
module Network.AWS.Glue.ResetJobBookmark
  ( -- * Creating a request
    ResetJobBookmark (..),
    mkResetJobBookmark,

    -- ** Request lenses
    rjbJobName,
    rjbRunId,

    -- * Destructuring the response
    ResetJobBookmarkResponse (..),
    mkResetJobBookmarkResponse,

    -- ** Response lenses
    rjbrrsJobBookmarkEntry,
    rjbrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetJobBookmark' smart constructor.
data ResetJobBookmark = ResetJobBookmark'
  { -- | The name of the job in question.
    jobName :: Types.JobName,
    -- | The unique run identifier associated with this job run.
    runId :: Core.Maybe Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetJobBookmark' value with any optional fields omitted.
mkResetJobBookmark ::
  -- | 'jobName'
  Types.JobName ->
  ResetJobBookmark
mkResetJobBookmark jobName =
  ResetJobBookmark' {jobName, runId = Core.Nothing}

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbJobName :: Lens.Lens' ResetJobBookmark Types.JobName
rjbJobName = Lens.field @"jobName"
{-# DEPRECATED rjbJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The unique run identifier associated with this job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbRunId :: Lens.Lens' ResetJobBookmark (Core.Maybe Types.RunId)
rjbRunId = Lens.field @"runId"
{-# DEPRECATED rjbRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Core.FromJSON ResetJobBookmark where
  toJSON ResetJobBookmark {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            ("RunId" Core..=) Core.<$> runId
          ]
      )

instance Core.AWSRequest ResetJobBookmark where
  type Rs ResetJobBookmark = ResetJobBookmarkResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.ResetJobBookmark")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetJobBookmarkResponse'
            Core.<$> (x Core..:? "JobBookmarkEntry")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResetJobBookmarkResponse' smart constructor.
data ResetJobBookmarkResponse = ResetJobBookmarkResponse'
  { -- | The reset bookmark entry.
    jobBookmarkEntry :: Core.Maybe Types.JobBookmarkEntry,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetJobBookmarkResponse' value with any optional fields omitted.
mkResetJobBookmarkResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResetJobBookmarkResponse
mkResetJobBookmarkResponse responseStatus =
  ResetJobBookmarkResponse'
    { jobBookmarkEntry = Core.Nothing,
      responseStatus
    }

-- | The reset bookmark entry.
--
-- /Note:/ Consider using 'jobBookmarkEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbrrsJobBookmarkEntry :: Lens.Lens' ResetJobBookmarkResponse (Core.Maybe Types.JobBookmarkEntry)
rjbrrsJobBookmarkEntry = Lens.field @"jobBookmarkEntry"
{-# DEPRECATED rjbrrsJobBookmarkEntry "Use generic-lens or generic-optics with 'jobBookmarkEntry' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rjbrrsResponseStatus :: Lens.Lens' ResetJobBookmarkResponse Core.Int
rjbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rjbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
