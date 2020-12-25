{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given job run.
module Network.AWS.Glue.GetJobRun
  ( -- * Creating a request
    GetJobRun (..),
    mkGetJobRun,

    -- ** Request lenses
    gJobName,
    gRunId,
    gPredecessorsIncluded,

    -- * Destructuring the response
    GetJobRunResponse (..),
    mkGetJobRunResponse,

    -- ** Response lenses
    gjrrfrsJobRun,
    gjrrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetJobRun' smart constructor.
data GetJobRun = GetJobRun'
  { -- | Name of the job definition being run.
    jobName :: Types.JobName,
    -- | The ID of the job run.
    runId :: Types.RunId,
    -- | True if a list of predecessor runs should be returned.
    predecessorsIncluded :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobRun' value with any optional fields omitted.
mkGetJobRun ::
  -- | 'jobName'
  Types.JobName ->
  -- | 'runId'
  Types.RunId ->
  GetJobRun
mkGetJobRun jobName runId =
  GetJobRun' {jobName, runId, predecessorsIncluded = Core.Nothing}

-- | Name of the job definition being run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gJobName :: Lens.Lens' GetJobRun Types.JobName
gJobName = Lens.field @"jobName"
{-# DEPRECATED gJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The ID of the job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gRunId :: Lens.Lens' GetJobRun Types.RunId
gRunId = Lens.field @"runId"
{-# DEPRECATED gRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | True if a list of predecessor runs should be returned.
--
-- /Note:/ Consider using 'predecessorsIncluded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPredecessorsIncluded :: Lens.Lens' GetJobRun (Core.Maybe Core.Bool)
gPredecessorsIncluded = Lens.field @"predecessorsIncluded"
{-# DEPRECATED gPredecessorsIncluded "Use generic-lens or generic-optics with 'predecessorsIncluded' instead." #-}

instance Core.FromJSON GetJobRun where
  toJSON GetJobRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            Core.Just ("RunId" Core..= runId),
            ("PredecessorsIncluded" Core..=) Core.<$> predecessorsIncluded
          ]
      )

instance Core.AWSRequest GetJobRun where
  type Rs GetJobRun = GetJobRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetJobRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobRunResponse'
            Core.<$> (x Core..:? "JobRun") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetJobRunResponse' smart constructor.
data GetJobRunResponse = GetJobRunResponse'
  { -- | The requested job-run metadata.
    jobRun :: Core.Maybe Types.JobRun,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetJobRunResponse' value with any optional fields omitted.
mkGetJobRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetJobRunResponse
mkGetJobRunResponse responseStatus =
  GetJobRunResponse' {jobRun = Core.Nothing, responseStatus}

-- | The requested job-run metadata.
--
-- /Note:/ Consider using 'jobRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrfrsJobRun :: Lens.Lens' GetJobRunResponse (Core.Maybe Types.JobRun)
gjrrfrsJobRun = Lens.field @"jobRun"
{-# DEPRECATED gjrrfrsJobRun "Use generic-lens or generic-optics with 'jobRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrfrsResponseStatus :: Lens.Lens' GetJobRunResponse Core.Int
gjrrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjrrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
