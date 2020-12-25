{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job.
module Network.AWS.IoT.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cJobId,
    cComment,
    cForce,
    cReasonCode,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    cjrfrsDescription,
    cjrfrsJobArn,
    cjrfrsJobId,
    cjrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelJob' smart constructor.
data CancelJob = CancelJob'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Types.JobId,
    -- | An optional comment string describing why the job was canceled.
    comment :: Core.Maybe Types.Comment,
    -- | (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ .
    --
    -- Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
    force :: Core.Maybe Core.Bool,
    -- | (Optional)A reason code string that explains why the job was canceled.
    reasonCode :: Core.Maybe Types.ReasonCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJob' value with any optional fields omitted.
mkCancelJob ::
  -- | 'jobId'
  Types.JobId ->
  CancelJob
mkCancelJob jobId =
  CancelJob'
    { jobId,
      comment = Core.Nothing,
      force = Core.Nothing,
      reasonCode = Core.Nothing
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJobId :: Lens.Lens' CancelJob Types.JobId
cJobId = Lens.field @"jobId"
{-# DEPRECATED cJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | An optional comment string describing why the job was canceled.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' CancelJob (Core.Maybe Types.Comment)
cComment = Lens.field @"comment"
{-# DEPRECATED cComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ .
--
-- Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cForce :: Lens.Lens' CancelJob (Core.Maybe Core.Bool)
cForce = Lens.field @"force"
{-# DEPRECATED cForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | (Optional)A reason code string that explains why the job was canceled.
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReasonCode :: Lens.Lens' CancelJob (Core.Maybe Types.ReasonCode)
cReasonCode = Lens.field @"reasonCode"
{-# DEPRECATED cReasonCode "Use generic-lens or generic-optics with 'reasonCode' instead." #-}

instance Core.FromJSON CancelJob where
  toJSON CancelJob {..} =
    Core.object
      ( Core.catMaybes
          [ ("comment" Core..=) Core.<$> comment,
            ("reasonCode" Core..=) Core.<$> reasonCode
          ]
      )

instance Core.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/jobs/" Core.<> (Core.toText jobId) Core.<> ("/cancel")),
        Core._rqQuery = Core.toQueryValue "force" Core.<$> force,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJobResponse'
            Core.<$> (x Core..:? "description")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "jobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { -- | A short text description of the job.
    description :: Core.Maybe Types.JobDescription,
    -- | The job ARN.
    jobArn :: Core.Maybe Types.JobArn,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Types.JobId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJobResponse' value with any optional fields omitted.
mkCancelJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelJobResponse
mkCancelJobResponse responseStatus =
  CancelJobResponse'
    { description = Core.Nothing,
      jobArn = Core.Nothing,
      jobId = Core.Nothing,
      responseStatus
    }

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsDescription :: Lens.Lens' CancelJobResponse (Core.Maybe Types.JobDescription)
cjrfrsDescription = Lens.field @"description"
{-# DEPRECATED cjrfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsJobArn :: Lens.Lens' CancelJobResponse (Core.Maybe Types.JobArn)
cjrfrsJobArn = Lens.field @"jobArn"
{-# DEPRECATED cjrfrsJobArn "Use generic-lens or generic-optics with 'jobArn' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsJobId :: Lens.Lens' CancelJobResponse (Core.Maybe Types.JobId)
cjrfrsJobId = Lens.field @"jobId"
{-# DEPRECATED cjrfrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsResponseStatus :: Lens.Lens' CancelJobResponse Core.Int
cjrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cjrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
