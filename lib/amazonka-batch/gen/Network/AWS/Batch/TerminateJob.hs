{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.TerminateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a job in a job queue. Jobs that are in the @STARTING@ or @RUNNING@ state are terminated, which causes them to transition to @FAILED@ . Jobs that have not progressed to the @STARTING@ state are cancelled.
module Network.AWS.Batch.TerminateJob
  ( -- * Creating a request
    TerminateJob (..),
    mkTerminateJob,

    -- ** Request lenses
    tjJobId,
    tjReason,

    -- * Destructuring the response
    TerminateJobResponse (..),
    mkTerminateJobResponse,

    -- ** Response lenses
    tjrrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTerminateJob' smart constructor.
data TerminateJob = TerminateJob'
  { -- | The AWS Batch job ID of the job to terminate.
    jobId :: Types.JobId,
    -- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
    reason :: Types.Reason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateJob' value with any optional fields omitted.
mkTerminateJob ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'reason'
  Types.Reason ->
  TerminateJob
mkTerminateJob jobId reason = TerminateJob' {jobId, reason}

-- | The AWS Batch job ID of the job to terminate.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjJobId :: Lens.Lens' TerminateJob Types.JobId
tjJobId = Lens.field @"jobId"
{-# DEPRECATED tjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjReason :: Lens.Lens' TerminateJob Types.Reason
tjReason = Lens.field @"reason"
{-# DEPRECATED tjReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON TerminateJob where
  toJSON TerminateJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("reason" Core..= reason)
          ]
      )

instance Core.AWSRequest TerminateJob where
  type Rs TerminateJob = TerminateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/terminatejob",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          TerminateJobResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTerminateJobResponse' smart constructor.
newtype TerminateJobResponse = TerminateJobResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateJobResponse' value with any optional fields omitted.
mkTerminateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TerminateJobResponse
mkTerminateJobResponse responseStatus =
  TerminateJobResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjrrsResponseStatus :: Lens.Lens' TerminateJobResponse Core.Int
tjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
