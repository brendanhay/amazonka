{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      TerminateJob (..)
    , mkTerminateJob
    -- ** Request lenses
    , tjJobId
    , tjReason

    -- * Destructuring the response
    , TerminateJobResponse (..)
    , mkTerminateJobResponse
    -- ** Response lenses
    , tjrrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTerminateJob' smart constructor.
data TerminateJob = TerminateJob'
  { jobId :: Core.Text
    -- ^ The AWS Batch job ID of the job to terminate.
  , reason :: Core.Text
    -- ^ A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateJob' value with any optional fields omitted.
mkTerminateJob
    :: Core.Text -- ^ 'jobId'
    -> Core.Text -- ^ 'reason'
    -> TerminateJob
mkTerminateJob jobId reason = TerminateJob'{jobId, reason}

-- | The AWS Batch job ID of the job to terminate.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjJobId :: Lens.Lens' TerminateJob Core.Text
tjJobId = Lens.field @"jobId"
{-# INLINEABLE tjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjReason :: Lens.Lens' TerminateJob Core.Text
tjReason = Lens.field @"reason"
{-# INLINEABLE tjReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery TerminateJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TerminateJob where
        toHeaders TerminateJob{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TerminateJob where
        toJSON TerminateJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobId" Core..= jobId),
                  Core.Just ("reason" Core..= reason)])

instance Core.AWSRequest TerminateJob where
        type Rs TerminateJob = TerminateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/terminatejob",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 TerminateJobResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTerminateJobResponse' smart constructor.
newtype TerminateJobResponse = TerminateJobResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateJobResponse' value with any optional fields omitted.
mkTerminateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TerminateJobResponse
mkTerminateJobResponse responseStatus
  = TerminateJobResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjrrsResponseStatus :: Lens.Lens' TerminateJobResponse Core.Int
tjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
