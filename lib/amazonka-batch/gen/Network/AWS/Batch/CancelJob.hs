{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job in an AWS Batch job queue. Jobs that are in the @SUBMITTED@ , @PENDING@ , or @RUNNABLE@ state are cancelled. Jobs that have progressed to @STARTING@ or @RUNNING@ are not cancelled (but the API operation still succeeds, even if no job is cancelled); these jobs must be terminated with the 'TerminateJob' operation.
module Network.AWS.Batch.CancelJob
    (
    -- * Creating a request
      CancelJob (..)
    , mkCancelJob
    -- ** Request lenses
    , cjJobId
    , cjReason

    -- * Destructuring the response
    , CancelJobResponse (..)
    , mkCancelJobResponse
    -- ** Response lenses
    , cjrrsResponseStatus
    ) where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelJob' smart constructor.
data CancelJob = CancelJob'
  { jobId :: Core.Text
    -- ^ The AWS Batch job ID of the job to cancel.
  , reason :: Core.Text
    -- ^ A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJob' value with any optional fields omitted.
mkCancelJob
    :: Core.Text -- ^ 'jobId'
    -> Core.Text -- ^ 'reason'
    -> CancelJob
mkCancelJob jobId reason = CancelJob'{jobId, reason}

-- | The AWS Batch job ID of the job to cancel.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobId :: Lens.Lens' CancelJob Core.Text
cjJobId = Lens.field @"jobId"
{-# INLINEABLE cjJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjReason :: Lens.Lens' CancelJob Core.Text
cjReason = Lens.field @"reason"
{-# INLINEABLE cjReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.ToQuery CancelJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelJob where
        toHeaders CancelJob{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelJob where
        toJSON CancelJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("jobId" Core..= jobId),
                  Core.Just ("reason" Core..= reason)])

instance Core.AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/canceljob",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelJobResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJobResponse' value with any optional fields omitted.
mkCancelJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelJobResponse
mkCancelJobResponse responseStatus
  = CancelJobResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CancelJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
