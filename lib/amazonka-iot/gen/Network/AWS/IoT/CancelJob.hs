{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CancelJob (..)
    , mkCancelJob
    -- ** Request lenses
    , cJobId
    , cComment
    , cForce
    , cReasonCode

    -- * Destructuring the response
    , CancelJobResponse (..)
    , mkCancelJobResponse
    -- ** Response lenses
    , cjrfrsDescription
    , cjrfrsJobArn
    , cjrfrsJobId
    , cjrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCancelJob' smart constructor.
data CancelJob = CancelJob'
  { jobId :: Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  , comment :: Core.Maybe Types.Comment
    -- ^ An optional comment string describing why the job was canceled.
  , force :: Core.Maybe Core.Bool
    -- ^ (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ .
--
-- Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
  , reasonCode :: Core.Maybe Types.ReasonCode
    -- ^ (Optional)A reason code string that explains why the job was canceled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJob' value with any optional fields omitted.
mkCancelJob
    :: Types.JobId -- ^ 'jobId'
    -> CancelJob
mkCancelJob jobId
  = CancelJob'{jobId, comment = Core.Nothing, force = Core.Nothing,
               reasonCode = Core.Nothing}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJobId :: Lens.Lens' CancelJob Types.JobId
cJobId = Lens.field @"jobId"
{-# INLINEABLE cJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | An optional comment string describing why the job was canceled.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cComment :: Lens.Lens' CancelJob (Core.Maybe Types.Comment)
cComment = Lens.field @"comment"
{-# INLINEABLE cComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

-- | (Optional) If @true@ job executions with status "IN_PROGRESS" and "QUEUED" are canceled, otherwise only job executions with status "QUEUED" are canceled. The default is @false@ .
--
-- Canceling a job which is "IN_PROGRESS", will cause a device which is executing the job to be unable to update the job execution status. Use caution and ensure that each device executing a job which is canceled is able to recover to a valid state.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cForce :: Lens.Lens' CancelJob (Core.Maybe Core.Bool)
cForce = Lens.field @"force"
{-# INLINEABLE cForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

-- | (Optional)A reason code string that explains why the job was canceled.
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReasonCode :: Lens.Lens' CancelJob (Core.Maybe Types.ReasonCode)
cReasonCode = Lens.field @"reasonCode"
{-# INLINEABLE cReasonCode #-}
{-# DEPRECATED reasonCode "Use generic-lens or generic-optics with 'reasonCode' instead"  #-}

instance Core.ToQuery CancelJob where
        toQuery CancelJob{..}
          = Core.maybe Core.mempty (Core.toQueryPair "force") force

instance Core.ToHeaders CancelJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CancelJob where
        toJSON CancelJob{..}
          = Core.object
              (Core.catMaybes
                 [("comment" Core..=) Core.<$> comment,
                  ("reasonCode" Core..=) Core.<$> reasonCode])

instance Core.AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/jobs/" Core.<> Core.toText jobId Core.<> "/cancel",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CancelJobResponse' Core.<$>
                   (x Core..:? "description") Core.<*> x Core..:? "jobArn" Core.<*>
                     x Core..:? "jobId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { description :: Core.Maybe Types.JobDescription
    -- ^ A short text description of the job.
  , jobArn :: Core.Maybe Types.JobArn
    -- ^ The job ARN.
  , jobId :: Core.Maybe Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelJobResponse' value with any optional fields omitted.
mkCancelJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelJobResponse
mkCancelJobResponse responseStatus
  = CancelJobResponse'{description = Core.Nothing,
                       jobArn = Core.Nothing, jobId = Core.Nothing, responseStatus}

-- | A short text description of the job.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsDescription :: Lens.Lens' CancelJobResponse (Core.Maybe Types.JobDescription)
cjrfrsDescription = Lens.field @"description"
{-# INLINEABLE cjrfrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsJobArn :: Lens.Lens' CancelJobResponse (Core.Maybe Types.JobArn)
cjrfrsJobArn = Lens.field @"jobArn"
{-# INLINEABLE cjrfrsJobArn #-}
{-# DEPRECATED jobArn "Use generic-lens or generic-optics with 'jobArn' instead"  #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsJobId :: Lens.Lens' CancelJobResponse (Core.Maybe Types.JobId)
cjrfrsJobId = Lens.field @"jobId"
{-# INLINEABLE cjrfrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrfrsResponseStatus :: Lens.Lens' CancelJobResponse Core.Int
cjrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
