{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchStopJobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops one or more job runs for a specified job definition.
module Network.AWS.Glue.BatchStopJobRun
    (
    -- * Creating a request
      BatchStopJobRun (..)
    , mkBatchStopJobRun
    -- ** Request lenses
    , bsjrJobName
    , bsjrJobRunIds

    -- * Destructuring the response
    , BatchStopJobRunResponse (..)
    , mkBatchStopJobRunResponse
    -- ** Response lenses
    , bsjrrrsErrors
    , bsjrrrsSuccessfulSubmissions
    , bsjrrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchStopJobRun' smart constructor.
data BatchStopJobRun = BatchStopJobRun'
  { jobName :: Types.JobName
    -- ^ The name of the job definition for which to stop job runs.
  , jobRunIds :: Core.NonEmpty Types.IdString
    -- ^ A list of the @JobRunIds@ that should be stopped for that job definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopJobRun' value with any optional fields omitted.
mkBatchStopJobRun
    :: Types.JobName -- ^ 'jobName'
    -> Core.NonEmpty Types.IdString -- ^ 'jobRunIds'
    -> BatchStopJobRun
mkBatchStopJobRun jobName jobRunIds
  = BatchStopJobRun'{jobName, jobRunIds}

-- | The name of the job definition for which to stop job runs.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrJobName :: Lens.Lens' BatchStopJobRun Types.JobName
bsjrJobName = Lens.field @"jobName"
{-# INLINEABLE bsjrJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | A list of the @JobRunIds@ that should be stopped for that job definition.
--
-- /Note:/ Consider using 'jobRunIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrJobRunIds :: Lens.Lens' BatchStopJobRun (Core.NonEmpty Types.IdString)
bsjrJobRunIds = Lens.field @"jobRunIds"
{-# INLINEABLE bsjrJobRunIds #-}
{-# DEPRECATED jobRunIds "Use generic-lens or generic-optics with 'jobRunIds' instead"  #-}

instance Core.ToQuery BatchStopJobRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchStopJobRun where
        toHeaders BatchStopJobRun{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchStopJobRun") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchStopJobRun where
        toJSON BatchStopJobRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobName" Core..= jobName),
                  Core.Just ("JobRunIds" Core..= jobRunIds)])

instance Core.AWSRequest BatchStopJobRun where
        type Rs BatchStopJobRun = BatchStopJobRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchStopJobRunResponse' Core.<$>
                   (x Core..:? "Errors") Core.<*> x Core..:? "SuccessfulSubmissions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchStopJobRunResponse' smart constructor.
data BatchStopJobRunResponse = BatchStopJobRunResponse'
  { errors :: Core.Maybe [Types.BatchStopJobRunError]
    -- ^ A list of the errors that were encountered in trying to stop @JobRuns@ , including the @JobRunId@ for which each error was encountered and details about the error.
  , successfulSubmissions :: Core.Maybe [Types.BatchStopJobRunSuccessfulSubmission]
    -- ^ A list of the JobRuns that were successfully submitted for stopping.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopJobRunResponse' value with any optional fields omitted.
mkBatchStopJobRunResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchStopJobRunResponse
mkBatchStopJobRunResponse responseStatus
  = BatchStopJobRunResponse'{errors = Core.Nothing,
                             successfulSubmissions = Core.Nothing, responseStatus}

-- | A list of the errors that were encountered in trying to stop @JobRuns@ , including the @JobRunId@ for which each error was encountered and details about the error.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrrsErrors :: Lens.Lens' BatchStopJobRunResponse (Core.Maybe [Types.BatchStopJobRunError])
bsjrrrsErrors = Lens.field @"errors"
{-# INLINEABLE bsjrrrsErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | A list of the JobRuns that were successfully submitted for stopping.
--
-- /Note:/ Consider using 'successfulSubmissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrrsSuccessfulSubmissions :: Lens.Lens' BatchStopJobRunResponse (Core.Maybe [Types.BatchStopJobRunSuccessfulSubmission])
bsjrrrsSuccessfulSubmissions = Lens.field @"successfulSubmissions"
{-# INLINEABLE bsjrrrsSuccessfulSubmissions #-}
{-# DEPRECATED successfulSubmissions "Use generic-lens or generic-optics with 'successfulSubmissions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrrsResponseStatus :: Lens.Lens' BatchStopJobRunResponse Core.Int
bsjrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bsjrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
