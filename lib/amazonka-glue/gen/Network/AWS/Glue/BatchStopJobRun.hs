{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    BatchStopJobRun (..),
    mkBatchStopJobRun,

    -- ** Request lenses
    bsjrJobName,
    bsjrJobRunIds,

    -- * Destructuring the response
    BatchStopJobRunResponse (..),
    mkBatchStopJobRunResponse,

    -- ** Response lenses
    bsjrrrsErrors,
    bsjrrrsSuccessfulSubmissions,
    bsjrrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchStopJobRun' smart constructor.
data BatchStopJobRun = BatchStopJobRun'
  { -- | The name of the job definition for which to stop job runs.
    jobName :: Types.JobName,
    -- | A list of the @JobRunIds@ that should be stopped for that job definition.
    jobRunIds :: Core.NonEmpty Types.IdString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopJobRun' value with any optional fields omitted.
mkBatchStopJobRun ::
  -- | 'jobName'
  Types.JobName ->
  -- | 'jobRunIds'
  Core.NonEmpty Types.IdString ->
  BatchStopJobRun
mkBatchStopJobRun jobName jobRunIds =
  BatchStopJobRun' {jobName, jobRunIds}

-- | The name of the job definition for which to stop job runs.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrJobName :: Lens.Lens' BatchStopJobRun Types.JobName
bsjrJobName = Lens.field @"jobName"
{-# DEPRECATED bsjrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | A list of the @JobRunIds@ that should be stopped for that job definition.
--
-- /Note:/ Consider using 'jobRunIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrJobRunIds :: Lens.Lens' BatchStopJobRun (Core.NonEmpty Types.IdString)
bsjrJobRunIds = Lens.field @"jobRunIds"
{-# DEPRECATED bsjrJobRunIds "Use generic-lens or generic-optics with 'jobRunIds' instead." #-}

instance Core.FromJSON BatchStopJobRun where
  toJSON BatchStopJobRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobName" Core..= jobName),
            Core.Just ("JobRunIds" Core..= jobRunIds)
          ]
      )

instance Core.AWSRequest BatchStopJobRun where
  type Rs BatchStopJobRun = BatchStopJobRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchStopJobRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStopJobRunResponse'
            Core.<$> (x Core..:? "Errors")
            Core.<*> (x Core..:? "SuccessfulSubmissions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchStopJobRunResponse' smart constructor.
data BatchStopJobRunResponse = BatchStopJobRunResponse'
  { -- | A list of the errors that were encountered in trying to stop @JobRuns@ , including the @JobRunId@ for which each error was encountered and details about the error.
    errors :: Core.Maybe [Types.BatchStopJobRunError],
    -- | A list of the JobRuns that were successfully submitted for stopping.
    successfulSubmissions :: Core.Maybe [Types.BatchStopJobRunSuccessfulSubmission],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopJobRunResponse' value with any optional fields omitted.
mkBatchStopJobRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchStopJobRunResponse
mkBatchStopJobRunResponse responseStatus =
  BatchStopJobRunResponse'
    { errors = Core.Nothing,
      successfulSubmissions = Core.Nothing,
      responseStatus
    }

-- | A list of the errors that were encountered in trying to stop @JobRuns@ , including the @JobRunId@ for which each error was encountered and details about the error.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrrsErrors :: Lens.Lens' BatchStopJobRunResponse (Core.Maybe [Types.BatchStopJobRunError])
bsjrrrsErrors = Lens.field @"errors"
{-# DEPRECATED bsjrrrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | A list of the JobRuns that were successfully submitted for stopping.
--
-- /Note:/ Consider using 'successfulSubmissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrrsSuccessfulSubmissions :: Lens.Lens' BatchStopJobRunResponse (Core.Maybe [Types.BatchStopJobRunSuccessfulSubmission])
bsjrrrsSuccessfulSubmissions = Lens.field @"successfulSubmissions"
{-# DEPRECATED bsjrrrsSuccessfulSubmissions "Use generic-lens or generic-optics with 'successfulSubmissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsjrrrsResponseStatus :: Lens.Lens' BatchStopJobRunResponse Core.Int
bsjrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bsjrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
