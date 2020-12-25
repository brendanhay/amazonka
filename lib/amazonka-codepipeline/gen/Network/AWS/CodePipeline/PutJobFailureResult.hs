{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutJobFailureResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job worker. Used for custom actions only.
module Network.AWS.CodePipeline.PutJobFailureResult
  ( -- * Creating a request
    PutJobFailureResult (..),
    mkPutJobFailureResult,

    -- ** Request lenses
    pjfrJobId,
    pjfrFailureDetails,

    -- * Destructuring the response
    PutJobFailureResultResponse (..),
    mkPutJobFailureResultResponse,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutJobFailureResult@ action.
--
-- /See:/ 'mkPutJobFailureResult' smart constructor.
data PutJobFailureResult = PutJobFailureResult'
  { -- | The unique system-generated ID of the job that failed. This is the same ID returned from @PollForJobs@ .
    jobId :: Types.JobId,
    -- | The details about the failure of a job.
    failureDetails :: Types.FailureDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutJobFailureResult' value with any optional fields omitted.
mkPutJobFailureResult ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'failureDetails'
  Types.FailureDetails ->
  PutJobFailureResult
mkPutJobFailureResult jobId failureDetails =
  PutJobFailureResult' {jobId, failureDetails}

-- | The unique system-generated ID of the job that failed. This is the same ID returned from @PollForJobs@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjfrJobId :: Lens.Lens' PutJobFailureResult Types.JobId
pjfrJobId = Lens.field @"jobId"
{-# DEPRECATED pjfrJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The details about the failure of a job.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjfrFailureDetails :: Lens.Lens' PutJobFailureResult Types.FailureDetails
pjfrFailureDetails = Lens.field @"failureDetails"
{-# DEPRECATED pjfrFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

instance Core.FromJSON PutJobFailureResult where
  toJSON PutJobFailureResult {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("failureDetails" Core..= failureDetails)
          ]
      )

instance Core.AWSRequest PutJobFailureResult where
  type Rs PutJobFailureResult = PutJobFailureResultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.PutJobFailureResult")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutJobFailureResultResponse'

-- | /See:/ 'mkPutJobFailureResultResponse' smart constructor.
data PutJobFailureResultResponse = PutJobFailureResultResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutJobFailureResultResponse' value with any optional fields omitted.
mkPutJobFailureResultResponse ::
  PutJobFailureResultResponse
mkPutJobFailureResultResponse = PutJobFailureResultResponse'
