{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a third party job as returned to the pipeline by a job worker. Used for partner actions only.
module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
  ( -- * Creating a request
    PutThirdPartyJobFailureResult (..),
    mkPutThirdPartyJobFailureResult,

    -- ** Request lenses
    ptpjfrJobId,
    ptpjfrClientToken,
    ptpjfrFailureDetails,

    -- * Destructuring the response
    PutThirdPartyJobFailureResultResponse (..),
    mkPutThirdPartyJobFailureResultResponse,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutThirdPartyJobFailureResult@ action.
--
-- /See:/ 'mkPutThirdPartyJobFailureResult' smart constructor.
data PutThirdPartyJobFailureResult = PutThirdPartyJobFailureResult'
  { -- | The ID of the job that failed. This is the same ID returned from @PollForThirdPartyJobs@ .
    jobId :: Types.JobId,
    -- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
    clientToken :: Types.ClientToken,
    -- | Represents information about failure details.
    failureDetails :: Types.FailureDetails
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutThirdPartyJobFailureResult' value with any optional fields omitted.
mkPutThirdPartyJobFailureResult ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'clientToken'
  Types.ClientToken ->
  -- | 'failureDetails'
  Types.FailureDetails ->
  PutThirdPartyJobFailureResult
mkPutThirdPartyJobFailureResult jobId clientToken failureDetails =
  PutThirdPartyJobFailureResult'
    { jobId,
      clientToken,
      failureDetails
    }

-- | The ID of the job that failed. This is the same ID returned from @PollForThirdPartyJobs@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrJobId :: Lens.Lens' PutThirdPartyJobFailureResult Types.JobId
ptpjfrJobId = Lens.field @"jobId"
{-# DEPRECATED ptpjfrJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrClientToken :: Lens.Lens' PutThirdPartyJobFailureResult Types.ClientToken
ptpjfrClientToken = Lens.field @"clientToken"
{-# DEPRECATED ptpjfrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Represents information about failure details.
--
-- /Note:/ Consider using 'failureDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpjfrFailureDetails :: Lens.Lens' PutThirdPartyJobFailureResult Types.FailureDetails
ptpjfrFailureDetails = Lens.field @"failureDetails"
{-# DEPRECATED ptpjfrFailureDetails "Use generic-lens or generic-optics with 'failureDetails' instead." #-}

instance Core.FromJSON PutThirdPartyJobFailureResult where
  toJSON PutThirdPartyJobFailureResult {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("clientToken" Core..= clientToken),
            Core.Just ("failureDetails" Core..= failureDetails)
          ]
      )

instance Core.AWSRequest PutThirdPartyJobFailureResult where
  type
    Rs PutThirdPartyJobFailureResult =
      PutThirdPartyJobFailureResultResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodePipeline_20150709.PutThirdPartyJobFailureResult"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull PutThirdPartyJobFailureResultResponse'

-- | /See:/ 'mkPutThirdPartyJobFailureResultResponse' smart constructor.
data PutThirdPartyJobFailureResultResponse = PutThirdPartyJobFailureResultResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutThirdPartyJobFailureResultResponse' value with any optional fields omitted.
mkPutThirdPartyJobFailureResultResponse ::
  PutThirdPartyJobFailureResultResponse
mkPutThirdPartyJobFailureResultResponse =
  PutThirdPartyJobFailureResultResponse'
