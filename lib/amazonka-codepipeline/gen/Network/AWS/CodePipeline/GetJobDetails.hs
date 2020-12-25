{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a job. Used for custom actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetJobDetails
  ( -- * Creating a request
    GetJobDetails (..),
    mkGetJobDetails,

    -- ** Request lenses
    gjdJobId,

    -- * Destructuring the response
    GetJobDetailsResponse (..),
    mkGetJobDetailsResponse,

    -- ** Response lenses
    gjdrrsJobDetails,
    gjdrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetJobDetails@ action.
--
-- /See:/ 'mkGetJobDetails' smart constructor.
newtype GetJobDetails = GetJobDetails'
  { -- | The unique system-generated ID for the job.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobDetails' value with any optional fields omitted.
mkGetJobDetails ::
  -- | 'jobId'
  Types.JobId ->
  GetJobDetails
mkGetJobDetails jobId = GetJobDetails' {jobId}

-- | The unique system-generated ID for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobId :: Lens.Lens' GetJobDetails Types.JobId
gjdJobId = Lens.field @"jobId"
{-# DEPRECATED gjdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON GetJobDetails where
  toJSON GetJobDetails {..} =
    Core.object (Core.catMaybes [Core.Just ("jobId" Core..= jobId)])

instance Core.AWSRequest GetJobDetails where
  type Rs GetJobDetails = GetJobDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.GetJobDetails")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobDetailsResponse'
            Core.<$> (x Core..:? "jobDetails") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetJobDetails@ action.
--
-- /See:/ 'mkGetJobDetailsResponse' smart constructor.
data GetJobDetailsResponse = GetJobDetailsResponse'
  { -- | The details of the job.
    jobDetails :: Core.Maybe Types.JobDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetJobDetailsResponse' value with any optional fields omitted.
mkGetJobDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetJobDetailsResponse
mkGetJobDetailsResponse responseStatus =
  GetJobDetailsResponse' {jobDetails = Core.Nothing, responseStatus}

-- | The details of the job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrrsJobDetails :: Lens.Lens' GetJobDetailsResponse (Core.Maybe Types.JobDetails)
gjdrrsJobDetails = Lens.field @"jobDetails"
{-# DEPRECATED gjdrrsJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrrsResponseStatus :: Lens.Lens' GetJobDetailsResponse Core.Int
gjdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gjdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
