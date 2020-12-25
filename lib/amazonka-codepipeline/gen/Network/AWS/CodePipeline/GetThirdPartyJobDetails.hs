{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetThirdPartyJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the details of a job for a third party action. Used for partner actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.GetThirdPartyJobDetails
  ( -- * Creating a request
    GetThirdPartyJobDetails (..),
    mkGetThirdPartyJobDetails,

    -- ** Request lenses
    gtpjdJobId,
    gtpjdClientToken,

    -- * Destructuring the response
    GetThirdPartyJobDetailsResponse (..),
    mkGetThirdPartyJobDetailsResponse,

    -- ** Response lenses
    gtpjdrrsJobDetails,
    gtpjdrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'mkGetThirdPartyJobDetails' smart constructor.
data GetThirdPartyJobDetails = GetThirdPartyJobDetails'
  { -- | The unique system-generated ID used for identifying the job.
    jobId :: Types.ThirdPartyJobId,
    -- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
    clientToken :: Types.ClientToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThirdPartyJobDetails' value with any optional fields omitted.
mkGetThirdPartyJobDetails ::
  -- | 'jobId'
  Types.ThirdPartyJobId ->
  -- | 'clientToken'
  Types.ClientToken ->
  GetThirdPartyJobDetails
mkGetThirdPartyJobDetails jobId clientToken =
  GetThirdPartyJobDetails' {jobId, clientToken}

-- | The unique system-generated ID used for identifying the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdJobId :: Lens.Lens' GetThirdPartyJobDetails Types.ThirdPartyJobId
gtpjdJobId = Lens.field @"jobId"
{-# DEPRECATED gtpjdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdClientToken :: Lens.Lens' GetThirdPartyJobDetails Types.ClientToken
gtpjdClientToken = Lens.field @"clientToken"
{-# DEPRECATED gtpjdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Core.FromJSON GetThirdPartyJobDetails where
  toJSON GetThirdPartyJobDetails {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.AWSRequest GetThirdPartyJobDetails where
  type Rs GetThirdPartyJobDetails = GetThirdPartyJobDetailsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.GetThirdPartyJobDetails")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThirdPartyJobDetailsResponse'
            Core.<$> (x Core..:? "jobDetails") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetThirdPartyJobDetails@ action.
--
-- /See:/ 'mkGetThirdPartyJobDetailsResponse' smart constructor.
data GetThirdPartyJobDetailsResponse = GetThirdPartyJobDetailsResponse'
  { -- | The details of the job, including any protected values defined for the job.
    jobDetails :: Core.Maybe Types.ThirdPartyJobDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetThirdPartyJobDetailsResponse' value with any optional fields omitted.
mkGetThirdPartyJobDetailsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetThirdPartyJobDetailsResponse
mkGetThirdPartyJobDetailsResponse responseStatus =
  GetThirdPartyJobDetailsResponse'
    { jobDetails = Core.Nothing,
      responseStatus
    }

-- | The details of the job, including any protected values defined for the job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdrrsJobDetails :: Lens.Lens' GetThirdPartyJobDetailsResponse (Core.Maybe Types.ThirdPartyJobDetails)
gtpjdrrsJobDetails = Lens.field @"jobDetails"
{-# DEPRECATED gtpjdrrsJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtpjdrrsResponseStatus :: Lens.Lens' GetThirdPartyJobDetailsResponse Core.Int
gtpjdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtpjdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
