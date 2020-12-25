{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.AcknowledgeJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified job and whether that job has been received by the job worker. Used for custom actions only.
module Network.AWS.CodePipeline.AcknowledgeJob
  ( -- * Creating a request
    AcknowledgeJob (..),
    mkAcknowledgeJob,

    -- ** Request lenses
    ajJobId,
    ajNonce,

    -- * Destructuring the response
    AcknowledgeJobResponse (..),
    mkAcknowledgeJobResponse,

    -- ** Response lenses
    ajrrsStatus,
    ajrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AcknowledgeJob action.
--
-- /See:/ 'mkAcknowledgeJob' smart constructor.
data AcknowledgeJob = AcknowledgeJob'
  { -- | The unique system-generated ID of the job for which you want to confirm receipt.
    jobId :: Types.JobId,
    -- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response of the 'PollForJobs' request that returned this job.
    nonce :: Types.Nonce
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcknowledgeJob' value with any optional fields omitted.
mkAcknowledgeJob ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'nonce'
  Types.Nonce ->
  AcknowledgeJob
mkAcknowledgeJob jobId nonce = AcknowledgeJob' {jobId, nonce}

-- | The unique system-generated ID of the job for which you want to confirm receipt.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajJobId :: Lens.Lens' AcknowledgeJob Types.JobId
ajJobId = Lens.field @"jobId"
{-# DEPRECATED ajJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response of the 'PollForJobs' request that returned this job.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajNonce :: Lens.Lens' AcknowledgeJob Types.Nonce
ajNonce = Lens.field @"nonce"
{-# DEPRECATED ajNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

instance Core.FromJSON AcknowledgeJob where
  toJSON AcknowledgeJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("nonce" Core..= nonce)
          ]
      )

instance Core.AWSRequest AcknowledgeJob where
  type Rs AcknowledgeJob = AcknowledgeJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.AcknowledgeJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AcknowledgeJobResponse'
            Core.<$> (x Core..:? "status") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an AcknowledgeJob action.
--
-- /See:/ 'mkAcknowledgeJobResponse' smart constructor.
data AcknowledgeJobResponse = AcknowledgeJobResponse'
  { -- | Whether the job worker has received the specified job.
    status :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcknowledgeJobResponse' value with any optional fields omitted.
mkAcknowledgeJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcknowledgeJobResponse
mkAcknowledgeJobResponse responseStatus =
  AcknowledgeJobResponse' {status = Core.Nothing, responseStatus}

-- | Whether the job worker has received the specified job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajrrsStatus :: Lens.Lens' AcknowledgeJobResponse (Core.Maybe Types.JobStatus)
ajrrsStatus = Lens.field @"status"
{-# DEPRECATED ajrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajrrsResponseStatus :: Lens.Lens' AcknowledgeJobResponse Core.Int
ajrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ajrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
