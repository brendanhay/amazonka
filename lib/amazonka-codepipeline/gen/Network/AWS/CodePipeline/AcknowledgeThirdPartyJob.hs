{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a job worker has received the specified job. Used for partner actions only.
module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
  ( -- * Creating a request
    AcknowledgeThirdPartyJob (..),
    mkAcknowledgeThirdPartyJob,

    -- ** Request lenses
    atpjJobId,
    atpjNonce,
    atpjClientToken,

    -- * Destructuring the response
    AcknowledgeThirdPartyJobResponse (..),
    mkAcknowledgeThirdPartyJobResponse,

    -- ** Response lenses
    atpjrrsStatus,
    atpjrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AcknowledgeThirdPartyJob action.
--
-- /See:/ 'mkAcknowledgeThirdPartyJob' smart constructor.
data AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJob'
  { -- | The unique system-generated ID of the job.
    jobId :: Types.JobId,
    -- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
    nonce :: Types.Nonce,
    -- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
    clientToken :: Types.ClientToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcknowledgeThirdPartyJob' value with any optional fields omitted.
mkAcknowledgeThirdPartyJob ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'nonce'
  Types.Nonce ->
  -- | 'clientToken'
  Types.ClientToken ->
  AcknowledgeThirdPartyJob
mkAcknowledgeThirdPartyJob jobId nonce clientToken =
  AcknowledgeThirdPartyJob' {jobId, nonce, clientToken}

-- | The unique system-generated ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjJobId :: Lens.Lens' AcknowledgeThirdPartyJob Types.JobId
atpjJobId = Lens.field @"jobId"
{-# DEPRECATED atpjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjNonce :: Lens.Lens' AcknowledgeThirdPartyJob Types.Nonce
atpjNonce = Lens.field @"nonce"
{-# DEPRECATED atpjNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjClientToken :: Lens.Lens' AcknowledgeThirdPartyJob Types.ClientToken
atpjClientToken = Lens.field @"clientToken"
{-# DEPRECATED atpjClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Core.FromJSON AcknowledgeThirdPartyJob where
  toJSON AcknowledgeThirdPartyJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("nonce" Core..= nonce),
            Core.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.AWSRequest AcknowledgeThirdPartyJob where
  type Rs AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.AcknowledgeThirdPartyJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AcknowledgeThirdPartyJobResponse'
            Core.<$> (x Core..:? "status") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an AcknowledgeThirdPartyJob action.
--
-- /See:/ 'mkAcknowledgeThirdPartyJobResponse' smart constructor.
data AcknowledgeThirdPartyJobResponse = AcknowledgeThirdPartyJobResponse'
  { -- | The status information for the third party job, if any.
    status :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcknowledgeThirdPartyJobResponse' value with any optional fields omitted.
mkAcknowledgeThirdPartyJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AcknowledgeThirdPartyJobResponse
mkAcknowledgeThirdPartyJobResponse responseStatus =
  AcknowledgeThirdPartyJobResponse'
    { status = Core.Nothing,
      responseStatus
    }

-- | The status information for the third party job, if any.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjrrsStatus :: Lens.Lens' AcknowledgeThirdPartyJobResponse (Core.Maybe Types.JobStatus)
atpjrrsStatus = Lens.field @"status"
{-# DEPRECATED atpjrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjrrsResponseStatus :: Lens.Lens' AcknowledgeThirdPartyJobResponse Core.Int
atpjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atpjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
