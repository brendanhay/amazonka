{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PollForThirdPartyJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether there are any third party jobs for a job worker to act on. Used for partner actions only.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts.
module Network.AWS.CodePipeline.PollForThirdPartyJobs
  ( -- * Creating a request
    PollForThirdPartyJobs (..),
    mkPollForThirdPartyJobs,

    -- ** Request lenses
    pftpjActionTypeId,
    pftpjMaxBatchSize,

    -- * Destructuring the response
    PollForThirdPartyJobsResponse (..),
    mkPollForThirdPartyJobsResponse,

    -- ** Response lenses
    pftpjrrsJobs,
    pftpjrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'mkPollForThirdPartyJobs' smart constructor.
data PollForThirdPartyJobs = PollForThirdPartyJobs'
  { -- | Represents information about an action type.
    actionTypeId :: Types.ActionTypeId,
    -- | The maximum number of jobs to return in a poll for jobs call.
    maxBatchSize :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForThirdPartyJobs' value with any optional fields omitted.
mkPollForThirdPartyJobs ::
  -- | 'actionTypeId'
  Types.ActionTypeId ->
  PollForThirdPartyJobs
mkPollForThirdPartyJobs actionTypeId =
  PollForThirdPartyJobs' {actionTypeId, maxBatchSize = Core.Nothing}

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjActionTypeId :: Lens.Lens' PollForThirdPartyJobs Types.ActionTypeId
pftpjActionTypeId = Lens.field @"actionTypeId"
{-# DEPRECATED pftpjActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | The maximum number of jobs to return in a poll for jobs call.
--
-- /Note:/ Consider using 'maxBatchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjMaxBatchSize :: Lens.Lens' PollForThirdPartyJobs (Core.Maybe Core.Natural)
pftpjMaxBatchSize = Lens.field @"maxBatchSize"
{-# DEPRECATED pftpjMaxBatchSize "Use generic-lens or generic-optics with 'maxBatchSize' instead." #-}

instance Core.FromJSON PollForThirdPartyJobs where
  toJSON PollForThirdPartyJobs {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("actionTypeId" Core..= actionTypeId),
            ("maxBatchSize" Core..=) Core.<$> maxBatchSize
          ]
      )

instance Core.AWSRequest PollForThirdPartyJobs where
  type Rs PollForThirdPartyJobs = PollForThirdPartyJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.PollForThirdPartyJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForThirdPartyJobsResponse'
            Core.<$> (x Core..:? "jobs") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'mkPollForThirdPartyJobsResponse' smart constructor.
data PollForThirdPartyJobsResponse = PollForThirdPartyJobsResponse'
  { -- | Information about the jobs to take action on.
    jobs :: Core.Maybe [Types.ThirdPartyJob],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForThirdPartyJobsResponse' value with any optional fields omitted.
mkPollForThirdPartyJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PollForThirdPartyJobsResponse
mkPollForThirdPartyJobsResponse responseStatus =
  PollForThirdPartyJobsResponse'
    { jobs = Core.Nothing,
      responseStatus
    }

-- | Information about the jobs to take action on.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjrrsJobs :: Lens.Lens' PollForThirdPartyJobsResponse (Core.Maybe [Types.ThirdPartyJob])
pftpjrrsJobs = Lens.field @"jobs"
{-# DEPRECATED pftpjrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjrrsResponseStatus :: Lens.Lens' PollForThirdPartyJobsResponse Core.Int
pftpjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pftpjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
