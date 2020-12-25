{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PollForJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about any jobs for AWS CodePipeline to act on. @PollForJobs@ is valid only for action types with "Custom" in the owner field. If the action type contains "AWS" or "ThirdParty" in the owner field, the @PollForJobs@ action returns an error.
--
-- /Important:/ When this API is called, AWS CodePipeline returns temporary credentials for the S3 bucket used to store artifacts for the pipeline, if the action requires access to that S3 bucket for input or output artifacts. This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.PollForJobs
  ( -- * Creating a request
    PollForJobs (..),
    mkPollForJobs,

    -- ** Request lenses
    pfjActionTypeId,
    pfjMaxBatchSize,
    pfjQueryParam,

    -- * Destructuring the response
    PollForJobsResponse (..),
    mkPollForJobsResponse,

    -- ** Response lenses
    pfjrrsJobs,
    pfjrrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PollForJobs@ action.
--
-- /See:/ 'mkPollForJobs' smart constructor.
data PollForJobs = PollForJobs'
  { -- | Represents information about an action type.
    actionTypeId :: Types.ActionTypeId,
    -- | The maximum number of jobs to return in a poll for jobs call.
    maxBatchSize :: Core.Maybe Core.Natural,
    -- | A map of property names and values. For an action type with no queryable properties, this value must be null or an empty map. For an action type with a queryable property, you must supply that property as a key in the map. Only jobs whose action configuration matches the mapped value are returned.
    queryParam :: Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationQueryableValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForJobs' value with any optional fields omitted.
mkPollForJobs ::
  -- | 'actionTypeId'
  Types.ActionTypeId ->
  PollForJobs
mkPollForJobs actionTypeId =
  PollForJobs'
    { actionTypeId,
      maxBatchSize = Core.Nothing,
      queryParam = Core.Nothing
    }

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjActionTypeId :: Lens.Lens' PollForJobs Types.ActionTypeId
pfjActionTypeId = Lens.field @"actionTypeId"
{-# DEPRECATED pfjActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | The maximum number of jobs to return in a poll for jobs call.
--
-- /Note:/ Consider using 'maxBatchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjMaxBatchSize :: Lens.Lens' PollForJobs (Core.Maybe Core.Natural)
pfjMaxBatchSize = Lens.field @"maxBatchSize"
{-# DEPRECATED pfjMaxBatchSize "Use generic-lens or generic-optics with 'maxBatchSize' instead." #-}

-- | A map of property names and values. For an action type with no queryable properties, this value must be null or an empty map. For an action type with a queryable property, you must supply that property as a key in the map. Only jobs whose action configuration matches the mapped value are returned.
--
-- /Note:/ Consider using 'queryParam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjQueryParam :: Lens.Lens' PollForJobs (Core.Maybe (Core.HashMap Types.ActionConfigurationKey Types.ActionConfigurationQueryableValue))
pfjQueryParam = Lens.field @"queryParam"
{-# DEPRECATED pfjQueryParam "Use generic-lens or generic-optics with 'queryParam' instead." #-}

instance Core.FromJSON PollForJobs where
  toJSON PollForJobs {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("actionTypeId" Core..= actionTypeId),
            ("maxBatchSize" Core..=) Core.<$> maxBatchSize,
            ("queryParam" Core..=) Core.<$> queryParam
          ]
      )

instance Core.AWSRequest PollForJobs where
  type Rs PollForJobs = PollForJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodePipeline_20150709.PollForJobs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForJobsResponse'
            Core.<$> (x Core..:? "jobs") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @PollForJobs@ action.
--
-- /See:/ 'mkPollForJobsResponse' smart constructor.
data PollForJobsResponse = PollForJobsResponse'
  { -- | Information about the jobs to take action on.
    jobs :: Core.Maybe [Types.Job],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForJobsResponse' value with any optional fields omitted.
mkPollForJobsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PollForJobsResponse
mkPollForJobsResponse responseStatus =
  PollForJobsResponse' {jobs = Core.Nothing, responseStatus}

-- | Information about the jobs to take action on.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjrrsJobs :: Lens.Lens' PollForJobsResponse (Core.Maybe [Types.Job])
pfjrrsJobs = Lens.field @"jobs"
{-# DEPRECATED pfjrrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfjrrsResponseStatus :: Lens.Lens' PollForJobsResponse Core.Int
pfjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pfjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
