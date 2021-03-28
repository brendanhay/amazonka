{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PollForThirdPartyJobs (..)
    , mkPollForThirdPartyJobs
    -- ** Request lenses
    , pftpjActionTypeId
    , pftpjMaxBatchSize

    -- * Destructuring the response
    , PollForThirdPartyJobsResponse (..)
    , mkPollForThirdPartyJobsResponse
    -- ** Response lenses
    , pftpjrrsJobs
    , pftpjrrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'mkPollForThirdPartyJobs' smart constructor.
data PollForThirdPartyJobs = PollForThirdPartyJobs'
  { actionTypeId :: Types.ActionTypeId
    -- ^ Represents information about an action type.
  , maxBatchSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of jobs to return in a poll for jobs call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForThirdPartyJobs' value with any optional fields omitted.
mkPollForThirdPartyJobs
    :: Types.ActionTypeId -- ^ 'actionTypeId'
    -> PollForThirdPartyJobs
mkPollForThirdPartyJobs actionTypeId
  = PollForThirdPartyJobs'{actionTypeId, maxBatchSize = Core.Nothing}

-- | Represents information about an action type.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjActionTypeId :: Lens.Lens' PollForThirdPartyJobs Types.ActionTypeId
pftpjActionTypeId = Lens.field @"actionTypeId"
{-# INLINEABLE pftpjActionTypeId #-}
{-# DEPRECATED actionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead"  #-}

-- | The maximum number of jobs to return in a poll for jobs call.
--
-- /Note:/ Consider using 'maxBatchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjMaxBatchSize :: Lens.Lens' PollForThirdPartyJobs (Core.Maybe Core.Natural)
pftpjMaxBatchSize = Lens.field @"maxBatchSize"
{-# INLINEABLE pftpjMaxBatchSize #-}
{-# DEPRECATED maxBatchSize "Use generic-lens or generic-optics with 'maxBatchSize' instead"  #-}

instance Core.ToQuery PollForThirdPartyJobs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PollForThirdPartyJobs where
        toHeaders PollForThirdPartyJobs{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.PollForThirdPartyJobs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PollForThirdPartyJobs where
        toJSON PollForThirdPartyJobs{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("actionTypeId" Core..= actionTypeId),
                  ("maxBatchSize" Core..=) Core.<$> maxBatchSize])

instance Core.AWSRequest PollForThirdPartyJobs where
        type Rs PollForThirdPartyJobs = PollForThirdPartyJobsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PollForThirdPartyJobsResponse' Core.<$>
                   (x Core..:? "jobs") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'mkPollForThirdPartyJobsResponse' smart constructor.
data PollForThirdPartyJobsResponse = PollForThirdPartyJobsResponse'
  { jobs :: Core.Maybe [Types.ThirdPartyJob]
    -- ^ Information about the jobs to take action on.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForThirdPartyJobsResponse' value with any optional fields omitted.
mkPollForThirdPartyJobsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PollForThirdPartyJobsResponse
mkPollForThirdPartyJobsResponse responseStatus
  = PollForThirdPartyJobsResponse'{jobs = Core.Nothing,
                                   responseStatus}

-- | Information about the jobs to take action on.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjrrsJobs :: Lens.Lens' PollForThirdPartyJobsResponse (Core.Maybe [Types.ThirdPartyJob])
pftpjrrsJobs = Lens.field @"jobs"
{-# INLINEABLE pftpjrrsJobs #-}
{-# DEPRECATED jobs "Use generic-lens or generic-optics with 'jobs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftpjrrsResponseStatus :: Lens.Lens' PollForThirdPartyJobsResponse Core.Int
pftpjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pftpjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
