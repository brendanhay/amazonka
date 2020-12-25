{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a dominant language detection job. Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
  ( -- * Creating a request
    DescribeDominantLanguageDetectionJob (..),
    mkDescribeDominantLanguageDetectionJob,

    -- ** Request lenses
    ddldjJobId,

    -- * Destructuring the response
    DescribeDominantLanguageDetectionJobResponse (..),
    mkDescribeDominantLanguageDetectionJobResponse,

    -- ** Response lenses
    ddldjrrsDominantLanguageDetectionJobProperties,
    ddldjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDominantLanguageDetectionJob' smart constructor.
newtype DescribeDominantLanguageDetectionJob = DescribeDominantLanguageDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDominantLanguageDetectionJob' value with any optional fields omitted.
mkDescribeDominantLanguageDetectionJob ::
  -- | 'jobId'
  Types.JobId ->
  DescribeDominantLanguageDetectionJob
mkDescribeDominantLanguageDetectionJob jobId =
  DescribeDominantLanguageDetectionJob' {jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddldjJobId :: Lens.Lens' DescribeDominantLanguageDetectionJob Types.JobId
ddldjJobId = Lens.field @"jobId"
{-# DEPRECATED ddldjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON DescribeDominantLanguageDetectionJob where
  toJSON DescribeDominantLanguageDetectionJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribeDominantLanguageDetectionJob where
  type
    Rs DescribeDominantLanguageDetectionJob =
      DescribeDominantLanguageDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.DescribeDominantLanguageDetectionJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDominantLanguageDetectionJobResponse'
            Core.<$> (x Core..:? "DominantLanguageDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDominantLanguageDetectionJobResponse' smart constructor.
data DescribeDominantLanguageDetectionJobResponse = DescribeDominantLanguageDetectionJobResponse'
  { -- | An object that contains the properties associated with a dominant language detection job.
    dominantLanguageDetectionJobProperties :: Core.Maybe Types.DominantLanguageDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDominantLanguageDetectionJobResponse' value with any optional fields omitted.
mkDescribeDominantLanguageDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDominantLanguageDetectionJobResponse
mkDescribeDominantLanguageDetectionJobResponse responseStatus =
  DescribeDominantLanguageDetectionJobResponse'
    { dominantLanguageDetectionJobProperties =
        Core.Nothing,
      responseStatus
    }

-- | An object that contains the properties associated with a dominant language detection job.
--
-- /Note:/ Consider using 'dominantLanguageDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddldjrrsDominantLanguageDetectionJobProperties :: Lens.Lens' DescribeDominantLanguageDetectionJobResponse (Core.Maybe Types.DominantLanguageDetectionJobProperties)
ddldjrrsDominantLanguageDetectionJobProperties = Lens.field @"dominantLanguageDetectionJobProperties"
{-# DEPRECATED ddldjrrsDominantLanguageDetectionJobProperties "Use generic-lens or generic-optics with 'dominantLanguageDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddldjrrsResponseStatus :: Lens.Lens' DescribeDominantLanguageDetectionJobResponse Core.Int
ddldjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddldjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
