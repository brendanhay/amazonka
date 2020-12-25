{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a PII entities detection job. For example, you can use this operation to get the job status.
module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
  ( -- * Creating a request
    DescribePiiEntitiesDetectionJob (..),
    mkDescribePiiEntitiesDetectionJob,

    -- ** Request lenses
    dpedjJobId,

    -- * Destructuring the response
    DescribePiiEntitiesDetectionJobResponse (..),
    mkDescribePiiEntitiesDetectionJobResponse,

    -- ** Response lenses
    dpedjrrsPiiEntitiesDetectionJobProperties,
    dpedjrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePiiEntitiesDetectionJob' smart constructor.
newtype DescribePiiEntitiesDetectionJob = DescribePiiEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePiiEntitiesDetectionJob' value with any optional fields omitted.
mkDescribePiiEntitiesDetectionJob ::
  -- | 'jobId'
  Types.JobId ->
  DescribePiiEntitiesDetectionJob
mkDescribePiiEntitiesDetectionJob jobId =
  DescribePiiEntitiesDetectionJob' {jobId}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjJobId :: Lens.Lens' DescribePiiEntitiesDetectionJob Types.JobId
dpedjJobId = Lens.field @"jobId"
{-# DEPRECATED dpedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON DescribePiiEntitiesDetectionJob where
  toJSON DescribePiiEntitiesDetectionJob {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest DescribePiiEntitiesDetectionJob where
  type
    Rs DescribePiiEntitiesDetectionJob =
      DescribePiiEntitiesDetectionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.DescribePiiEntitiesDetectionJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePiiEntitiesDetectionJobResponse'
            Core.<$> (x Core..:? "PiiEntitiesDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePiiEntitiesDetectionJobResponse' smart constructor.
data DescribePiiEntitiesDetectionJobResponse = DescribePiiEntitiesDetectionJobResponse'
  { piiEntitiesDetectionJobProperties :: Core.Maybe Types.PiiEntitiesDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePiiEntitiesDetectionJobResponse' value with any optional fields omitted.
mkDescribePiiEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePiiEntitiesDetectionJobResponse
mkDescribePiiEntitiesDetectionJobResponse responseStatus =
  DescribePiiEntitiesDetectionJobResponse'
    { piiEntitiesDetectionJobProperties =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'piiEntitiesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjrrsPiiEntitiesDetectionJobProperties :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse (Core.Maybe Types.PiiEntitiesDetectionJobProperties)
dpedjrrsPiiEntitiesDetectionJobProperties = Lens.field @"piiEntitiesDetectionJobProperties"
{-# DEPRECATED dpedjrrsPiiEntitiesDetectionJobProperties "Use generic-lens or generic-optics with 'piiEntitiesDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjrrsResponseStatus :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse Core.Int
dpedjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpedjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
