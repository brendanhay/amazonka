{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information on the shipping label of a Snow device that is being returned to AWS.
module Network.AWS.Snowball.DescribeReturnShippingLabel
  ( -- * Creating a request
    DescribeReturnShippingLabel (..),
    mkDescribeReturnShippingLabel,

    -- ** Request lenses
    drslJobId,

    -- * Destructuring the response
    DescribeReturnShippingLabelResponse (..),
    mkDescribeReturnShippingLabelResponse,

    -- ** Response lenses
    drslrrsExpirationDate,
    drslrrsStatus,
    drslrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkDescribeReturnShippingLabel' smart constructor.
newtype DescribeReturnShippingLabel = DescribeReturnShippingLabel'
  { -- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Core.Maybe Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReturnShippingLabel' value with any optional fields omitted.
mkDescribeReturnShippingLabel ::
  DescribeReturnShippingLabel
mkDescribeReturnShippingLabel =
  DescribeReturnShippingLabel' {jobId = Core.Nothing}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslJobId :: Lens.Lens' DescribeReturnShippingLabel (Core.Maybe Types.JobId)
drslJobId = Lens.field @"jobId"
{-# DEPRECATED drslJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON DescribeReturnShippingLabel where
  toJSON DescribeReturnShippingLabel {..} =
    Core.object (Core.catMaybes [("JobId" Core..=) Core.<$> jobId])

instance Core.AWSRequest DescribeReturnShippingLabel where
  type
    Rs DescribeReturnShippingLabel =
      DescribeReturnShippingLabelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.DescribeReturnShippingLabel"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReturnShippingLabelResponse'
            Core.<$> (x Core..:? "ExpirationDate")
            Core.<*> (x Core..:? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeReturnShippingLabelResponse' smart constructor.
data DescribeReturnShippingLabelResponse = DescribeReturnShippingLabelResponse'
  { -- | The expiration date of the current return shipping label.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The status information of the task on a Snow device that is being returned to AWS.
    status :: Core.Maybe Types.ShippingLabelStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeReturnShippingLabelResponse' value with any optional fields omitted.
mkDescribeReturnShippingLabelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReturnShippingLabelResponse
mkDescribeReturnShippingLabelResponse responseStatus =
  DescribeReturnShippingLabelResponse'
    { expirationDate =
        Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The expiration date of the current return shipping label.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrrsExpirationDate :: Lens.Lens' DescribeReturnShippingLabelResponse (Core.Maybe Core.NominalDiffTime)
drslrrsExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED drslrrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The status information of the task on a Snow device that is being returned to AWS.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrrsStatus :: Lens.Lens' DescribeReturnShippingLabelResponse (Core.Maybe Types.ShippingLabelStatus)
drslrrsStatus = Lens.field @"status"
{-# DEPRECATED drslrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrrsResponseStatus :: Lens.Lens' DescribeReturnShippingLabelResponse Core.Int
drslrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drslrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
