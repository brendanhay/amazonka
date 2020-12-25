{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment runs that are specified by the ARNs of the assessment runs.
module Network.AWS.Inspector.DescribeAssessmentRuns
  ( -- * Creating a request
    DescribeAssessmentRuns (..),
    mkDescribeAssessmentRuns,

    -- ** Request lenses
    darAssessmentRunArns,

    -- * Destructuring the response
    DescribeAssessmentRunsResponse (..),
    mkDescribeAssessmentRunsResponse,

    -- ** Response lenses
    darrrsAssessmentRuns,
    darrrsFailedItems,
    darrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAssessmentRuns' smart constructor.
newtype DescribeAssessmentRuns = DescribeAssessmentRuns'
  { -- | The ARN that specifies the assessment run that you want to describe.
    assessmentRunArns :: Core.NonEmpty Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAssessmentRuns' value with any optional fields omitted.
mkDescribeAssessmentRuns ::
  -- | 'assessmentRunArns'
  Core.NonEmpty Types.Arn ->
  DescribeAssessmentRuns
mkDescribeAssessmentRuns assessmentRunArns =
  DescribeAssessmentRuns' {assessmentRunArns}

-- | The ARN that specifies the assessment run that you want to describe.
--
-- /Note:/ Consider using 'assessmentRunArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darAssessmentRunArns :: Lens.Lens' DescribeAssessmentRuns (Core.NonEmpty Types.Arn)
darAssessmentRunArns = Lens.field @"assessmentRunArns"
{-# DEPRECATED darAssessmentRunArns "Use generic-lens or generic-optics with 'assessmentRunArns' instead." #-}

instance Core.FromJSON DescribeAssessmentRuns where
  toJSON DescribeAssessmentRuns {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("assessmentRunArns" Core..= assessmentRunArns)]
      )

instance Core.AWSRequest DescribeAssessmentRuns where
  type Rs DescribeAssessmentRuns = DescribeAssessmentRunsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.DescribeAssessmentRuns")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentRunsResponse'
            Core.<$> (x Core..:? "assessmentRuns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "failedItems" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAssessmentRunsResponse' smart constructor.
data DescribeAssessmentRunsResponse = DescribeAssessmentRunsResponse'
  { -- | Information about the assessment run.
    assessmentRuns :: [Types.AssessmentRun],
    -- | Assessment run details that cannot be described. An error code is provided for each failed item.
    failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAssessmentRunsResponse' value with any optional fields omitted.
mkDescribeAssessmentRunsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAssessmentRunsResponse
mkDescribeAssessmentRunsResponse responseStatus =
  DescribeAssessmentRunsResponse'
    { assessmentRuns = Core.mempty,
      failedItems = Core.mempty,
      responseStatus
    }

-- | Information about the assessment run.
--
-- /Note:/ Consider using 'assessmentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrrsAssessmentRuns :: Lens.Lens' DescribeAssessmentRunsResponse [Types.AssessmentRun]
darrrsAssessmentRuns = Lens.field @"assessmentRuns"
{-# DEPRECATED darrrsAssessmentRuns "Use generic-lens or generic-optics with 'assessmentRuns' instead." #-}

-- | Assessment run details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrrsFailedItems :: Lens.Lens' DescribeAssessmentRunsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
darrrsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED darrrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrrsResponseStatus :: Lens.Lens' DescribeAssessmentRunsResponse Core.Int
darrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
