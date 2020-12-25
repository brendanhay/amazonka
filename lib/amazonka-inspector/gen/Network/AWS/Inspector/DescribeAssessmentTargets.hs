{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment targets that are specified by the ARNs of the assessment targets.
module Network.AWS.Inspector.DescribeAssessmentTargets
  ( -- * Creating a request
    DescribeAssessmentTargets (..),
    mkDescribeAssessmentTargets,

    -- ** Request lenses
    datAssessmentTargetArns,

    -- * Destructuring the response
    DescribeAssessmentTargetsResponse (..),
    mkDescribeAssessmentTargetsResponse,

    -- ** Response lenses
    drsAssessmentTargets,
    drsFailedItems,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAssessmentTargets' smart constructor.
newtype DescribeAssessmentTargets = DescribeAssessmentTargets'
  { -- | The ARNs that specifies the assessment targets that you want to describe.
    assessmentTargetArns :: Core.NonEmpty Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAssessmentTargets' value with any optional fields omitted.
mkDescribeAssessmentTargets ::
  -- | 'assessmentTargetArns'
  Core.NonEmpty Types.Arn ->
  DescribeAssessmentTargets
mkDescribeAssessmentTargets assessmentTargetArns =
  DescribeAssessmentTargets' {assessmentTargetArns}

-- | The ARNs that specifies the assessment targets that you want to describe.
--
-- /Note:/ Consider using 'assessmentTargetArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTargetArns :: Lens.Lens' DescribeAssessmentTargets (Core.NonEmpty Types.Arn)
datAssessmentTargetArns = Lens.field @"assessmentTargetArns"
{-# DEPRECATED datAssessmentTargetArns "Use generic-lens or generic-optics with 'assessmentTargetArns' instead." #-}

instance Core.FromJSON DescribeAssessmentTargets where
  toJSON DescribeAssessmentTargets {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("assessmentTargetArns" Core..= assessmentTargetArns)]
      )

instance Core.AWSRequest DescribeAssessmentTargets where
  type
    Rs DescribeAssessmentTargets =
      DescribeAssessmentTargetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.DescribeAssessmentTargets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssessmentTargetsResponse'
            Core.<$> (x Core..:? "assessmentTargets" Core..!= Core.mempty)
            Core.<*> (x Core..:? "failedItems" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAssessmentTargetsResponse' smart constructor.
data DescribeAssessmentTargetsResponse = DescribeAssessmentTargetsResponse'
  { -- | Information about the assessment targets.
    assessmentTargets :: [Types.AssessmentTarget],
    -- | Assessment target details that cannot be described. An error code is provided for each failed item.
    failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAssessmentTargetsResponse' value with any optional fields omitted.
mkDescribeAssessmentTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAssessmentTargetsResponse
mkDescribeAssessmentTargetsResponse responseStatus =
  DescribeAssessmentTargetsResponse'
    { assessmentTargets =
        Core.mempty,
      failedItems = Core.mempty,
      responseStatus
    }

-- | Information about the assessment targets.
--
-- /Note:/ Consider using 'assessmentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAssessmentTargets :: Lens.Lens' DescribeAssessmentTargetsResponse [Types.AssessmentTarget]
drsAssessmentTargets = Lens.field @"assessmentTargets"
{-# DEPRECATED drsAssessmentTargets "Use generic-lens or generic-optics with 'assessmentTargets' instead." #-}

-- | Assessment target details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFailedItems :: Lens.Lens' DescribeAssessmentTargetsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
drsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED drsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeAssessmentTargetsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
