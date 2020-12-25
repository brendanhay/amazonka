{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current effective patches (the patch and the approval state) for the specified patch baseline. Note that this API applies only to Windows patch baselines.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
  ( -- * Creating a request
    DescribeEffectivePatchesForPatchBaseline (..),
    mkDescribeEffectivePatchesForPatchBaseline,

    -- ** Request lenses
    depfpbBaselineId,
    depfpbMaxResults,
    depfpbNextToken,

    -- * Destructuring the response
    DescribeEffectivePatchesForPatchBaselineResponse (..),
    mkDescribeEffectivePatchesForPatchBaselineResponse,

    -- ** Response lenses
    depfpbrrsEffectivePatches,
    depfpbrrsNextToken,
    depfpbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeEffectivePatchesForPatchBaseline' smart constructor.
data DescribeEffectivePatchesForPatchBaseline = DescribeEffectivePatchesForPatchBaseline'
  { -- | The ID of the patch baseline to retrieve the effective patches for.
    baselineId :: Types.BaselineId,
    -- | The maximum number of patches to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEffectivePatchesForPatchBaseline' value with any optional fields omitted.
mkDescribeEffectivePatchesForPatchBaseline ::
  -- | 'baselineId'
  Types.BaselineId ->
  DescribeEffectivePatchesForPatchBaseline
mkDescribeEffectivePatchesForPatchBaseline baselineId =
  DescribeEffectivePatchesForPatchBaseline'
    { baselineId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the patch baseline to retrieve the effective patches for.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbBaselineId :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline Types.BaselineId
depfpbBaselineId = Lens.field @"baselineId"
{-# DEPRECATED depfpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbMaxResults :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Core.Maybe Core.Natural)
depfpbMaxResults = Lens.field @"maxResults"
{-# DEPRECATED depfpbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbNextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaseline (Core.Maybe Types.NextToken)
depfpbNextToken = Lens.field @"nextToken"
{-# DEPRECATED depfpbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeEffectivePatchesForPatchBaseline where
  toJSON DescribeEffectivePatchesForPatchBaseline {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BaselineId" Core..= baselineId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeEffectivePatchesForPatchBaseline where
  type
    Rs DescribeEffectivePatchesForPatchBaseline =
      DescribeEffectivePatchesForPatchBaselineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonSSM.DescribeEffectivePatchesForPatchBaseline"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEffectivePatchesForPatchBaselineResponse'
            Core.<$> (x Core..:? "EffectivePatches")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEffectivePatchesForPatchBaseline where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"effectivePatches" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeEffectivePatchesForPatchBaselineResponse' smart constructor.
data DescribeEffectivePatchesForPatchBaselineResponse = DescribeEffectivePatchesForPatchBaselineResponse'
  { -- | An array of patches and patch status.
    effectivePatches :: Core.Maybe [Types.EffectivePatch],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEffectivePatchesForPatchBaselineResponse' value with any optional fields omitted.
mkDescribeEffectivePatchesForPatchBaselineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEffectivePatchesForPatchBaselineResponse
mkDescribeEffectivePatchesForPatchBaselineResponse responseStatus =
  DescribeEffectivePatchesForPatchBaselineResponse'
    { effectivePatches =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of patches and patch status.
--
-- /Note:/ Consider using 'effectivePatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbrrsEffectivePatches :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Core.Maybe [Types.EffectivePatch])
depfpbrrsEffectivePatches = Lens.field @"effectivePatches"
{-# DEPRECATED depfpbrrsEffectivePatches "Use generic-lens or generic-optics with 'effectivePatches' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbrrsNextToken :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse (Core.Maybe Types.NextToken)
depfpbrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED depfpbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depfpbrrsResponseStatus :: Lens.Lens' DescribeEffectivePatchesForPatchBaselineResponse Core.Int
depfpbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED depfpbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
