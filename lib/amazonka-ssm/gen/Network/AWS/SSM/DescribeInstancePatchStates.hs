{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstancePatchStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state of one or more instances.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatchStates
  ( -- * Creating a request
    DescribeInstancePatchStates (..),
    mkDescribeInstancePatchStates,

    -- ** Request lenses
    dipsInstanceIds,
    dipsMaxResults,
    dipsNextToken,

    -- * Destructuring the response
    DescribeInstancePatchStatesResponse (..),
    mkDescribeInstancePatchStatesResponse,

    -- ** Response lenses
    dipsrrsInstancePatchStates,
    dipsrrsNextToken,
    dipsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInstancePatchStates' smart constructor.
data DescribeInstancePatchStates = DescribeInstancePatchStates'
  { -- | The ID of the instance whose patch state information should be retrieved.
    instanceIds :: [Types.InstanceId],
    -- | The maximum number of instances to return (per page).
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstancePatchStates' value with any optional fields omitted.
mkDescribeInstancePatchStates ::
  DescribeInstancePatchStates
mkDescribeInstancePatchStates =
  DescribeInstancePatchStates'
    { instanceIds = Core.mempty,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the instance whose patch state information should be retrieved.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsInstanceIds :: Lens.Lens' DescribeInstancePatchStates [Types.InstanceId]
dipsInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED dipsInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of instances to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsMaxResults :: Lens.Lens' DescribeInstancePatchStates (Core.Maybe Core.Natural)
dipsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dipsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsNextToken :: Lens.Lens' DescribeInstancePatchStates (Core.Maybe Types.NextToken)
dipsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dipsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeInstancePatchStates where
  toJSON DescribeInstancePatchStates {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceIds" Core..= instanceIds),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeInstancePatchStates where
  type
    Rs DescribeInstancePatchStates =
      DescribeInstancePatchStatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DescribeInstancePatchStates")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancePatchStatesResponse'
            Core.<$> (x Core..:? "InstancePatchStates")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInstancePatchStates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"instancePatchStates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInstancePatchStatesResponse' smart constructor.
data DescribeInstancePatchStatesResponse = DescribeInstancePatchStatesResponse'
  { -- | The high-level patch state for the requested instances.
    instancePatchStates :: Core.Maybe [Types.InstancePatchState],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInstancePatchStatesResponse' value with any optional fields omitted.
mkDescribeInstancePatchStatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstancePatchStatesResponse
mkDescribeInstancePatchStatesResponse responseStatus =
  DescribeInstancePatchStatesResponse'
    { instancePatchStates =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The high-level patch state for the requested instances.
--
-- /Note:/ Consider using 'instancePatchStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrrsInstancePatchStates :: Lens.Lens' DescribeInstancePatchStatesResponse (Core.Maybe [Types.InstancePatchState])
dipsrrsInstancePatchStates = Lens.field @"instancePatchStates"
{-# DEPRECATED dipsrrsInstancePatchStates "Use generic-lens or generic-optics with 'instancePatchStates' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrrsNextToken :: Lens.Lens' DescribeInstancePatchStatesResponse (Core.Maybe Types.NextToken)
dipsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dipsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrrsResponseStatus :: Lens.Lens' DescribeInstancePatchStatesResponse Core.Int
dipsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dipsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
