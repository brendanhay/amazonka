{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListUniqueProblems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique problems, such as exceptions or crashes.
--
-- Unique problems are defined as a single instance of an error across a run, job, or suite. For example, if a call in your application consistently raises an exception (@OutOfBoundsException in MyActivity.java:386@ ), @ListUniqueProblems@ returns a single entry instead of many individual entries for that exception.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListUniqueProblems
  ( -- * Creating a request
    ListUniqueProblems (..),
    mkListUniqueProblems,

    -- ** Request lenses
    lupArn,
    lupNextToken,

    -- * Destructuring the response
    ListUniqueProblemsResponse (..),
    mkListUniqueProblemsResponse,

    -- ** Response lenses
    luprrsNextToken,
    luprrsUniqueProblems,
    luprrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list unique problems operation.
--
-- /See:/ 'mkListUniqueProblems' smart constructor.
data ListUniqueProblems = ListUniqueProblems'
  { -- | The unique problems' ARNs.
    arn :: Types.Arn,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUniqueProblems' value with any optional fields omitted.
mkListUniqueProblems ::
  -- | 'arn'
  Types.Arn ->
  ListUniqueProblems
mkListUniqueProblems arn =
  ListUniqueProblems' {arn, nextToken = Core.Nothing}

-- | The unique problems' ARNs.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupArn :: Lens.Lens' ListUniqueProblems Types.Arn
lupArn = Lens.field @"arn"
{-# DEPRECATED lupArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupNextToken :: Lens.Lens' ListUniqueProblems (Core.Maybe Types.PaginationToken)
lupNextToken = Lens.field @"nextToken"
{-# DEPRECATED lupNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListUniqueProblems where
  toJSON ListUniqueProblems {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("arn" Core..= arn),
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListUniqueProblems where
  type Rs ListUniqueProblems = ListUniqueProblemsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.ListUniqueProblems")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUniqueProblemsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "uniqueProblems")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListUniqueProblems where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"uniqueProblems" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the result of a list unique problems request.
--
-- /See:/ 'mkListUniqueProblemsResponse' smart constructor.
data ListUniqueProblemsResponse = ListUniqueProblemsResponse'
  { -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Information about the unique problems.
    --
    -- Allowed values include:
    --
    --     * PENDING
    --
    --
    --     * PASSED
    --
    --
    --     * WARNED
    --
    --
    --     * FAILED
    --
    --
    --     * SKIPPED
    --
    --
    --     * ERRORED
    --
    --
    --     * STOPPED
    uniqueProblems :: Core.Maybe (Core.HashMap Types.ExecutionResult [Types.UniqueProblem]),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListUniqueProblemsResponse' value with any optional fields omitted.
mkListUniqueProblemsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListUniqueProblemsResponse
mkListUniqueProblemsResponse responseStatus =
  ListUniqueProblemsResponse'
    { nextToken = Core.Nothing,
      uniqueProblems = Core.Nothing,
      responseStatus
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsNextToken :: Lens.Lens' ListUniqueProblemsResponse (Core.Maybe Types.PaginationToken)
luprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED luprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the unique problems.
--
-- Allowed values include:
--
--     * PENDING
--
--
--     * PASSED
--
--
--     * WARNED
--
--
--     * FAILED
--
--
--     * SKIPPED
--
--
--     * ERRORED
--
--
--     * STOPPED
--
--
--
-- /Note:/ Consider using 'uniqueProblems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsUniqueProblems :: Lens.Lens' ListUniqueProblemsResponse (Core.Maybe (Core.HashMap Types.ExecutionResult [Types.UniqueProblem]))
luprrsUniqueProblems = Lens.field @"uniqueProblems"
{-# DEPRECATED luprrsUniqueProblems "Use generic-lens or generic-optics with 'uniqueProblems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprrsResponseStatus :: Lens.Lens' ListUniqueProblemsResponse Core.Int
luprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED luprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
