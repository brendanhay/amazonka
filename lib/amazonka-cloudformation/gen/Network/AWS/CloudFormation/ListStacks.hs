{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the summary information for stacks whose status matches the specified StackStatusFilter. Summary information for stacks that have been deleted is kept for 90 days after the stack is deleted. If no StackStatusFilter is specified, summary information for all stacks is returned (including existing stacks and stacks that have been deleted).
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStacks
  ( -- * Creating a request
    ListStacks (..),
    mkListStacks,

    -- ** Request lenses
    lsNextToken,
    lsStackStatusFilter,

    -- * Destructuring the response
    ListStacksResponse (..),
    mkListStacksResponse,

    -- ** Response lenses
    lsrrsNextToken,
    lsrrsStackSummaries,
    lsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for 'ListStacks' action.
--
-- /See:/ 'mkListStacks' smart constructor.
data ListStacks = ListStacks'
  { -- | A string that identifies the next page of stacks that you want to retrieve.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Stack status to use as a filter. Specify one or more stack status codes to list only stacks with the specified status codes. For a complete list of stack status codes, see the @StackStatus@ parameter of the 'Stack' data type.
    stackStatusFilter :: Core.Maybe [Types.StackStatus]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStacks' value with any optional fields omitted.
mkListStacks ::
  ListStacks
mkListStacks =
  ListStacks'
    { nextToken = Core.Nothing,
      stackStatusFilter = Core.Nothing
    }

-- | A string that identifies the next page of stacks that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListStacks (Core.Maybe Types.NextToken)
lsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Stack status to use as a filter. Specify one or more stack status codes to list only stacks with the specified status codes. For a complete list of stack status codes, see the @StackStatus@ parameter of the 'Stack' data type.
--
-- /Note:/ Consider using 'stackStatusFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStackStatusFilter :: Lens.Lens' ListStacks (Core.Maybe [Types.StackStatus])
lsStackStatusFilter = Lens.field @"stackStatusFilter"
{-# DEPRECATED lsStackStatusFilter "Use generic-lens or generic-optics with 'stackStatusFilter' instead." #-}

instance Core.AWSRequest ListStacks where
  type Rs ListStacks = ListStacksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListStacks")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue
                            "StackStatusFilter"
                            (Core.toQueryList "member" Core.<$> stackStatusFilter)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListStacksResult"
      ( \s h x ->
          ListStacksResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "StackSummaries" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStacks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"stackSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output for 'ListStacks' action.
--
-- /See:/ 'mkListStacksResponse' smart constructor.
data ListStacksResponse = ListStacksResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @StackSummary@ structures containing information about the specified stacks.
    stackSummaries :: Core.Maybe [Types.StackSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStacksResponse' value with any optional fields omitted.
mkListStacksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStacksResponse
mkListStacksResponse responseStatus =
  ListStacksResponse'
    { nextToken = Core.Nothing,
      stackSummaries = Core.Nothing,
      responseStatus
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next page of stacks. If no additional page exists, this value is null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListStacksResponse (Core.Maybe Types.NextToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackSummary@ structures containing information about the specified stacks.
--
-- /Note:/ Consider using 'stackSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsStackSummaries :: Lens.Lens' ListStacksResponse (Core.Maybe [Types.StackSummary])
lsrrsStackSummaries = Lens.field @"stackSummaries"
{-# DEPRECATED lsrrsStackSummaries "Use generic-lens or generic-optics with 'stackSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStacksResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
