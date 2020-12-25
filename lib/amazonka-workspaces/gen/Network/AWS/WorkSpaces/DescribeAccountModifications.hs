{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeAccountModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes modifications to the configuration of Bring Your Own License (BYOL) for the specified account.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeAccountModifications
  ( -- * Creating a request
    DescribeAccountModifications (..),
    mkDescribeAccountModifications,

    -- ** Request lenses
    damNextToken,

    -- * Destructuring the response
    DescribeAccountModificationsResponse (..),
    mkDescribeAccountModificationsResponse,

    -- ** Response lenses
    damrrsAccountModifications,
    damrrsNextToken,
    damrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeAccountModifications' smart constructor.
newtype DescribeAccountModifications = DescribeAccountModifications'
  { -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountModifications' value with any optional fields omitted.
mkDescribeAccountModifications ::
  DescribeAccountModifications
mkDescribeAccountModifications =
  DescribeAccountModifications' {nextToken = Core.Nothing}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damNextToken :: Lens.Lens' DescribeAccountModifications (Core.Maybe Types.PaginationToken)
damNextToken = Lens.field @"nextToken"
{-# DEPRECATED damNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAccountModifications where
  toJSON DescribeAccountModifications {..} =
    Core.object
      (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeAccountModifications where
  type
    Rs DescribeAccountModifications =
      DescribeAccountModificationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.DescribeAccountModifications")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountModificationsResponse'
            Core.<$> (x Core..:? "AccountModifications")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAccountModifications where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"accountModifications" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAccountModificationsResponse' smart constructor.
data DescribeAccountModificationsResponse = DescribeAccountModificationsResponse'
  { -- | The list of modifications to the configuration of BYOL.
    accountModifications :: Core.Maybe [Types.AccountModification],
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAccountModificationsResponse' value with any optional fields omitted.
mkDescribeAccountModificationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAccountModificationsResponse
mkDescribeAccountModificationsResponse responseStatus =
  DescribeAccountModificationsResponse'
    { accountModifications =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of modifications to the configuration of BYOL.
--
-- /Note:/ Consider using 'accountModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsAccountModifications :: Lens.Lens' DescribeAccountModificationsResponse (Core.Maybe [Types.AccountModification])
damrrsAccountModifications = Lens.field @"accountModifications"
{-# DEPRECATED damrrsAccountModifications "Use generic-lens or generic-optics with 'accountModifications' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsNextToken :: Lens.Lens' DescribeAccountModificationsResponse (Core.Maybe Types.PaginationToken)
damrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED damrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damrrsResponseStatus :: Lens.Lens' DescribeAccountModificationsResponse Core.Int
damrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED damrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
