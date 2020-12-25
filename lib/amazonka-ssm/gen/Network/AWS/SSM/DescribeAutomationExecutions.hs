{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAutomationExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about all active and terminated Automation executions.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAutomationExecutions
  ( -- * Creating a request
    DescribeAutomationExecutions (..),
    mkDescribeAutomationExecutions,

    -- ** Request lenses
    daesFilters,
    daesMaxResults,
    daesNextToken,

    -- * Destructuring the response
    DescribeAutomationExecutionsResponse (..),
    mkDescribeAutomationExecutionsResponse,

    -- ** Response lenses
    daerfrsAutomationExecutionMetadataList,
    daerfrsNextToken,
    daerfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAutomationExecutions' smart constructor.
data DescribeAutomationExecutions = DescribeAutomationExecutions'
  { -- | Filters used to limit the scope of executions that are requested.
    filters :: Core.Maybe (Core.NonEmpty Types.AutomationExecutionFilter),
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutomationExecutions' value with any optional fields omitted.
mkDescribeAutomationExecutions ::
  DescribeAutomationExecutions
mkDescribeAutomationExecutions =
  DescribeAutomationExecutions'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Filters used to limit the scope of executions that are requested.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesFilters :: Lens.Lens' DescribeAutomationExecutions (Core.Maybe (Core.NonEmpty Types.AutomationExecutionFilter))
daesFilters = Lens.field @"filters"
{-# DEPRECATED daesFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesMaxResults :: Lens.Lens' DescribeAutomationExecutions (Core.Maybe Core.Natural)
daesMaxResults = Lens.field @"maxResults"
{-# DEPRECATED daesMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesNextToken :: Lens.Lens' DescribeAutomationExecutions (Core.Maybe Types.NextToken)
daesNextToken = Lens.field @"nextToken"
{-# DEPRECATED daesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAutomationExecutions where
  toJSON DescribeAutomationExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAutomationExecutions where
  type
    Rs DescribeAutomationExecutions =
      DescribeAutomationExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeAutomationExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutomationExecutionsResponse'
            Core.<$> (x Core..:? "AutomationExecutionMetadataList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAutomationExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"automationExecutionMetadataList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAutomationExecutionsResponse' smart constructor.
data DescribeAutomationExecutionsResponse = DescribeAutomationExecutionsResponse'
  { -- | The list of details about each automation execution which has occurred which matches the filter specification, if any.
    automationExecutionMetadataList :: Core.Maybe [Types.AutomationExecutionMetadata],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAutomationExecutionsResponse' value with any optional fields omitted.
mkDescribeAutomationExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAutomationExecutionsResponse
mkDescribeAutomationExecutionsResponse responseStatus =
  DescribeAutomationExecutionsResponse'
    { automationExecutionMetadataList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of details about each automation execution which has occurred which matches the filter specification, if any.
--
-- /Note:/ Consider using 'automationExecutionMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerfrsAutomationExecutionMetadataList :: Lens.Lens' DescribeAutomationExecutionsResponse (Core.Maybe [Types.AutomationExecutionMetadata])
daerfrsAutomationExecutionMetadataList = Lens.field @"automationExecutionMetadataList"
{-# DEPRECATED daerfrsAutomationExecutionMetadataList "Use generic-lens or generic-optics with 'automationExecutionMetadataList' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerfrsNextToken :: Lens.Lens' DescribeAutomationExecutionsResponse (Core.Maybe Types.NextToken)
daerfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daerfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerfrsResponseStatus :: Lens.Lens' DescribeAutomationExecutionsResponse Core.Int
daerfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daerfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
