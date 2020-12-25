{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAutomationStepExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about all active and terminated step executions in an Automation workflow.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAutomationStepExecutions
  ( -- * Creating a request
    DescribeAutomationStepExecutions (..),
    mkDescribeAutomationStepExecutions,

    -- ** Request lenses
    daseAutomationExecutionId,
    daseFilters,
    daseMaxResults,
    daseNextToken,
    daseReverseOrder,

    -- * Destructuring the response
    DescribeAutomationStepExecutionsResponse (..),
    mkDescribeAutomationStepExecutionsResponse,

    -- ** Response lenses
    daserrsNextToken,
    daserrsStepExecutions,
    daserrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAutomationStepExecutions' smart constructor.
data DescribeAutomationStepExecutions = DescribeAutomationStepExecutions'
  { -- | The Automation execution ID for which you want step execution descriptions.
    automationExecutionId :: Types.AutomationExecutionId,
    -- | One or more filters to limit the number of step executions returned by the request.
    filters :: Core.Maybe (Core.NonEmpty Types.StepExecutionFilter),
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | A boolean that indicates whether to list step executions in reverse order by start time. The default value is false.
    reverseOrder :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutomationStepExecutions' value with any optional fields omitted.
mkDescribeAutomationStepExecutions ::
  -- | 'automationExecutionId'
  Types.AutomationExecutionId ->
  DescribeAutomationStepExecutions
mkDescribeAutomationStepExecutions automationExecutionId =
  DescribeAutomationStepExecutions'
    { automationExecutionId,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      reverseOrder = Core.Nothing
    }

-- | The Automation execution ID for which you want step execution descriptions.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseAutomationExecutionId :: Lens.Lens' DescribeAutomationStepExecutions Types.AutomationExecutionId
daseAutomationExecutionId = Lens.field @"automationExecutionId"
{-# DEPRECATED daseAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | One or more filters to limit the number of step executions returned by the request.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseFilters :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe (Core.NonEmpty Types.StepExecutionFilter))
daseFilters = Lens.field @"filters"
{-# DEPRECATED daseFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseMaxResults :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe Core.Natural)
daseMaxResults = Lens.field @"maxResults"
{-# DEPRECATED daseMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseNextToken :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe Types.NextToken)
daseNextToken = Lens.field @"nextToken"
{-# DEPRECATED daseNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A boolean that indicates whether to list step executions in reverse order by start time. The default value is false.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseReverseOrder :: Lens.Lens' DescribeAutomationStepExecutions (Core.Maybe Core.Bool)
daseReverseOrder = Lens.field @"reverseOrder"
{-# DEPRECATED daseReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

instance Core.FromJSON DescribeAutomationStepExecutions where
  toJSON DescribeAutomationStepExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AutomationExecutionId" Core..= automationExecutionId),
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ReverseOrder" Core..=) Core.<$> reverseOrder
          ]
      )

instance Core.AWSRequest DescribeAutomationStepExecutions where
  type
    Rs DescribeAutomationStepExecutions =
      DescribeAutomationStepExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeAutomationStepExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutomationStepExecutionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "StepExecutions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAutomationStepExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"stepExecutions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAutomationStepExecutionsResponse' smart constructor.
data DescribeAutomationStepExecutionsResponse = DescribeAutomationStepExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of details about the current state of all steps that make up an execution.
    stepExecutions :: Core.Maybe [Types.StepExecution],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAutomationStepExecutionsResponse' value with any optional fields omitted.
mkDescribeAutomationStepExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAutomationStepExecutionsResponse
mkDescribeAutomationStepExecutionsResponse responseStatus =
  DescribeAutomationStepExecutionsResponse'
    { nextToken =
        Core.Nothing,
      stepExecutions = Core.Nothing,
      responseStatus
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daserrsNextToken :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Core.Maybe Types.NextToken)
daserrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daserrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of details about the current state of all steps that make up an execution.
--
-- /Note:/ Consider using 'stepExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daserrsStepExecutions :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Core.Maybe [Types.StepExecution])
daserrsStepExecutions = Lens.field @"stepExecutions"
{-# DEPRECATED daserrsStepExecutions "Use generic-lens or generic-optics with 'stepExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daserrsResponseStatus :: Lens.Lens' DescribeAutomationStepExecutionsResponse Core.Int
daserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
