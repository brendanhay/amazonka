{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    daseFilters,
    daseReverseOrder,
    daseNextToken,
    daseMaxResults,
    daseAutomationExecutionId,

    -- * Destructuring the response
    DescribeAutomationStepExecutionsResponse (..),
    mkDescribeAutomationStepExecutionsResponse,

    -- ** Response lenses
    dasersNextToken,
    dasersStepExecutions,
    dasersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeAutomationStepExecutions' smart constructor.
data DescribeAutomationStepExecutions = DescribeAutomationStepExecutions'
  { filters ::
      Lude.Maybe
        ( Lude.NonEmpty
            StepExecutionFilter
        ),
    reverseOrder ::
      Lude.Maybe Lude.Bool,
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    automationExecutionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutomationStepExecutions' with the minimum fields required to make a request.
--
-- * 'automationExecutionId' - The Automation execution ID for which you want step execution descriptions.
-- * 'filters' - One or more filters to limit the number of step executions returned by the request.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'reverseOrder' - A boolean that indicates whether to list step executions in reverse order by start time. The default value is false.
mkDescribeAutomationStepExecutions ::
  -- | 'automationExecutionId'
  Lude.Text ->
  DescribeAutomationStepExecutions
mkDescribeAutomationStepExecutions pAutomationExecutionId_ =
  DescribeAutomationStepExecutions'
    { filters = Lude.Nothing,
      reverseOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      automationExecutionId = pAutomationExecutionId_
    }

-- | One or more filters to limit the number of step executions returned by the request.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseFilters :: Lens.Lens' DescribeAutomationStepExecutions (Lude.Maybe (Lude.NonEmpty StepExecutionFilter))
daseFilters = Lens.lens (filters :: DescribeAutomationStepExecutions -> Lude.Maybe (Lude.NonEmpty StepExecutionFilter)) (\s a -> s {filters = a} :: DescribeAutomationStepExecutions)
{-# DEPRECATED daseFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A boolean that indicates whether to list step executions in reverse order by start time. The default value is false.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseReverseOrder :: Lens.Lens' DescribeAutomationStepExecutions (Lude.Maybe Lude.Bool)
daseReverseOrder = Lens.lens (reverseOrder :: DescribeAutomationStepExecutions -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: DescribeAutomationStepExecutions)
{-# DEPRECATED daseReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseNextToken :: Lens.Lens' DescribeAutomationStepExecutions (Lude.Maybe Lude.Text)
daseNextToken = Lens.lens (nextToken :: DescribeAutomationStepExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutomationStepExecutions)
{-# DEPRECATED daseNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseMaxResults :: Lens.Lens' DescribeAutomationStepExecutions (Lude.Maybe Lude.Natural)
daseMaxResults = Lens.lens (maxResults :: DescribeAutomationStepExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAutomationStepExecutions)
{-# DEPRECATED daseMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Automation execution ID for which you want step execution descriptions.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daseAutomationExecutionId :: Lens.Lens' DescribeAutomationStepExecutions Lude.Text
daseAutomationExecutionId = Lens.lens (automationExecutionId :: DescribeAutomationStepExecutions -> Lude.Text) (\s a -> s {automationExecutionId = a} :: DescribeAutomationStepExecutions)
{-# DEPRECATED daseAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

instance Page.AWSPager DescribeAutomationStepExecutions where
  page rq rs
    | Page.stop (rs Lens.^. dasersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dasersStepExecutions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daseNextToken Lens..~ rs Lens.^. dasersNextToken

instance Lude.AWSRequest DescribeAutomationStepExecutions where
  type
    Rs DescribeAutomationStepExecutions =
      DescribeAutomationStepExecutionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAutomationStepExecutionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "StepExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAutomationStepExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeAutomationStepExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAutomationStepExecutions where
  toJSON DescribeAutomationStepExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("ReverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AutomationExecutionId" Lude..= automationExecutionId)
          ]
      )

instance Lude.ToPath DescribeAutomationStepExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAutomationStepExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAutomationStepExecutionsResponse' smart constructor.
data DescribeAutomationStepExecutionsResponse = DescribeAutomationStepExecutionsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    stepExecutions ::
      Lude.Maybe
        [StepExecution],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutomationStepExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'stepExecutions' - A list of details about the current state of all steps that make up an execution.
mkDescribeAutomationStepExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAutomationStepExecutionsResponse
mkDescribeAutomationStepExecutionsResponse pResponseStatus_ =
  DescribeAutomationStepExecutionsResponse'
    { nextToken =
        Lude.Nothing,
      stepExecutions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasersNextToken :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Lude.Maybe Lude.Text)
dasersNextToken = Lens.lens (nextToken :: DescribeAutomationStepExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutomationStepExecutionsResponse)
{-# DEPRECATED dasersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of details about the current state of all steps that make up an execution.
--
-- /Note:/ Consider using 'stepExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasersStepExecutions :: Lens.Lens' DescribeAutomationStepExecutionsResponse (Lude.Maybe [StepExecution])
dasersStepExecutions = Lens.lens (stepExecutions :: DescribeAutomationStepExecutionsResponse -> Lude.Maybe [StepExecution]) (\s a -> s {stepExecutions = a} :: DescribeAutomationStepExecutionsResponse)
{-# DEPRECATED dasersStepExecutions "Use generic-lens or generic-optics with 'stepExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasersResponseStatus :: Lens.Lens' DescribeAutomationStepExecutionsResponse Lude.Int
dasersResponseStatus = Lens.lens (responseStatus :: DescribeAutomationStepExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAutomationStepExecutionsResponse)
{-# DEPRECATED dasersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
