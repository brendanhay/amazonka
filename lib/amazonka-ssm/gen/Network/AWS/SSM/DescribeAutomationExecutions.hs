{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    daesNextToken,
    daesMaxResults,

    -- * Destructuring the response
    DescribeAutomationExecutionsResponse (..),
    mkDescribeAutomationExecutionsResponse,

    -- ** Response lenses
    daesrsNextToken,
    daesrsAutomationExecutionMetadataList,
    daesrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeAutomationExecutions' smart constructor.
data DescribeAutomationExecutions = DescribeAutomationExecutions'
  { filters ::
      Lude.Maybe
        ( Lude.NonEmpty
            AutomationExecutionFilter
        ),
    nextToken :: Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutomationExecutions' with the minimum fields required to make a request.
--
-- * 'filters' - Filters used to limit the scope of executions that are requested.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeAutomationExecutions ::
  DescribeAutomationExecutions
mkDescribeAutomationExecutions =
  DescribeAutomationExecutions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters used to limit the scope of executions that are requested.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesFilters :: Lens.Lens' DescribeAutomationExecutions (Lude.Maybe (Lude.NonEmpty AutomationExecutionFilter))
daesFilters = Lens.lens (filters :: DescribeAutomationExecutions -> Lude.Maybe (Lude.NonEmpty AutomationExecutionFilter)) (\s a -> s {filters = a} :: DescribeAutomationExecutions)
{-# DEPRECATED daesFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesNextToken :: Lens.Lens' DescribeAutomationExecutions (Lude.Maybe Lude.Text)
daesNextToken = Lens.lens (nextToken :: DescribeAutomationExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutomationExecutions)
{-# DEPRECATED daesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesMaxResults :: Lens.Lens' DescribeAutomationExecutions (Lude.Maybe Lude.Natural)
daesMaxResults = Lens.lens (maxResults :: DescribeAutomationExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAutomationExecutions)
{-# DEPRECATED daesMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeAutomationExecutions where
  page rq rs
    | Page.stop (rs Lens.^. daesrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daesrsAutomationExecutionMetadataList) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daesNextToken Lens..~ rs Lens.^. daesrsNextToken

instance Lude.AWSRequest DescribeAutomationExecutions where
  type
    Rs DescribeAutomationExecutions =
      DescribeAutomationExecutionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAutomationExecutionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AutomationExecutionMetadataList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAutomationExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeAutomationExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAutomationExecutions where
  toJSON DescribeAutomationExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeAutomationExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAutomationExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAutomationExecutionsResponse' smart constructor.
data DescribeAutomationExecutionsResponse = DescribeAutomationExecutionsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    automationExecutionMetadataList ::
      Lude.Maybe
        [AutomationExecutionMetadata],
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

-- | Creates a value of 'DescribeAutomationExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'automationExecutionMetadataList' - The list of details about each automation execution which has occurred which matches the filter specification, if any.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeAutomationExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAutomationExecutionsResponse
mkDescribeAutomationExecutionsResponse pResponseStatus_ =
  DescribeAutomationExecutionsResponse'
    { nextToken = Lude.Nothing,
      automationExecutionMetadataList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesrsNextToken :: Lens.Lens' DescribeAutomationExecutionsResponse (Lude.Maybe Lude.Text)
daesrsNextToken = Lens.lens (nextToken :: DescribeAutomationExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutomationExecutionsResponse)
{-# DEPRECATED daesrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of details about each automation execution which has occurred which matches the filter specification, if any.
--
-- /Note:/ Consider using 'automationExecutionMetadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesrsAutomationExecutionMetadataList :: Lens.Lens' DescribeAutomationExecutionsResponse (Lude.Maybe [AutomationExecutionMetadata])
daesrsAutomationExecutionMetadataList = Lens.lens (automationExecutionMetadataList :: DescribeAutomationExecutionsResponse -> Lude.Maybe [AutomationExecutionMetadata]) (\s a -> s {automationExecutionMetadataList = a} :: DescribeAutomationExecutionsResponse)
{-# DEPRECATED daesrsAutomationExecutionMetadataList "Use generic-lens or generic-optics with 'automationExecutionMetadataList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daesrsResponseStatus :: Lens.Lens' DescribeAutomationExecutionsResponse Lude.Int
daesrsResponseStatus = Lens.lens (responseStatus :: DescribeAutomationExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAutomationExecutionsResponse)
{-# DEPRECATED daesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
