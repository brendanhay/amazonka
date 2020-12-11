{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociationExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view all executions for a specific association ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutions
  ( -- * Creating a request
    DescribeAssociationExecutions (..),
    mkDescribeAssociationExecutions,

    -- ** Request lenses
    daeFilters,
    daeNextToken,
    daeMaxResults,
    daeAssociationId,

    -- * Destructuring the response
    DescribeAssociationExecutionsResponse (..),
    mkDescribeAssociationExecutionsResponse,

    -- ** Response lenses
    daersNextToken,
    daersAssociationExecutions,
    daersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeAssociationExecutions' smart constructor.
data DescribeAssociationExecutions = DescribeAssociationExecutions'
  { filters ::
      Lude.Maybe
        ( Lude.NonEmpty
            AssociationExecutionFilter
        ),
    nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    associationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAssociationExecutions' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for which you want to view execution history details.
-- * 'filters' - Filters for the request. You can specify the following filters and values.
--
-- ExecutionId (EQUAL)
-- Status (EQUAL)
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
mkDescribeAssociationExecutions ::
  -- | 'associationId'
  Lude.Text ->
  DescribeAssociationExecutions
mkDescribeAssociationExecutions pAssociationId_ =
  DescribeAssociationExecutions'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      associationId = pAssociationId_
    }

-- | Filters for the request. You can specify the following filters and values.
--
-- ExecutionId (EQUAL)
-- Status (EQUAL)
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeFilters :: Lens.Lens' DescribeAssociationExecutions (Lude.Maybe (Lude.NonEmpty AssociationExecutionFilter))
daeFilters = Lens.lens (filters :: DescribeAssociationExecutions -> Lude.Maybe (Lude.NonEmpty AssociationExecutionFilter)) (\s a -> s {filters = a} :: DescribeAssociationExecutions)
{-# DEPRECATED daeFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeNextToken :: Lens.Lens' DescribeAssociationExecutions (Lude.Maybe Lude.Text)
daeNextToken = Lens.lens (nextToken :: DescribeAssociationExecutions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAssociationExecutions)
{-# DEPRECATED daeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeMaxResults :: Lens.Lens' DescribeAssociationExecutions (Lude.Maybe Lude.Natural)
daeMaxResults = Lens.lens (maxResults :: DescribeAssociationExecutions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAssociationExecutions)
{-# DEPRECATED daeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The association ID for which you want to view execution history details.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeAssociationId :: Lens.Lens' DescribeAssociationExecutions Lude.Text
daeAssociationId = Lens.lens (associationId :: DescribeAssociationExecutions -> Lude.Text) (\s a -> s {associationId = a} :: DescribeAssociationExecutions)
{-# DEPRECATED daeAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Page.AWSPager DescribeAssociationExecutions where
  page rq rs
    | Page.stop (rs Lens.^. daersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daersAssociationExecutions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daeNextToken Lens..~ rs Lens.^. daersNextToken

instance Lude.AWSRequest DescribeAssociationExecutions where
  type
    Rs DescribeAssociationExecutions =
      DescribeAssociationExecutionsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAssociationExecutionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AssociationExecutions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAssociationExecutions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeAssociationExecutions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAssociationExecutions where
  toJSON DescribeAssociationExecutions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AssociationId" Lude..= associationId)
          ]
      )

instance Lude.ToPath DescribeAssociationExecutions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAssociationExecutions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAssociationExecutionsResponse' smart constructor.
data DescribeAssociationExecutionsResponse = DescribeAssociationExecutionsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    associationExecutions ::
      Lude.Maybe
        [AssociationExecution],
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

-- | Creates a value of 'DescribeAssociationExecutionsResponse' with the minimum fields required to make a request.
--
-- * 'associationExecutions' - A list of the executions for the specified association ID.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeAssociationExecutionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAssociationExecutionsResponse
mkDescribeAssociationExecutionsResponse pResponseStatus_ =
  DescribeAssociationExecutionsResponse'
    { nextToken = Lude.Nothing,
      associationExecutions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daersNextToken :: Lens.Lens' DescribeAssociationExecutionsResponse (Lude.Maybe Lude.Text)
daersNextToken = Lens.lens (nextToken :: DescribeAssociationExecutionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAssociationExecutionsResponse)
{-# DEPRECATED daersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the executions for the specified association ID.
--
-- /Note:/ Consider using 'associationExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daersAssociationExecutions :: Lens.Lens' DescribeAssociationExecutionsResponse (Lude.Maybe [AssociationExecution])
daersAssociationExecutions = Lens.lens (associationExecutions :: DescribeAssociationExecutionsResponse -> Lude.Maybe [AssociationExecution]) (\s a -> s {associationExecutions = a} :: DescribeAssociationExecutionsResponse)
{-# DEPRECATED daersAssociationExecutions "Use generic-lens or generic-optics with 'associationExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daersResponseStatus :: Lens.Lens' DescribeAssociationExecutionsResponse Lude.Int
daersResponseStatus = Lens.lens (responseStatus :: DescribeAssociationExecutionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAssociationExecutionsResponse)
{-# DEPRECATED daersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
