{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociationExecutionTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view information about a specific execution of a specific association.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutionTargets
  ( -- * Creating a request
    DescribeAssociationExecutionTargets (..),
    mkDescribeAssociationExecutionTargets,

    -- ** Request lenses
    daetFilters,
    daetNextToken,
    daetMaxResults,
    daetAssociationId,
    daetExecutionId,

    -- * Destructuring the response
    DescribeAssociationExecutionTargetsResponse (..),
    mkDescribeAssociationExecutionTargetsResponse,

    -- ** Response lenses
    daetrsNextToken,
    daetrsAssociationExecutionTargets,
    daetrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeAssociationExecutionTargets' smart constructor.
data DescribeAssociationExecutionTargets = DescribeAssociationExecutionTargets'
  { filters ::
      Lude.Maybe
        ( Lude.NonEmpty
            AssociationExecutionTargetsFilter
        ),
    nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Natural,
    associationId ::
      Lude.Text,
    executionId ::
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

-- | Creates a value of 'DescribeAssociationExecutionTargets' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID that includes the execution for which you want to view details.
-- * 'executionId' - The execution ID for which you want to view details.
-- * 'filters' - Filters for the request. You can specify the following filters and values.
--
-- Status (EQUAL)
-- ResourceId (EQUAL)
-- ResourceType (EQUAL)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
mkDescribeAssociationExecutionTargets ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'executionId'
  Lude.Text ->
  DescribeAssociationExecutionTargets
mkDescribeAssociationExecutionTargets pAssociationId_ pExecutionId_ =
  DescribeAssociationExecutionTargets'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      associationId = pAssociationId_,
      executionId = pExecutionId_
    }

-- | Filters for the request. You can specify the following filters and values.
--
-- Status (EQUAL)
-- ResourceId (EQUAL)
-- ResourceType (EQUAL)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetFilters :: Lens.Lens' DescribeAssociationExecutionTargets (Lude.Maybe (Lude.NonEmpty AssociationExecutionTargetsFilter))
daetFilters = Lens.lens (filters :: DescribeAssociationExecutionTargets -> Lude.Maybe (Lude.NonEmpty AssociationExecutionTargetsFilter)) (\s a -> s {filters = a} :: DescribeAssociationExecutionTargets)
{-# DEPRECATED daetFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetNextToken :: Lens.Lens' DescribeAssociationExecutionTargets (Lude.Maybe Lude.Text)
daetNextToken = Lens.lens (nextToken :: DescribeAssociationExecutionTargets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAssociationExecutionTargets)
{-# DEPRECATED daetNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetMaxResults :: Lens.Lens' DescribeAssociationExecutionTargets (Lude.Maybe Lude.Natural)
daetMaxResults = Lens.lens (maxResults :: DescribeAssociationExecutionTargets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeAssociationExecutionTargets)
{-# DEPRECATED daetMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The association ID that includes the execution for which you want to view details.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetAssociationId :: Lens.Lens' DescribeAssociationExecutionTargets Lude.Text
daetAssociationId = Lens.lens (associationId :: DescribeAssociationExecutionTargets -> Lude.Text) (\s a -> s {associationId = a} :: DescribeAssociationExecutionTargets)
{-# DEPRECATED daetAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The execution ID for which you want to view details.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetExecutionId :: Lens.Lens' DescribeAssociationExecutionTargets Lude.Text
daetExecutionId = Lens.lens (executionId :: DescribeAssociationExecutionTargets -> Lude.Text) (\s a -> s {executionId = a} :: DescribeAssociationExecutionTargets)
{-# DEPRECATED daetExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

instance Page.AWSPager DescribeAssociationExecutionTargets where
  page rq rs
    | Page.stop (rs Lens.^. daetrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. daetrsAssociationExecutionTargets) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daetNextToken Lens..~ rs Lens.^. daetrsNextToken

instance Lude.AWSRequest DescribeAssociationExecutionTargets where
  type
    Rs DescribeAssociationExecutionTargets =
      DescribeAssociationExecutionTargetsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAssociationExecutionTargetsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AssociationExecutionTargets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAssociationExecutionTargets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DescribeAssociationExecutionTargets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAssociationExecutionTargets where
  toJSON DescribeAssociationExecutionTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AssociationId" Lude..= associationId),
            Lude.Just ("ExecutionId" Lude..= executionId)
          ]
      )

instance Lude.ToPath DescribeAssociationExecutionTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAssociationExecutionTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAssociationExecutionTargetsResponse' smart constructor.
data DescribeAssociationExecutionTargetsResponse = DescribeAssociationExecutionTargetsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    associationExecutionTargets ::
      Lude.Maybe
        [AssociationExecutionTarget],
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

-- | Creates a value of 'DescribeAssociationExecutionTargetsResponse' with the minimum fields required to make a request.
--
-- * 'associationExecutionTargets' - Information about the execution.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeAssociationExecutionTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAssociationExecutionTargetsResponse
mkDescribeAssociationExecutionTargetsResponse pResponseStatus_ =
  DescribeAssociationExecutionTargetsResponse'
    { nextToken =
        Lude.Nothing,
      associationExecutionTargets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetrsNextToken :: Lens.Lens' DescribeAssociationExecutionTargetsResponse (Lude.Maybe Lude.Text)
daetrsNextToken = Lens.lens (nextToken :: DescribeAssociationExecutionTargetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAssociationExecutionTargetsResponse)
{-# DEPRECATED daetrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the execution.
--
-- /Note:/ Consider using 'associationExecutionTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetrsAssociationExecutionTargets :: Lens.Lens' DescribeAssociationExecutionTargetsResponse (Lude.Maybe [AssociationExecutionTarget])
daetrsAssociationExecutionTargets = Lens.lens (associationExecutionTargets :: DescribeAssociationExecutionTargetsResponse -> Lude.Maybe [AssociationExecutionTarget]) (\s a -> s {associationExecutionTargets = a} :: DescribeAssociationExecutionTargetsResponse)
{-# DEPRECATED daetrsAssociationExecutionTargets "Use generic-lens or generic-optics with 'associationExecutionTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetrsResponseStatus :: Lens.Lens' DescribeAssociationExecutionTargetsResponse Lude.Int
daetrsResponseStatus = Lens.lens (responseStatus :: DescribeAssociationExecutionTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAssociationExecutionTargetsResponse)
{-# DEPRECATED daetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
