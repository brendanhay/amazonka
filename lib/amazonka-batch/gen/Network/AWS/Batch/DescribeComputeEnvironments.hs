{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeComputeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your compute environments.
--
-- If you are using an unmanaged compute environment, you can use the @DescribeComputeEnvironment@ operation to determine the @ecsClusterArn@ that you should launch your Amazon ECS container instances into.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeComputeEnvironments
  ( -- * Creating a request
    DescribeComputeEnvironments (..),
    mkDescribeComputeEnvironments,

    -- ** Request lenses
    dceComputeEnvironments,
    dceNextToken,
    dceMaxResults,

    -- * Destructuring the response
    DescribeComputeEnvironmentsResponse (..),
    mkDescribeComputeEnvironmentsResponse,

    -- ** Response lenses
    drsComputeEnvironments,
    drsNextToken,
    drsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeComputeEnvironments' smart constructor.
data DescribeComputeEnvironments = DescribeComputeEnvironments'
  { computeEnvironments ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeComputeEnvironments' with the minimum fields required to make a request.
--
-- * 'computeEnvironments' - A list of up to 100 compute environment names or full Amazon Resource Name (ARN) entries.
-- * 'maxResults' - The maximum number of cluster results returned by @DescribeComputeEnvironments@ in paginated output. When this parameter is used, @DescribeComputeEnvironments@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeComputeEnvironments@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeComputeEnvironments@ returns up to 100 results and a @nextToken@ value if applicable.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeComputeEnvironments@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
mkDescribeComputeEnvironments ::
  DescribeComputeEnvironments
mkDescribeComputeEnvironments =
  DescribeComputeEnvironments'
    { computeEnvironments = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of up to 100 compute environment names or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'computeEnvironments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceComputeEnvironments :: Lens.Lens' DescribeComputeEnvironments (Lude.Maybe [Lude.Text])
dceComputeEnvironments = Lens.lens (computeEnvironments :: DescribeComputeEnvironments -> Lude.Maybe [Lude.Text]) (\s a -> s {computeEnvironments = a} :: DescribeComputeEnvironments)
{-# DEPRECATED dceComputeEnvironments "Use generic-lens or generic-optics with 'computeEnvironments' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeComputeEnvironments@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceNextToken :: Lens.Lens' DescribeComputeEnvironments (Lude.Maybe Lude.Text)
dceNextToken = Lens.lens (nextToken :: DescribeComputeEnvironments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeComputeEnvironments)
{-# DEPRECATED dceNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of cluster results returned by @DescribeComputeEnvironments@ in paginated output. When this parameter is used, @DescribeComputeEnvironments@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeComputeEnvironments@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeComputeEnvironments@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceMaxResults :: Lens.Lens' DescribeComputeEnvironments (Lude.Maybe Lude.Int)
dceMaxResults = Lens.lens (maxResults :: DescribeComputeEnvironments -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeComputeEnvironments)
{-# DEPRECATED dceMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeComputeEnvironments where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsComputeEnvironments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dceNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeComputeEnvironments where
  type
    Rs DescribeComputeEnvironments =
      DescribeComputeEnvironmentsResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeComputeEnvironmentsResponse'
            Lude.<$> (x Lude..?> "computeEnvironments" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeComputeEnvironments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeComputeEnvironments where
  toJSON DescribeComputeEnvironments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("computeEnvironments" Lude..=) Lude.<$> computeEnvironments,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeComputeEnvironments where
  toPath = Lude.const "/v1/describecomputeenvironments"

instance Lude.ToQuery DescribeComputeEnvironments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeComputeEnvironmentsResponse' smart constructor.
data DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse'
  { computeEnvironments ::
      Lude.Maybe
        [ComputeEnvironmentDetail],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'DescribeComputeEnvironmentsResponse' with the minimum fields required to make a request.
--
-- * 'computeEnvironments' - The list of compute environments.
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeComputeEnvironments@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeComputeEnvironmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeComputeEnvironmentsResponse
mkDescribeComputeEnvironmentsResponse pResponseStatus_ =
  DescribeComputeEnvironmentsResponse'
    { computeEnvironments =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of compute environments.
--
-- /Note:/ Consider using 'computeEnvironments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsComputeEnvironments :: Lens.Lens' DescribeComputeEnvironmentsResponse (Lude.Maybe [ComputeEnvironmentDetail])
drsComputeEnvironments = Lens.lens (computeEnvironments :: DescribeComputeEnvironmentsResponse -> Lude.Maybe [ComputeEnvironmentDetail]) (\s a -> s {computeEnvironments = a} :: DescribeComputeEnvironmentsResponse)
{-# DEPRECATED drsComputeEnvironments "Use generic-lens or generic-optics with 'computeEnvironments' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeComputeEnvironments@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeComputeEnvironmentsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeComputeEnvironmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeComputeEnvironmentsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeComputeEnvironmentsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeComputeEnvironmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeComputeEnvironmentsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
