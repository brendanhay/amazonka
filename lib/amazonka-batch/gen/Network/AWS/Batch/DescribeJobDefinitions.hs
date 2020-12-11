{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DescribeJobDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of job definitions. You can specify a @status@ (such as @ACTIVE@ ) to only return job definitions that match that status.
--
-- This operation returns paginated results.
module Network.AWS.Batch.DescribeJobDefinitions
  ( -- * Creating a request
    DescribeJobDefinitions (..),
    mkDescribeJobDefinitions,

    -- ** Request lenses
    djdStatus,
    djdJobDefinitionName,
    djdJobDefinitions,
    djdNextToken,
    djdMaxResults,

    -- * Destructuring the response
    DescribeJobDefinitionsResponse (..),
    mkDescribeJobDefinitionsResponse,

    -- ** Response lenses
    djdrsJobDefinitions,
    djdrsNextToken,
    djdrsResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeJobDefinitions' smart constructor.
data DescribeJobDefinitions = DescribeJobDefinitions'
  { status ::
      Lude.Maybe Lude.Text,
    jobDefinitionName :: Lude.Maybe Lude.Text,
    jobDefinitions :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'DescribeJobDefinitions' with the minimum fields required to make a request.
--
-- * 'jobDefinitionName' - The name of the job definition to describe.
-- * 'jobDefinitions' - A list of up to 100 job definition names or full Amazon Resource Name (ARN) entries.
-- * 'maxResults' - The maximum number of results returned by @DescribeJobDefinitions@ in paginated output. When this parameter is used, @DescribeJobDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeJobDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
-- * 'status' - The status with which to filter job definitions.
mkDescribeJobDefinitions ::
  DescribeJobDefinitions
mkDescribeJobDefinitions =
  DescribeJobDefinitions'
    { status = Lude.Nothing,
      jobDefinitionName = Lude.Nothing,
      jobDefinitions = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The status with which to filter job definitions.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdStatus :: Lens.Lens' DescribeJobDefinitions (Lude.Maybe Lude.Text)
djdStatus = Lens.lens (status :: DescribeJobDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DescribeJobDefinitions)
{-# DEPRECATED djdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the job definition to describe.
--
-- /Note:/ Consider using 'jobDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdJobDefinitionName :: Lens.Lens' DescribeJobDefinitions (Lude.Maybe Lude.Text)
djdJobDefinitionName = Lens.lens (jobDefinitionName :: DescribeJobDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {jobDefinitionName = a} :: DescribeJobDefinitions)
{-# DEPRECATED djdJobDefinitionName "Use generic-lens or generic-optics with 'jobDefinitionName' instead." #-}

-- | A list of up to 100 job definition names or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'jobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdJobDefinitions :: Lens.Lens' DescribeJobDefinitions (Lude.Maybe [Lude.Text])
djdJobDefinitions = Lens.lens (jobDefinitions :: DescribeJobDefinitions -> Lude.Maybe [Lude.Text]) (\s a -> s {jobDefinitions = a} :: DescribeJobDefinitions)
{-# DEPRECATED djdJobDefinitions "Use generic-lens or generic-optics with 'jobDefinitions' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeJobDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdNextToken :: Lens.Lens' DescribeJobDefinitions (Lude.Maybe Lude.Text)
djdNextToken = Lens.lens (nextToken :: DescribeJobDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeJobDefinitions)
{-# DEPRECATED djdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results returned by @DescribeJobDefinitions@ in paginated output. When this parameter is used, @DescribeJobDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdMaxResults :: Lens.Lens' DescribeJobDefinitions (Lude.Maybe Lude.Int)
djdMaxResults = Lens.lens (maxResults :: DescribeJobDefinitions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeJobDefinitions)
{-# DEPRECATED djdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeJobDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. djdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. djdrsJobDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& djdNextToken Lens..~ rs Lens.^. djdrsNextToken

instance Lude.AWSRequest DescribeJobDefinitions where
  type Rs DescribeJobDefinitions = DescribeJobDefinitionsResponse
  request = Req.postJSON batchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeJobDefinitionsResponse'
            Lude.<$> (x Lude..?> "jobDefinitions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeJobDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeJobDefinitions where
  toJSON DescribeJobDefinitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("jobDefinitionName" Lude..=) Lude.<$> jobDefinitionName,
            ("jobDefinitions" Lude..=) Lude.<$> jobDefinitions,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeJobDefinitions where
  toPath = Lude.const "/v1/describejobdefinitions"

instance Lude.ToQuery DescribeJobDefinitions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeJobDefinitionsResponse' smart constructor.
data DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse'
  { jobDefinitions ::
      Lude.Maybe [JobDefinition],
    nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'jobDefinitions' - The list of job definitions.
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeJobDefinitions@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeJobDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeJobDefinitionsResponse
mkDescribeJobDefinitionsResponse pResponseStatus_ =
  DescribeJobDefinitionsResponse'
    { jobDefinitions = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of job definitions.
--
-- /Note:/ Consider using 'jobDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrsJobDefinitions :: Lens.Lens' DescribeJobDefinitionsResponse (Lude.Maybe [JobDefinition])
djdrsJobDefinitions = Lens.lens (jobDefinitions :: DescribeJobDefinitionsResponse -> Lude.Maybe [JobDefinition]) (\s a -> s {jobDefinitions = a} :: DescribeJobDefinitionsResponse)
{-# DEPRECATED djdrsJobDefinitions "Use generic-lens or generic-optics with 'jobDefinitions' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeJobDefinitions@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrsNextToken :: Lens.Lens' DescribeJobDefinitionsResponse (Lude.Maybe Lude.Text)
djdrsNextToken = Lens.lens (nextToken :: DescribeJobDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeJobDefinitionsResponse)
{-# DEPRECATED djdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djdrsResponseStatus :: Lens.Lens' DescribeJobDefinitionsResponse Lude.Int
djdrsResponseStatus = Lens.lens (responseStatus :: DescribeJobDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeJobDefinitionsResponse)
{-# DEPRECATED djdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
