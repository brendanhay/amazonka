{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definitions that are registered to your account. You can filter the results by family name with the @familyPrefix@ parameter or by status with the @status@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTaskDefinitions
  ( -- * Creating a request
    ListTaskDefinitions (..),
    mkListTaskDefinitions,

    -- ** Request lenses
    ltdStatus,
    ltdFamilyPrefix,
    ltdNextToken,
    ltdSort,
    ltdMaxResults,

    -- * Destructuring the response
    ListTaskDefinitionsResponse (..),
    mkListTaskDefinitionsResponse,

    -- ** Response lenses
    ltdrsTaskDefinitionARNs,
    ltdrsNextToken,
    ltdrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTaskDefinitions' smart constructor.
data ListTaskDefinitions = ListTaskDefinitions'
  { -- | The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
    status :: Lude.Maybe TaskDefinitionStatus,
    -- | The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
    familyPrefix :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a @ListTaskDefinitions@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
    sort :: Lude.Maybe SortOrder,
    -- | The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTaskDefinitions' with the minimum fields required to make a request.
--
-- * 'status' - The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
-- * 'familyPrefix' - The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
-- * 'nextToken' - The @nextToken@ value returned from a @ListTaskDefinitions@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'sort' - The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
-- * 'maxResults' - The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
mkListTaskDefinitions ::
  ListTaskDefinitions
mkListTaskDefinitions =
  ListTaskDefinitions'
    { status = Lude.Nothing,
      familyPrefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      sort = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The task definition status with which to filter the @ListTaskDefinitions@ results. By default, only @ACTIVE@ task definitions are listed. By setting this parameter to @INACTIVE@ , you can view task definitions that are @INACTIVE@ as long as an active task or service still references them. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdStatus :: Lens.Lens' ListTaskDefinitions (Lude.Maybe TaskDefinitionStatus)
ltdStatus = Lens.lens (status :: ListTaskDefinitions -> Lude.Maybe TaskDefinitionStatus) (\s a -> s {status = a} :: ListTaskDefinitions)
{-# DEPRECATED ltdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The full family name with which to filter the @ListTaskDefinitions@ results. Specifying a @familyPrefix@ limits the listed task definitions to task definition revisions that belong to that family.
--
-- /Note:/ Consider using 'familyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdFamilyPrefix :: Lens.Lens' ListTaskDefinitions (Lude.Maybe Lude.Text)
ltdFamilyPrefix = Lens.lens (familyPrefix :: ListTaskDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {familyPrefix = a} :: ListTaskDefinitions)
{-# DEPRECATED ltdFamilyPrefix "Use generic-lens or generic-optics with 'familyPrefix' instead." #-}

-- | The @nextToken@ value returned from a @ListTaskDefinitions@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdNextToken :: Lens.Lens' ListTaskDefinitions (Lude.Maybe Lude.Text)
ltdNextToken = Lens.lens (nextToken :: ListTaskDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTaskDefinitions)
{-# DEPRECATED ltdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The order in which to sort the results. Valid values are @ASC@ and @DESC@ . By default (@ASC@ ), task definitions are listed lexicographically by family name and in ascending numerical order by revision so that the newest task definitions in a family are listed last. Setting this parameter to @DESC@ reverses the sort order on family name and revision so that the newest task definitions in a family are listed first.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdSort :: Lens.Lens' ListTaskDefinitions (Lude.Maybe SortOrder)
ltdSort = Lens.lens (sort :: ListTaskDefinitions -> Lude.Maybe SortOrder) (\s a -> s {sort = a} :: ListTaskDefinitions)
{-# DEPRECATED ltdSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The maximum number of task definition results returned by @ListTaskDefinitions@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdMaxResults :: Lens.Lens' ListTaskDefinitions (Lude.Maybe Lude.Int)
ltdMaxResults = Lens.lens (maxResults :: ListTaskDefinitions -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListTaskDefinitions)
{-# DEPRECATED ltdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTaskDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. ltdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltdrsTaskDefinitionARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltdNextToken Lens..~ rs Lens.^. ltdrsNextToken

instance Lude.AWSRequest ListTaskDefinitions where
  type Rs ListTaskDefinitions = ListTaskDefinitionsResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTaskDefinitionsResponse'
            Lude.<$> (x Lude..?> "taskDefinitionArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTaskDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListTaskDefinitions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTaskDefinitions where
  toJSON ListTaskDefinitions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("familyPrefix" Lude..=) Lude.<$> familyPrefix,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("sort" Lude..=) Lude.<$> sort,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTaskDefinitions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTaskDefinitions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTaskDefinitionsResponse' smart constructor.
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'
  { -- | The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
    taskDefinitionARNs :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTaskDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'taskDefinitionARNs' - The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListTaskDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTaskDefinitionsResponse
mkListTaskDefinitionsResponse pResponseStatus_ =
  ListTaskDefinitionsResponse'
    { taskDefinitionARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of task definition Amazon Resource Name (ARN) entries for the @ListTaskDefinitions@ request.
--
-- /Note:/ Consider using 'taskDefinitionARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdrsTaskDefinitionARNs :: Lens.Lens' ListTaskDefinitionsResponse (Lude.Maybe [Lude.Text])
ltdrsTaskDefinitionARNs = Lens.lens (taskDefinitionARNs :: ListTaskDefinitionsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {taskDefinitionARNs = a} :: ListTaskDefinitionsResponse)
{-# DEPRECATED ltdrsTaskDefinitionARNs "Use generic-lens or generic-optics with 'taskDefinitionARNs' instead." #-}

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@ request. When the results of a @ListTaskDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdrsNextToken :: Lens.Lens' ListTaskDefinitionsResponse (Lude.Maybe Lude.Text)
ltdrsNextToken = Lens.lens (nextToken :: ListTaskDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTaskDefinitionsResponse)
{-# DEPRECATED ltdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdrsResponseStatus :: Lens.Lens' ListTaskDefinitionsResponse Lude.Int
ltdrsResponseStatus = Lens.lens (responseStatus :: ListTaskDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTaskDefinitionsResponse)
{-# DEPRECATED ltdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
