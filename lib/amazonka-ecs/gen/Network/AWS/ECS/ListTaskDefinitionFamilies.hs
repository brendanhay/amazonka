{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.ListTaskDefinitionFamilies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definition families that are registered to your account (which may include task definition families that no longer have any @ACTIVE@ task definition revisions).
--
-- You can filter out task definition families that do not contain any @ACTIVE@ task definition revisions by setting the @status@ parameter to @ACTIVE@ . You can also filter the results with the @familyPrefix@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListTaskDefinitionFamilies
  ( -- * Creating a request
    ListTaskDefinitionFamilies (..),
    mkListTaskDefinitionFamilies,

    -- ** Request lenses
    ltdfStatus,
    ltdfFamilyPrefix,
    ltdfNextToken,
    ltdfMaxResults,

    -- * Destructuring the response
    ListTaskDefinitionFamiliesResponse (..),
    mkListTaskDefinitionFamiliesResponse,

    -- ** Response lenses
    ltdfrsFamilies,
    ltdfrsNextToken,
    ltdfrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTaskDefinitionFamilies' smart constructor.
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'
  { -- | The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
    status :: Lude.Maybe TaskDefinitionFamilyStatus,
    -- | The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
    familyPrefix :: Lude.Maybe Lude.Text,
    -- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTaskDefinitionFamilies' with the minimum fields required to make a request.
--
-- * 'status' - The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
-- * 'familyPrefix' - The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
-- * 'nextToken' - The @nextToken@ value returned from a @ListTaskDefinitionFamilies@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
-- * 'maxResults' - The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
mkListTaskDefinitionFamilies ::
  ListTaskDefinitionFamilies
mkListTaskDefinitionFamilies =
  ListTaskDefinitionFamilies'
    { status = Lude.Nothing,
      familyPrefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfStatus :: Lens.Lens' ListTaskDefinitionFamilies (Lude.Maybe TaskDefinitionFamilyStatus)
ltdfStatus = Lens.lens (status :: ListTaskDefinitionFamilies -> Lude.Maybe TaskDefinitionFamilyStatus) (\s a -> s {status = a} :: ListTaskDefinitionFamilies)
{-# DEPRECATED ltdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
--
-- /Note:/ Consider using 'familyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfFamilyPrefix :: Lens.Lens' ListTaskDefinitionFamilies (Lude.Maybe Lude.Text)
ltdfFamilyPrefix = Lens.lens (familyPrefix :: ListTaskDefinitionFamilies -> Lude.Maybe Lude.Text) (\s a -> s {familyPrefix = a} :: ListTaskDefinitionFamilies)
{-# DEPRECATED ltdfFamilyPrefix "Use generic-lens or generic-optics with 'familyPrefix' instead." #-}

-- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfNextToken :: Lens.Lens' ListTaskDefinitionFamilies (Lude.Maybe Lude.Text)
ltdfNextToken = Lens.lens (nextToken :: ListTaskDefinitionFamilies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTaskDefinitionFamilies)
{-# DEPRECATED ltdfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfMaxResults :: Lens.Lens' ListTaskDefinitionFamilies (Lude.Maybe Lude.Int)
ltdfMaxResults = Lens.lens (maxResults :: ListTaskDefinitionFamilies -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListTaskDefinitionFamilies)
{-# DEPRECATED ltdfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTaskDefinitionFamilies where
  page rq rs
    | Page.stop (rs Lens.^. ltdfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltdfrsFamilies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltdfNextToken Lens..~ rs Lens.^. ltdfrsNextToken

instance Lude.AWSRequest ListTaskDefinitionFamilies where
  type
    Rs ListTaskDefinitionFamilies =
      ListTaskDefinitionFamiliesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTaskDefinitionFamiliesResponse'
            Lude.<$> (x Lude..?> "families" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTaskDefinitionFamilies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.ListTaskDefinitionFamilies" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTaskDefinitionFamilies where
  toJSON ListTaskDefinitionFamilies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("familyPrefix" Lude..=) Lude.<$> familyPrefix,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTaskDefinitionFamilies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTaskDefinitionFamilies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTaskDefinitionFamiliesResponse' smart constructor.
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'
  { -- | The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
    families :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTaskDefinitionFamiliesResponse' with the minimum fields required to make a request.
--
-- * 'families' - The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
-- * 'nextToken' - The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkListTaskDefinitionFamiliesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTaskDefinitionFamiliesResponse
mkListTaskDefinitionFamiliesResponse pResponseStatus_ =
  ListTaskDefinitionFamiliesResponse'
    { families = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
--
-- /Note:/ Consider using 'families' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfrsFamilies :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Lude.Maybe [Lude.Text])
ltdfrsFamilies = Lens.lens (families :: ListTaskDefinitionFamiliesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {families = a} :: ListTaskDefinitionFamiliesResponse)
{-# DEPRECATED ltdfrsFamilies "Use generic-lens or generic-optics with 'families' instead." #-}

-- | The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfrsNextToken :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Lude.Maybe Lude.Text)
ltdfrsNextToken = Lens.lens (nextToken :: ListTaskDefinitionFamiliesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTaskDefinitionFamiliesResponse)
{-# DEPRECATED ltdfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfrsResponseStatus :: Lens.Lens' ListTaskDefinitionFamiliesResponse Lude.Int
ltdfrsResponseStatus = Lens.lens (responseStatus :: ListTaskDefinitionFamiliesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTaskDefinitionFamiliesResponse)
{-# DEPRECATED ltdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
