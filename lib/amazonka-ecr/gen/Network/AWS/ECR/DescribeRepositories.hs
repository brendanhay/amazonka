{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.DescribeRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes image repositories in a registry.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeRepositories
  ( -- * Creating a request
    DescribeRepositories (..),
    mkDescribeRepositories,

    -- ** Request lenses
    drRegistryId,
    drRepositoryNames,
    drNextToken,
    drMaxResults,

    -- * Destructuring the response
    DescribeRepositoriesResponse (..),
    mkDescribeRepositoriesResponse,

    -- ** Response lenses
    drrsRepositories,
    drrsNextToken,
    drrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRepositories' smart constructor.
data DescribeRepositories = DescribeRepositories'
  { -- | The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
    registryId :: Lude.Maybe Lude.Text,
    -- | A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
    repositoryNames :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRepositories' with the minimum fields required to make a request.
--
-- * 'registryId' - The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
-- * 'repositoryNames' - A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
-- * 'maxResults' - The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
mkDescribeRepositories ::
  DescribeRepositories
mkDescribeRepositories =
  DescribeRepositories'
    { registryId = Lude.Nothing,
      repositoryNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRegistryId :: Lens.Lens' DescribeRepositories (Lude.Maybe Lude.Text)
drRegistryId = Lens.lens (registryId :: DescribeRepositories -> Lude.Maybe Lude.Text) (\s a -> s {registryId = a} :: DescribeRepositories)
{-# DEPRECATED drRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRepositoryNames :: Lens.Lens' DescribeRepositories (Lude.Maybe (Lude.NonEmpty Lude.Text))
drRepositoryNames = Lens.lens (repositoryNames :: DescribeRepositories -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {repositoryNames = a} :: DescribeRepositories)
{-# DEPRECATED drRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drNextToken :: Lens.Lens' DescribeRepositories (Lude.Maybe Lude.Text)
drNextToken = Lens.lens (nextToken :: DescribeRepositories -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRepositories)
{-# DEPRECATED drNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMaxResults :: Lens.Lens' DescribeRepositories (Lude.Maybe Lude.Natural)
drMaxResults = Lens.lens (maxResults :: DescribeRepositories -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeRepositories)
{-# DEPRECATED drMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeRepositories where
  page rq rs
    | Page.stop (rs Lens.^. drrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drrsRepositories) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drNextToken Lens..~ rs Lens.^. drrsNextToken

instance Lude.AWSRequest DescribeRepositories where
  type Rs DescribeRepositories = DescribeRepositoriesResponse
  request = Req.postJSON ecrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRepositoriesResponse'
            Lude.<$> (x Lude..?> "repositories" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRepositories where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeRepositories" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRepositories where
  toJSON DescribeRepositories' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("registryId" Lude..=) Lude.<$> registryId,
            ("repositoryNames" Lude..=) Lude.<$> repositoryNames,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeRepositories where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRepositories where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRepositoriesResponse' smart constructor.
data DescribeRepositoriesResponse = DescribeRepositoriesResponse'
  { -- | A list of repository objects corresponding to valid repositories.
    repositories :: Lude.Maybe [Repository],
    -- | The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRepositoriesResponse' with the minimum fields required to make a request.
--
-- * 'repositories' - A list of repository objects corresponding to valid repositories.
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeRepositoriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRepositoriesResponse
mkDescribeRepositoriesResponse pResponseStatus_ =
  DescribeRepositoriesResponse'
    { repositories = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of repository objects corresponding to valid repositories.
--
-- /Note:/ Consider using 'repositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRepositories :: Lens.Lens' DescribeRepositoriesResponse (Lude.Maybe [Repository])
drrsRepositories = Lens.lens (repositories :: DescribeRepositoriesResponse -> Lude.Maybe [Repository]) (\s a -> s {repositories = a} :: DescribeRepositoriesResponse)
{-# DEPRECATED drrsRepositories "Use generic-lens or generic-optics with 'repositories' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsNextToken :: Lens.Lens' DescribeRepositoriesResponse (Lude.Maybe Lude.Text)
drrsNextToken = Lens.lens (nextToken :: DescribeRepositoriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRepositoriesResponse)
{-# DEPRECATED drrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DescribeRepositoriesResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DescribeRepositoriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRepositoriesResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
