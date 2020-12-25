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
    drMaxResults,
    drNextToken,
    drRegistryId,
    drRepositoryNames,

    -- * Destructuring the response
    DescribeRepositoriesResponse (..),
    mkDescribeRepositoriesResponse,

    -- ** Response lenses
    drrrsNextToken,
    drrrsRepositories,
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.ECR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRepositories' smart constructor.
data DescribeRepositories = DescribeRepositories'
  { -- | The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
    maxResults :: Core.Maybe Core.Natural,
    -- | The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
    registryId :: Core.Maybe Types.RegistryId,
    -- | A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
    repositoryNames :: Core.Maybe (Core.NonEmpty Types.RepositoryName)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRepositories' value with any optional fields omitted.
mkDescribeRepositories ::
  DescribeRepositories
mkDescribeRepositories =
  DescribeRepositories'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      registryId = Core.Nothing,
      repositoryNames = Core.Nothing
    }

-- | The maximum number of repository results returned by @DescribeRepositories@ in paginated output. When this parameter is used, @DescribeRepositories@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeRepositories@ request with the returned @nextToken@ value. This value can be between 1 and 1000. If this parameter is not used, then @DescribeRepositories@ returns up to 100 results and a @nextToken@ value, if applicable. This option cannot be used when you specify repositories with @repositoryNames@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMaxResults :: Lens.Lens' DescribeRepositories (Core.Maybe Core.Natural)
drMaxResults = Lens.field @"maxResults"
{-# DEPRECATED drMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeRepositories@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return. This option cannot be used when you specify repositories with @repositoryNames@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drNextToken :: Lens.Lens' DescribeRepositories (Core.Maybe Types.NextToken)
drNextToken = Lens.field @"nextToken"
{-# DEPRECATED drNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The AWS account ID associated with the registry that contains the repositories to be described. If you do not specify a registry, the default registry is assumed.
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRegistryId :: Lens.Lens' DescribeRepositories (Core.Maybe Types.RegistryId)
drRegistryId = Lens.field @"registryId"
{-# DEPRECATED drRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A list of repositories to describe. If this parameter is omitted, then all repositories in a registry are described.
--
-- /Note:/ Consider using 'repositoryNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRepositoryNames :: Lens.Lens' DescribeRepositories (Core.Maybe (Core.NonEmpty Types.RepositoryName))
drRepositoryNames = Lens.field @"repositoryNames"
{-# DEPRECATED drRepositoryNames "Use generic-lens or generic-optics with 'repositoryNames' instead." #-}

instance Core.FromJSON DescribeRepositories where
  toJSON DescribeRepositories {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("registryId" Core..=) Core.<$> registryId,
            ("repositoryNames" Core..=) Core.<$> repositoryNames
          ]
      )

instance Core.AWSRequest DescribeRepositories where
  type Rs DescribeRepositories = DescribeRepositoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerRegistry_V20150921.DescribeRepositories"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRepositoriesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "repositories")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeRepositories where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"repositories" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeRepositoriesResponse' smart constructor.
data DescribeRepositoriesResponse = DescribeRepositoriesResponse'
  { -- | The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of repository objects corresponding to valid repositories.
    repositories :: Core.Maybe [Types.Repository],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeRepositoriesResponse' value with any optional fields omitted.
mkDescribeRepositoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRepositoriesResponse
mkDescribeRepositoriesResponse responseStatus =
  DescribeRepositoriesResponse'
    { nextToken = Core.Nothing,
      repositories = Core.Nothing,
      responseStatus
    }

-- | The @nextToken@ value to include in a future @DescribeRepositories@ request. When the results of a @DescribeRepositories@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsNextToken :: Lens.Lens' DescribeRepositoriesResponse (Core.Maybe Types.NextToken)
drrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of repository objects corresponding to valid repositories.
--
-- /Note:/ Consider using 'repositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRepositories :: Lens.Lens' DescribeRepositoriesResponse (Core.Maybe [Types.Repository])
drrrsRepositories = Lens.field @"repositories"
{-# DEPRECATED drrrsRepositories "Use generic-lens or generic-optics with 'repositories' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeRepositoriesResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
