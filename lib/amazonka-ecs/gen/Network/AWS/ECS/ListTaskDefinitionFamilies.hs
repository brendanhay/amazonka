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
    ltdfFamilyPrefix,
    ltdfMaxResults,
    ltdfNextToken,
    ltdfStatus,

    -- * Destructuring the response
    ListTaskDefinitionFamiliesResponse (..),
    mkListTaskDefinitionFamiliesResponse,

    -- ** Response lenses
    ltdfrrsFamilies,
    ltdfrrsNextToken,
    ltdfrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTaskDefinitionFamilies' smart constructor.
data ListTaskDefinitionFamilies = ListTaskDefinitionFamilies'
  { -- | The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
    familyPrefix :: Core.Maybe Types.String,
    -- | The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
    nextToken :: Core.Maybe Types.String,
    -- | The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
    status :: Core.Maybe Types.TaskDefinitionFamilyStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTaskDefinitionFamilies' value with any optional fields omitted.
mkListTaskDefinitionFamilies ::
  ListTaskDefinitionFamilies
mkListTaskDefinitionFamilies =
  ListTaskDefinitionFamilies'
    { familyPrefix = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | The @familyPrefix@ is a string that is used to filter the results of @ListTaskDefinitionFamilies@ . If you specify a @familyPrefix@ , only task definition family names that begin with the @familyPrefix@ string are returned.
--
-- /Note:/ Consider using 'familyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfFamilyPrefix :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Types.String)
ltdfFamilyPrefix = Lens.field @"familyPrefix"
{-# DEPRECATED ltdfFamilyPrefix "Use generic-lens or generic-optics with 'familyPrefix' instead." #-}

-- | The maximum number of task definition family results returned by @ListTaskDefinitionFamilies@ in paginated output. When this parameter is used, @ListTaskDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListTaskDefinitionFamilies@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListTaskDefinitionFamilies@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfMaxResults :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Core.Int)
ltdfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltdfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a @ListTaskDefinitionFamilies@ request indicating that more results are available to fulfill the request and further calls will be needed. If @maxResults@ was provided, it is possible the number of results to be fewer than @maxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfNextToken :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Types.String)
ltdfNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltdfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The task definition family status with which to filter the @ListTaskDefinitionFamilies@ results. By default, both @ACTIVE@ and @INACTIVE@ task definition families are listed. If this parameter is set to @ACTIVE@ , only task definition families that have an @ACTIVE@ task definition revision are returned. If this parameter is set to @INACTIVE@ , only task definition families that do not have any @ACTIVE@ task definition revisions are returned. If you paginate the resulting output, be sure to keep the @status@ value constant in each subsequent request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfStatus :: Lens.Lens' ListTaskDefinitionFamilies (Core.Maybe Types.TaskDefinitionFamilyStatus)
ltdfStatus = Lens.field @"status"
{-# DEPRECATED ltdfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ListTaskDefinitionFamilies where
  toJSON ListTaskDefinitionFamilies {..} =
    Core.object
      ( Core.catMaybes
          [ ("familyPrefix" Core..=) Core.<$> familyPrefix,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest ListTaskDefinitionFamilies where
  type
    Rs ListTaskDefinitionFamilies =
      ListTaskDefinitionFamiliesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.ListTaskDefinitionFamilies"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskDefinitionFamiliesResponse'
            Core.<$> (x Core..:? "families")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTaskDefinitionFamilies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"families" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTaskDefinitionFamiliesResponse' smart constructor.
data ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse'
  { -- | The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
    families :: Core.Maybe [Types.String],
    -- | The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTaskDefinitionFamiliesResponse' value with any optional fields omitted.
mkListTaskDefinitionFamiliesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTaskDefinitionFamiliesResponse
mkListTaskDefinitionFamiliesResponse responseStatus =
  ListTaskDefinitionFamiliesResponse'
    { families = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of task definition family names that match the @ListTaskDefinitionFamilies@ request.
--
-- /Note:/ Consider using 'families' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfrrsFamilies :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Core.Maybe [Types.String])
ltdfrrsFamilies = Lens.field @"families"
{-# DEPRECATED ltdfrrsFamilies "Use generic-lens or generic-optics with 'families' instead." #-}

-- | The @nextToken@ value to include in a future @ListTaskDefinitionFamilies@ request. When the results of a @ListTaskDefinitionFamilies@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfrrsNextToken :: Lens.Lens' ListTaskDefinitionFamiliesResponse (Core.Maybe Types.String)
ltdfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltdfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltdfrrsResponseStatus :: Lens.Lens' ListTaskDefinitionFamiliesResponse Core.Int
ltdfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltdfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
