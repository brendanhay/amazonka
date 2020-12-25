{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.
--
-- At least one event ARN is required. Results are sorted by the @lastUpdatedTime@ of the entity, starting with the most recent.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntities
  ( -- * Creating a request
    DescribeAffectedEntities (..),
    mkDescribeAffectedEntities,

    -- ** Request lenses
    daeFilter,
    daeLocale,
    daeMaxResults,
    daeNextToken,

    -- * Destructuring the response
    DescribeAffectedEntitiesResponse (..),
    mkDescribeAffectedEntitiesResponse,

    -- ** Response lenses
    daerrsEntities,
    daerrsNextToken,
    daerrsResponseStatus,
  )
where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAffectedEntities' smart constructor.
data DescribeAffectedEntities = DescribeAffectedEntities'
  { -- | Values to narrow the results returned. At least one event ARN is required.
    filter :: Types.EntityFilter,
    -- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
    locale :: Core.Maybe Types.Locale,
    -- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAffectedEntities' value with any optional fields omitted.
mkDescribeAffectedEntities ::
  -- | 'filter'
  Types.EntityFilter ->
  DescribeAffectedEntities
mkDescribeAffectedEntities filter =
  DescribeAffectedEntities'
    { filter,
      locale = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Values to narrow the results returned. At least one event ARN is required.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeFilter :: Lens.Lens' DescribeAffectedEntities Types.EntityFilter
daeFilter = Lens.field @"filter"
{-# DEPRECATED daeFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeLocale :: Lens.Lens' DescribeAffectedEntities (Core.Maybe Types.Locale)
daeLocale = Lens.field @"locale"
{-# DEPRECATED daeLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeMaxResults :: Lens.Lens' DescribeAffectedEntities (Core.Maybe Core.Natural)
daeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED daeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeNextToken :: Lens.Lens' DescribeAffectedEntities (Core.Maybe Types.NextToken)
daeNextToken = Lens.field @"nextToken"
{-# DEPRECATED daeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAffectedEntities where
  toJSON DescribeAffectedEntities {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("filter" Core..= filter),
            ("locale" Core..=) Core.<$> locale,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAffectedEntities where
  type Rs DescribeAffectedEntities = DescribeAffectedEntitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSHealth_20160804.DescribeAffectedEntities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesResponse'
            Core.<$> (x Core..:? "entities")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAffectedEntities where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"entities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAffectedEntitiesResponse' smart constructor.
data DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse'
  { -- | The entities that match the filter criteria.
    entities :: Core.Maybe [Types.AffectedEntity],
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAffectedEntitiesResponse' value with any optional fields omitted.
mkDescribeAffectedEntitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAffectedEntitiesResponse
mkDescribeAffectedEntitiesResponse responseStatus =
  DescribeAffectedEntitiesResponse'
    { entities = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The entities that match the filter criteria.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerrsEntities :: Lens.Lens' DescribeAffectedEntitiesResponse (Core.Maybe [Types.AffectedEntity])
daerrsEntities = Lens.field @"entities"
{-# DEPRECATED daerrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerrsNextToken :: Lens.Lens' DescribeAffectedEntitiesResponse (Core.Maybe Types.NextToken)
daerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerrsResponseStatus :: Lens.Lens' DescribeAffectedEntitiesResponse Core.Int
daerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
