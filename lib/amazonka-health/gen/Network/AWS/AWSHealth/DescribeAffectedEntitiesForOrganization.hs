{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of entities that have been affected by one or more events for one or more accounts in your organization in AWS Organizations, based on the filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service.
--
-- At least one event Amazon Resource Name (ARN) and account ID are required. Results are sorted by the @lastUpdatedTime@ of the entity, starting with the most recent.
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master account.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeAffectedEntitiesForOrganization
  ( -- * Creating a request
    DescribeAffectedEntitiesForOrganization (..),
    mkDescribeAffectedEntitiesForOrganization,

    -- ** Request lenses
    daefoOrganizationEntityFilters,
    daefoLocale,
    daefoMaxResults,
    daefoNextToken,

    -- * Destructuring the response
    DescribeAffectedEntitiesForOrganizationResponse (..),
    mkDescribeAffectedEntitiesForOrganizationResponse,

    -- ** Response lenses
    daeforrsEntities,
    daeforrsFailedSet,
    daeforrsNextToken,
    daeforrsResponseStatus,
  )
where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAffectedEntitiesForOrganization' smart constructor.
data DescribeAffectedEntitiesForOrganization = DescribeAffectedEntitiesForOrganization'
  { -- | A JSON set of elements including the @awsAccountId@ and the @eventArn@ .
    organizationEntityFilters :: Core.NonEmpty Types.EventAccountFilter,
    -- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
    locale :: Core.Maybe Types.Locale,
    -- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAffectedEntitiesForOrganization' value with any optional fields omitted.
mkDescribeAffectedEntitiesForOrganization ::
  -- | 'organizationEntityFilters'
  Core.NonEmpty Types.EventAccountFilter ->
  DescribeAffectedEntitiesForOrganization
mkDescribeAffectedEntitiesForOrganization organizationEntityFilters =
  DescribeAffectedEntitiesForOrganization'
    { organizationEntityFilters,
      locale = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A JSON set of elements including the @awsAccountId@ and the @eventArn@ .
--
-- /Note:/ Consider using 'organizationEntityFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoOrganizationEntityFilters :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Core.NonEmpty Types.EventAccountFilter)
daefoOrganizationEntityFilters = Lens.field @"organizationEntityFilters"
{-# DEPRECATED daefoOrganizationEntityFilters "Use generic-lens or generic-optics with 'organizationEntityFilters' instead." #-}

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoLocale :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Core.Maybe Types.Locale)
daefoLocale = Lens.field @"locale"
{-# DEPRECATED daefoLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoMaxResults :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Core.Maybe Core.Natural)
daefoMaxResults = Lens.field @"maxResults"
{-# DEPRECATED daefoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daefoNextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganization (Core.Maybe Types.NextToken)
daefoNextToken = Lens.field @"nextToken"
{-# DEPRECATED daefoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeAffectedEntitiesForOrganization where
  toJSON DescribeAffectedEntitiesForOrganization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("organizationEntityFilters" Core..= organizationEntityFilters),
            ("locale" Core..=) Core.<$> locale,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeAffectedEntitiesForOrganization where
  type
    Rs DescribeAffectedEntitiesForOrganization =
      DescribeAffectedEntitiesForOrganizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSHealth_20160804.DescribeAffectedEntitiesForOrganization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAffectedEntitiesForOrganizationResponse'
            Core.<$> (x Core..:? "entities")
            Core.<*> (x Core..:? "failedSet")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAffectedEntitiesForOrganization where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"entities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAffectedEntitiesForOrganizationResponse' smart constructor.
data DescribeAffectedEntitiesForOrganizationResponse = DescribeAffectedEntitiesForOrganizationResponse'
  { -- | A JSON set of elements including the @awsAccountId@ and its @entityArn@ , @entityValue@ and its @entityArn@ , @lastUpdatedTime@ , and @statusCode@ .
    entities :: Core.Maybe [Types.AffectedEntity],
    -- | A JSON set of elements of the failed response, including the @awsAccountId@ , @errorMessage@ , @errorName@ , and @eventArn@ .
    failedSet :: Core.Maybe [Types.OrganizationAffectedEntitiesErrorItem],
    -- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAffectedEntitiesForOrganizationResponse' value with any optional fields omitted.
mkDescribeAffectedEntitiesForOrganizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAffectedEntitiesForOrganizationResponse
mkDescribeAffectedEntitiesForOrganizationResponse responseStatus =
  DescribeAffectedEntitiesForOrganizationResponse'
    { entities =
        Core.Nothing,
      failedSet = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A JSON set of elements including the @awsAccountId@ and its @entityArn@ , @entityValue@ and its @entityArn@ , @lastUpdatedTime@ , and @statusCode@ .
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforrsEntities :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Core.Maybe [Types.AffectedEntity])
daeforrsEntities = Lens.field @"entities"
{-# DEPRECATED daeforrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | A JSON set of elements of the failed response, including the @awsAccountId@ , @errorMessage@ , @errorName@ , and @eventArn@ .
--
-- /Note:/ Consider using 'failedSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforrsFailedSet :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Core.Maybe [Types.OrganizationAffectedEntitiesErrorItem])
daeforrsFailedSet = Lens.field @"failedSet"
{-# DEPRECATED daeforrsFailedSet "Use generic-lens or generic-optics with 'failedSet' instead." #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforrsNextToken :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse (Core.Maybe Types.NextToken)
daeforrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED daeforrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeforrsResponseStatus :: Lens.Lens' DescribeAffectedEntitiesForOrganizationResponse Core.Int
daeforrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daeforrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
