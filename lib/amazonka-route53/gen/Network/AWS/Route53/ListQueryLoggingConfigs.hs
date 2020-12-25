{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListQueryLoggingConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configurations for DNS query logging that are associated with the current AWS account or the configuration that is associated with a specified hosted zone.
--
-- For more information about DNS query logs, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig> . Additional information, including the format of DNS query logs, appears in <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> in the /Amazon Route 53 Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListQueryLoggingConfigs
  ( -- * Creating a request
    ListQueryLoggingConfigs (..),
    mkListQueryLoggingConfigs,

    -- ** Request lenses
    lqlcHostedZoneId,
    lqlcMaxResults,
    lqlcNextToken,

    -- * Destructuring the response
    ListQueryLoggingConfigsResponse (..),
    mkListQueryLoggingConfigsResponse,

    -- ** Response lenses
    lqlcrrsQueryLoggingConfigs,
    lqlcrrsNextToken,
    lqlcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | /See:/ 'mkListQueryLoggingConfigs' smart constructor.
data ListQueryLoggingConfigs = ListQueryLoggingConfigs'
  { -- | (Optional) If you want to list the query logging configuration that is associated with a hosted zone, specify the ID in @HostedZoneId@ .
    --
    -- If you don't specify a hosted zone ID, @ListQueryLoggingConfigs@ returns all of the configurations that are associated with the current AWS account.
    hostedZoneId :: Core.Maybe Types.ResourceId,
    -- | (Optional) The maximum number of query logging configurations that you want Amazon Route 53 to return in response to the current request. If the current AWS account has more than @MaxResults@ configurations, use the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken> in the response to get the next page of results.
    --
    -- If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 configurations.
    maxResults :: Core.Maybe Types.MaxResults,
    -- | (Optional) If the current AWS account has more than @MaxResults@ query logging configurations, use @NextToken@ to get the second and subsequent pages of results.
    --
    -- For the first @ListQueryLoggingConfigs@ request, omit this value.
    -- For the second and subsequent requests, get the value of @NextToken@ from the previous response and specify that value for @NextToken@ in the request.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueryLoggingConfigs' value with any optional fields omitted.
mkListQueryLoggingConfigs ::
  ListQueryLoggingConfigs
mkListQueryLoggingConfigs =
  ListQueryLoggingConfigs'
    { hostedZoneId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | (Optional) If you want to list the query logging configuration that is associated with a hosted zone, specify the ID in @HostedZoneId@ .
--
-- If you don't specify a hosted zone ID, @ListQueryLoggingConfigs@ returns all of the configurations that are associated with the current AWS account.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcHostedZoneId :: Lens.Lens' ListQueryLoggingConfigs (Core.Maybe Types.ResourceId)
lqlcHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED lqlcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | (Optional) The maximum number of query logging configurations that you want Amazon Route 53 to return in response to the current request. If the current AWS account has more than @MaxResults@ configurations, use the value of <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html#API_ListQueryLoggingConfigs_RequestSyntax NextToken> in the response to get the next page of results.
--
-- If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 configurations.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcMaxResults :: Lens.Lens' ListQueryLoggingConfigs (Core.Maybe Types.MaxResults)
lqlcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lqlcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | (Optional) If the current AWS account has more than @MaxResults@ query logging configurations, use @NextToken@ to get the second and subsequent pages of results.
--
-- For the first @ListQueryLoggingConfigs@ request, omit this value.
-- For the second and subsequent requests, get the value of @NextToken@ from the previous response and specify that value for @NextToken@ in the request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcNextToken :: Lens.Lens' ListQueryLoggingConfigs (Core.Maybe Types.PaginationToken)
lqlcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqlcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListQueryLoggingConfigs where
  type Rs ListQueryLoggingConfigs = ListQueryLoggingConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/queryloggingconfig",
        Core._rqQuery =
          Core.toQueryValue "hostedzoneid" Core.<$> hostedZoneId
            Core.<> (Core.toQueryValue "maxresults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nexttoken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListQueryLoggingConfigsResponse'
            Core.<$> ( x Core..@? "QueryLoggingConfigs" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "QueryLoggingConfig"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListQueryLoggingConfigs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"queryLoggingConfigs") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListQueryLoggingConfigsResponse' smart constructor.
data ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse'
  { -- | An array that contains one <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig> element for each configuration for DNS query logging that is associated with the current AWS account.
    queryLoggingConfigs :: [Types.QueryLoggingConfig],
    -- | If a response includes the last of the query logging configurations that are associated with the current AWS account, @NextToken@ doesn't appear in the response.
    --
    -- If a response doesn't include the last of the configurations, you can get more configurations by submitting another <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs> request. Get the value of @NextToken@ that Amazon Route 53 returned in the previous response and include it in @NextToken@ in the next request.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueryLoggingConfigsResponse' value with any optional fields omitted.
mkListQueryLoggingConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListQueryLoggingConfigsResponse
mkListQueryLoggingConfigsResponse responseStatus =
  ListQueryLoggingConfigsResponse'
    { queryLoggingConfigs =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array that contains one <https://docs.aws.amazon.com/Route53/latest/APIReference/API_QueryLoggingConfig.html QueryLoggingConfig> element for each configuration for DNS query logging that is associated with the current AWS account.
--
-- /Note:/ Consider using 'queryLoggingConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcrrsQueryLoggingConfigs :: Lens.Lens' ListQueryLoggingConfigsResponse [Types.QueryLoggingConfig]
lqlcrrsQueryLoggingConfigs = Lens.field @"queryLoggingConfigs"
{-# DEPRECATED lqlcrrsQueryLoggingConfigs "Use generic-lens or generic-optics with 'queryLoggingConfigs' instead." #-}

-- | If a response includes the last of the query logging configurations that are associated with the current AWS account, @NextToken@ doesn't appear in the response.
--
-- If a response doesn't include the last of the configurations, you can get more configurations by submitting another <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListQueryLoggingConfigs.html ListQueryLoggingConfigs> request. Get the value of @NextToken@ that Amazon Route 53 returned in the previous response and include it in @NextToken@ in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcrrsNextToken :: Lens.Lens' ListQueryLoggingConfigsResponse (Core.Maybe Types.PaginationToken)
lqlcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqlcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqlcrrsResponseStatus :: Lens.Lens' ListQueryLoggingConfigsResponse Core.Int
lqlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lqlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
