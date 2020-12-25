{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListPlatformVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform versions available for your account in an AWS Region. Provides summary information about each platform version. Compare to 'DescribePlatformVersion' , which provides full details about a single platform version.
--
-- For definitions of platform version and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
--
-- This operation returns paginated results.
module Network.AWS.ElasticBeanstalk.ListPlatformVersions
  ( -- * Creating a request
    ListPlatformVersions (..),
    mkListPlatformVersions,

    -- ** Request lenses
    lpvFilters,
    lpvMaxRecords,
    lpvNextToken,

    -- * Destructuring the response
    ListPlatformVersionsResponse (..),
    mkListPlatformVersionsResponse,

    -- ** Response lenses
    lpvrrsNextToken,
    lpvrrsPlatformSummaryList,
    lpvrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPlatformVersions' smart constructor.
data ListPlatformVersions = ListPlatformVersions'
  { -- | Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
    filters :: Core.Maybe [Types.PlatformFilter],
    -- | The maximum number of platform version values returned in one call.
    maxRecords :: Core.Maybe Core.Natural,
    -- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
    --
    -- If no @NextToken@ is specified, the first page is retrieved.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformVersions' value with any optional fields omitted.
mkListPlatformVersions ::
  ListPlatformVersions
mkListPlatformVersions =
  ListPlatformVersions'
    { filters = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvFilters :: Lens.Lens' ListPlatformVersions (Core.Maybe [Types.PlatformFilter])
lpvFilters = Lens.field @"filters"
{-# DEPRECATED lpvFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of platform version values returned in one call.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMaxRecords :: Lens.Lens' ListPlatformVersions (Core.Maybe Core.Natural)
lpvMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED lpvMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvNextToken :: Lens.Lens' ListPlatformVersions (Core.Maybe Types.Token)
lpvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListPlatformVersions where
  type Rs ListPlatformVersions = ListPlatformVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListPlatformVersions")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "member" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListPlatformVersionsResult"
      ( \s h x ->
          ListPlatformVersionsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "PlatformSummaryList"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPlatformVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"platformSummaryList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListPlatformVersionsResponse' smart constructor.
data ListPlatformVersionsResponse = ListPlatformVersionsResponse'
  { -- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
    nextToken :: Core.Maybe Types.Token,
    -- | Summary information about the platform versions.
    platformSummaryList :: Core.Maybe [Types.PlatformSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformVersionsResponse' value with any optional fields omitted.
mkListPlatformVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPlatformVersionsResponse
mkListPlatformVersionsResponse responseStatus =
  ListPlatformVersionsResponse'
    { nextToken = Core.Nothing,
      platformSummaryList = Core.Nothing,
      responseStatus
    }

-- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsNextToken :: Lens.Lens' ListPlatformVersionsResponse (Core.Maybe Types.Token)
lpvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summary information about the platform versions.
--
-- /Note:/ Consider using 'platformSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsPlatformSummaryList :: Lens.Lens' ListPlatformVersionsResponse (Core.Maybe [Types.PlatformSummary])
lpvrrsPlatformSummaryList = Lens.field @"platformSummaryList"
{-# DEPRECATED lpvrrsPlatformSummaryList "Use generic-lens or generic-optics with 'platformSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsResponseStatus :: Lens.Lens' ListPlatformVersionsResponse Core.Int
lpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
