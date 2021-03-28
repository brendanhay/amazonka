{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListPlatformVersions (..)
    , mkListPlatformVersions
    -- ** Request lenses
    , lpvFilters
    , lpvMaxRecords
    , lpvNextToken

    -- * Destructuring the response
    , ListPlatformVersionsResponse (..)
    , mkListPlatformVersionsResponse
    -- ** Response lenses
    , lpvrrsNextToken
    , lpvrrsPlatformSummaryList
    , lpvrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPlatformVersions' smart constructor.
data ListPlatformVersions = ListPlatformVersions'
  { filters :: Core.Maybe [Types.PlatformFilter]
    -- ^ Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
  , maxRecords :: Core.Maybe Core.Natural
    -- ^ The maximum number of platform version values returned in one call.
  , nextToken :: Core.Maybe Types.Token
    -- ^ For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformVersions' value with any optional fields omitted.
mkListPlatformVersions
    :: ListPlatformVersions
mkListPlatformVersions
  = ListPlatformVersions'{filters = Core.Nothing,
                          maxRecords = Core.Nothing, nextToken = Core.Nothing}

-- | Criteria for restricting the resulting list of platform versions. The filter is interpreted as a logical conjunction (AND) of the separate @PlatformFilter@ terms.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvFilters :: Lens.Lens' ListPlatformVersions (Core.Maybe [Types.PlatformFilter])
lpvFilters = Lens.field @"filters"
{-# INLINEABLE lpvFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of platform version values returned in one call.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvMaxRecords :: Lens.Lens' ListPlatformVersions (Core.Maybe Core.Natural)
lpvMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE lpvMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvNextToken :: Lens.Lens' ListPlatformVersions (Core.Maybe Types.Token)
lpvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPlatformVersions where
        toQuery ListPlatformVersions{..}
          = Core.toQueryPair "Action" ("ListPlatformVersions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "member") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListPlatformVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPlatformVersions where
        type Rs ListPlatformVersions = ListPlatformVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListPlatformVersionsResult"
              (\ s h x ->
                 ListPlatformVersionsResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "PlatformSummaryList" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPlatformVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"platformSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPlatformVersionsResponse' smart constructor.
data ListPlatformVersionsResponse = ListPlatformVersionsResponse'
  { nextToken :: Core.Maybe Types.Token
    -- ^ In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
  , platformSummaryList :: Core.Maybe [Types.PlatformSummary]
    -- ^ Summary information about the platform versions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformVersionsResponse' value with any optional fields omitted.
mkListPlatformVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPlatformVersionsResponse
mkListPlatformVersionsResponse responseStatus
  = ListPlatformVersionsResponse'{nextToken = Core.Nothing,
                                  platformSummaryList = Core.Nothing, responseStatus}

-- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsNextToken :: Lens.Lens' ListPlatformVersionsResponse (Core.Maybe Types.Token)
lpvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Summary information about the platform versions.
--
-- /Note:/ Consider using 'platformSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsPlatformSummaryList :: Lens.Lens' ListPlatformVersionsResponse (Core.Maybe [Types.PlatformSummary])
lpvrrsPlatformSummaryList = Lens.field @"platformSummaryList"
{-# INLINEABLE lpvrrsPlatformSummaryList #-}
{-# DEPRECATED platformSummaryList "Use generic-lens or generic-optics with 'platformSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsResponseStatus :: Lens.Lens' ListPlatformVersionsResponse Core.Int
lpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
