{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListPlatformBranches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform branches available for your account in an AWS Region. Provides summary information about each platform branch.
--
-- For definitions of platform branch and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
module Network.AWS.ElasticBeanstalk.ListPlatformBranches
    (
    -- * Creating a request
      ListPlatformBranches (..)
    , mkListPlatformBranches
    -- ** Request lenses
    , lpbFilters
    , lpbMaxRecords
    , lpbNextToken

    -- * Destructuring the response
    , ListPlatformBranchesResponse (..)
    , mkListPlatformBranchesResponse
    -- ** Response lenses
    , lpbrrsNextToken
    , lpbrrsPlatformBranchSummaryList
    , lpbrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPlatformBranches' smart constructor.
data ListPlatformBranches = ListPlatformBranches'
  { filters :: Core.Maybe [Types.SearchFilter]
    -- ^ Criteria for restricting the resulting list of platform branches. The filter is evaluated as a logical conjunction (AND) of the separate @SearchFilter@ terms.
--
-- The following list shows valid attribute values for each of the @SearchFilter@ terms. Most operators take a single value. The @in@ and @not_in@ operators can take multiple values.
--
--     * @Attribute = BranchName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@ 
--
--
--
--
--     * @Attribute = LifecycleState@ :
--
--     * @Operator@ : @=@ | @!=@ | @in@ | @not_in@ 
--
--
--     * @Values@ : @beta@ | @supported@ | @deprecated@ | @retired@ 
--
--
--
--
--     * @Attribute = PlatformName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@ 
--
--
--
--
--     * @Attribute = TierType@ :
--
--     * @Operator@ : @=@ | @!=@ 
--
--
--     * @Values@ : @WebServer/Standard@ | @Worker/SQS/HTTP@ 
--
--
--
--
-- Array size: limited to 10 @SearchFilter@ objects.
-- Within each @SearchFilter@ item, the @Values@ array is limited to 10 items.
  , maxRecords :: Core.Maybe Core.Natural
    -- ^ The maximum number of platform branch values returned in one call.
  , nextToken :: Core.Maybe Types.Token
    -- ^ For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformBranches' value with any optional fields omitted.
mkListPlatformBranches
    :: ListPlatformBranches
mkListPlatformBranches
  = ListPlatformBranches'{filters = Core.Nothing,
                          maxRecords = Core.Nothing, nextToken = Core.Nothing}

-- | Criteria for restricting the resulting list of platform branches. The filter is evaluated as a logical conjunction (AND) of the separate @SearchFilter@ terms.
--
-- The following list shows valid attribute values for each of the @SearchFilter@ terms. Most operators take a single value. The @in@ and @not_in@ operators can take multiple values.
--
--     * @Attribute = BranchName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@ 
--
--
--
--
--     * @Attribute = LifecycleState@ :
--
--     * @Operator@ : @=@ | @!=@ | @in@ | @not_in@ 
--
--
--     * @Values@ : @beta@ | @supported@ | @deprecated@ | @retired@ 
--
--
--
--
--     * @Attribute = PlatformName@ :
--
--     * @Operator@ : @=@ | @!=@ | @begins_with@ | @ends_with@ | @contains@ | @in@ | @not_in@ 
--
--
--
--
--     * @Attribute = TierType@ :
--
--     * @Operator@ : @=@ | @!=@ 
--
--
--     * @Values@ : @WebServer/Standard@ | @Worker/SQS/HTTP@ 
--
--
--
--
-- Array size: limited to 10 @SearchFilter@ objects.
-- Within each @SearchFilter@ item, the @Values@ array is limited to 10 items.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbFilters :: Lens.Lens' ListPlatformBranches (Core.Maybe [Types.SearchFilter])
lpbFilters = Lens.field @"filters"
{-# INLINEABLE lpbFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of platform branch values returned in one call.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbMaxRecords :: Lens.Lens' ListPlatformBranches (Core.Maybe Core.Natural)
lpbMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE lpbMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | For a paginated request. Specify a token from a previous response page to retrieve the next response page. All other parameter values must be identical to the ones specified in the initial request.
--
-- If no @NextToken@ is specified, the first page is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbNextToken :: Lens.Lens' ListPlatformBranches (Core.Maybe Types.Token)
lpbNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPlatformBranches where
        toQuery ListPlatformBranches{..}
          = Core.toQueryPair "Action" ("ListPlatformBranches" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "member") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListPlatformBranches where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPlatformBranches where
        type Rs ListPlatformBranches = ListPlatformBranchesResponse
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
          = Response.receiveXMLWrapper "ListPlatformBranchesResult"
              (\ s h x ->
                 ListPlatformBranchesResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "PlatformBranchSummaryList" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListPlatformBranchesResponse' smart constructor.
data ListPlatformBranchesResponse = ListPlatformBranchesResponse'
  { nextToken :: Core.Maybe Types.Token
    -- ^ In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
  , platformBranchSummaryList :: Core.Maybe [Types.PlatformBranchSummary]
    -- ^ Summary information about the platform branches.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPlatformBranchesResponse' value with any optional fields omitted.
mkListPlatformBranchesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPlatformBranchesResponse
mkListPlatformBranchesResponse responseStatus
  = ListPlatformBranchesResponse'{nextToken = Core.Nothing,
                                  platformBranchSummaryList = Core.Nothing, responseStatus}

-- | In a paginated request, if this value isn't @null@ , it's the token that you can pass in a subsequent request to get the next response page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbrrsNextToken :: Lens.Lens' ListPlatformBranchesResponse (Core.Maybe Types.Token)
lpbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Summary information about the platform branches.
--
-- /Note:/ Consider using 'platformBranchSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbrrsPlatformBranchSummaryList :: Lens.Lens' ListPlatformBranchesResponse (Core.Maybe [Types.PlatformBranchSummary])
lpbrrsPlatformBranchSummaryList = Lens.field @"platformBranchSummaryList"
{-# INLINEABLE lpbrrsPlatformBranchSummaryList #-}
{-# DEPRECATED platformBranchSummaryList "Use generic-lens or generic-optics with 'platformBranchSummaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpbrrsResponseStatus :: Lens.Lens' ListPlatformBranchesResponse Core.Int
lpbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
