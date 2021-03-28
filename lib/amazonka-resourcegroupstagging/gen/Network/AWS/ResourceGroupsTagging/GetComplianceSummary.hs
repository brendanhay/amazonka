{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a table that shows counts of resources that are noncompliant with their tag policies.
--
-- For more information on tag policies, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies> in the /AWS Organizations User Guide./ 
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetComplianceSummary
    (
    -- * Creating a request
      GetComplianceSummary (..)
    , mkGetComplianceSummary
    -- ** Request lenses
    , gcsGroupBy
    , gcsMaxResults
    , gcsPaginationToken
    , gcsRegionFilters
    , gcsResourceTypeFilters
    , gcsTagKeyFilters
    , gcsTargetIdFilters

    -- * Destructuring the response
    , GetComplianceSummaryResponse (..)
    , mkGetComplianceSummaryResponse
    -- ** Response lenses
    , gcsrrsPaginationToken
    , gcsrrsSummaryList
    , gcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetComplianceSummary' smart constructor.
data GetComplianceSummary = GetComplianceSummary'
  { groupBy :: Core.Maybe [Types.GroupByAttribute]
    -- ^ A list of attributes to group the counts of noncompliant resources by. If supplied, the counts are sorted by those attributes.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ A limit that restricts the number of results that are returned per page.
  , paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
  , regionFilters :: Core.Maybe (Core.NonEmpty Types.Region)
    -- ^ A list of Regions to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources in the specified Regions.
  , resourceTypeFilters :: Core.Maybe [Types.AmazonResourceType]
    -- ^ The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances. 
--
-- The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:
--
--     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .
--
--
--     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .
--
--
--     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
--
-- You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter. 
  , tagKeyFilters :: Core.Maybe (Core.NonEmpty Types.TagKey)
    -- ^ A list of tag keys to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources that have the specified tag keys.
  , targetIdFilters :: Core.Maybe (Core.NonEmpty Types.TargetId)
    -- ^ The target identifiers (usually, specific account IDs) to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources with the specified target IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceSummary' value with any optional fields omitted.
mkGetComplianceSummary
    :: GetComplianceSummary
mkGetComplianceSummary
  = GetComplianceSummary'{groupBy = Core.Nothing,
                          maxResults = Core.Nothing, paginationToken = Core.Nothing,
                          regionFilters = Core.Nothing, resourceTypeFilters = Core.Nothing,
                          tagKeyFilters = Core.Nothing, targetIdFilters = Core.Nothing}

-- | A list of attributes to group the counts of noncompliant resources by. If supplied, the counts are sorted by those attributes.
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsGroupBy :: Lens.Lens' GetComplianceSummary (Core.Maybe [Types.GroupByAttribute])
gcsGroupBy = Lens.field @"groupBy"
{-# INLINEABLE gcsGroupBy #-}
{-# DEPRECATED groupBy "Use generic-lens or generic-optics with 'groupBy' instead"  #-}

-- | A limit that restricts the number of results that are returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsMaxResults :: Lens.Lens' GetComplianceSummary (Core.Maybe Core.Natural)
gcsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsPaginationToken :: Lens.Lens' GetComplianceSummary (Core.Maybe Types.PaginationToken)
gcsPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE gcsPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

-- | A list of Regions to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources in the specified Regions.
--
-- /Note:/ Consider using 'regionFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsRegionFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe (Core.NonEmpty Types.Region))
gcsRegionFilters = Lens.field @"regionFilters"
{-# INLINEABLE gcsRegionFilters #-}
{-# DEPRECATED regionFilters "Use generic-lens or generic-optics with 'regionFilters' instead"  #-}

-- | The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances. 
--
-- The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:
--
--     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .
--
--
--     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .
--
--
--     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
--
-- You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter. 
--
-- /Note:/ Consider using 'resourceTypeFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsResourceTypeFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe [Types.AmazonResourceType])
gcsResourceTypeFilters = Lens.field @"resourceTypeFilters"
{-# INLINEABLE gcsResourceTypeFilters #-}
{-# DEPRECATED resourceTypeFilters "Use generic-lens or generic-optics with 'resourceTypeFilters' instead"  #-}

-- | A list of tag keys to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources that have the specified tag keys.
--
-- /Note:/ Consider using 'tagKeyFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsTagKeyFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe (Core.NonEmpty Types.TagKey))
gcsTagKeyFilters = Lens.field @"tagKeyFilters"
{-# INLINEABLE gcsTagKeyFilters #-}
{-# DEPRECATED tagKeyFilters "Use generic-lens or generic-optics with 'tagKeyFilters' instead"  #-}

-- | The target identifiers (usually, specific account IDs) to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources with the specified target IDs.
--
-- /Note:/ Consider using 'targetIdFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsTargetIdFilters :: Lens.Lens' GetComplianceSummary (Core.Maybe (Core.NonEmpty Types.TargetId))
gcsTargetIdFilters = Lens.field @"targetIdFilters"
{-# INLINEABLE gcsTargetIdFilters #-}
{-# DEPRECATED targetIdFilters "Use generic-lens or generic-optics with 'targetIdFilters' instead"  #-}

instance Core.ToQuery GetComplianceSummary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetComplianceSummary where
        toHeaders GetComplianceSummary{..}
          = Core.pure
              ("X-Amz-Target",
               "ResourceGroupsTaggingAPI_20170126.GetComplianceSummary")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetComplianceSummary where
        toJSON GetComplianceSummary{..}
          = Core.object
              (Core.catMaybes
                 [("GroupBy" Core..=) Core.<$> groupBy,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("PaginationToken" Core..=) Core.<$> paginationToken,
                  ("RegionFilters" Core..=) Core.<$> regionFilters,
                  ("ResourceTypeFilters" Core..=) Core.<$> resourceTypeFilters,
                  ("TagKeyFilters" Core..=) Core.<$> tagKeyFilters,
                  ("TargetIdFilters" Core..=) Core.<$> targetIdFilters])

instance Core.AWSRequest GetComplianceSummary where
        type Rs GetComplianceSummary = GetComplianceSummaryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetComplianceSummaryResponse' Core.<$>
                   (x Core..:? "PaginationToken") Core.<*> x Core..:? "SummaryList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetComplianceSummary where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"paginationToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"summaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"paginationToken" Lens..~
                   rs Lens.^. Lens.field @"paginationToken")

-- | /See:/ 'mkGetComplianceSummaryResponse' smart constructor.
data GetComplianceSummaryResponse = GetComplianceSummaryResponse'
  { paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
  , summaryList :: Core.Maybe [Types.Summary]
    -- ^ A table that shows counts of noncompliant resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceSummaryResponse' value with any optional fields omitted.
mkGetComplianceSummaryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetComplianceSummaryResponse
mkGetComplianceSummaryResponse responseStatus
  = GetComplianceSummaryResponse'{paginationToken = Core.Nothing,
                                  summaryList = Core.Nothing, responseStatus}

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsPaginationToken :: Lens.Lens' GetComplianceSummaryResponse (Core.Maybe Types.PaginationToken)
gcsrrsPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE gcsrrsPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

-- | A table that shows counts of noncompliant resources.
--
-- /Note:/ Consider using 'summaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsSummaryList :: Lens.Lens' GetComplianceSummaryResponse (Core.Maybe [Types.Summary])
gcsrrsSummaryList = Lens.field @"summaryList"
{-# INLINEABLE gcsrrsSummaryList #-}
{-# DEPRECATED summaryList "Use generic-lens or generic-optics with 'summaryList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsrrsResponseStatus :: Lens.Lens' GetComplianceSummaryResponse Core.Int
gcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
