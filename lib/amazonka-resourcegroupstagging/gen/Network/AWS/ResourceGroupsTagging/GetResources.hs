{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the tagged or previously tagged resources that are located in the specified Region for the AWS account.
--
-- Depending on what information you want returned, you can also specify the following:
--
--     * /Filters/ that specify what tags and resource types you want returned. The response includes all tags that are associated with the requested resources.
--
--
--     * Information about compliance with the account's effective tag policy. For more information on tag policies, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies> in the /AWS Organizations User Guide./ 
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetResources
    (
    -- * Creating a request
      GetResources (..)
    , mkGetResources
    -- ** Request lenses
    , grExcludeCompliantResources
    , grIncludeComplianceDetails
    , grPaginationToken
    , grResourceTypeFilters
    , grResourcesPerPage
    , grTagFilters
    , grTagsPerPage

    -- * Destructuring the response
    , GetResourcesResponse (..)
    , mkGetResourcesResponse
    -- ** Response lenses
    , grrrsPaginationToken
    , grrrsResourceTagMappingList
    , grrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroupsTagging.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResources' smart constructor.
data GetResources = GetResources'
  { excludeCompliantResources :: Core.Maybe Core.Bool
    -- ^ Specifies whether to exclude resources that are compliant with the tag policy. Set this to @true@ if you are interested in retrieving information on noncompliant resources only.
--
-- You can use this parameter only if the @IncludeComplianceDetails@ parameter is also set to @true@ .
  , includeComplianceDetails :: Core.Maybe Core.Bool
    -- ^ Specifies whether to include details regarding the compliance with the effective tag policy. Set this to @true@ to determine whether resources are compliant with the tag policy and to get details.
  , paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
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
  , resourcesPerPage :: Core.Maybe Core.Int
    -- ^ A limit that restricts the number of resources returned by GetResources in paginated output. You can set ResourcesPerPage to a minimum of 1 item and the maximum of 100 items. 
  , tagFilters :: Core.Maybe [Types.TagFilter]
    -- ^ A list of TagFilters (keys and values). Each TagFilter specified must contain a key with values as optional. A request can include up to 50 keys, and each key can include up to 20 values. 
--
-- Note the following when deciding how to use TagFilters:
--
--     * If you /do/ specify a TagFilter, the response returns only those resources that are currently associated with the specified tag. 
--
--
--     * If you /don't/ specify a TagFilter, the response includes all resources that were ever associated with tags. Resources that currently don't have associated tags are shown with an empty tag set, like this: @"Tags": []@ .
--
--
--     * If you specify more than one filter in a single request, the response returns only those resources that satisfy all specified filters.
--
--
--     * If you specify a filter that contains more than one value for a key, the response returns resources that match any of the specified values for that key.
--
--
--     * If you don't specify any values for a key, the response returns resources that are tagged with that key irrespective of the value.
-- For example, for filters: filter1 = {key1, {value1}}, filter2 = {key2, {value2,value3,value4}} , filter3 = {key3}:
--
--     * GetResources( {filter1} ) returns resources tagged with key1=value1
--
--
--     * GetResources( {filter2} ) returns resources tagged with key2=value2 or key2=value3 or key2=value4
--
--
--     * GetResources( {filter3} ) returns resources tagged with any tag containing key3 as its tag key, irrespective of its value
--
--
--     * GetResources( {filter1,filter2,filter3} ) returns resources tagged with ( key1=value1) and ( key2=value2 or key2=value3 or key2=value4) and (key3, irrespective of the value)
--
--
--
--
  , tagsPerPage :: Core.Maybe Core.Int
    -- ^ AWS recommends using @ResourcesPerPage@ instead of this parameter.
--
-- A limit that restricts the number of tags (key and value pairs) returned by GetResources in paginated output. A resource with no tags is counted as having one tag (one key and value pair).
-- @GetResources@ does not split a resource and its associated tags across pages. If the specified @TagsPerPage@ would cause such a break, a @PaginationToken@ is returned in place of the affected resource and its tags. Use that token in another request to get the remaining data. For example, if you specify a @TagsPerPage@ of @100@ and the account has 22 resources with 10 tags each (meaning that each resource has 10 key and value pairs), the output will consist of three pages. The first page displays the first 10 resources, each with its 10 tags. The second page displays the next 10 resources, each with its 10 tags. The third page displays the remaining 2 resources, each with its 10 tags.
-- You can set @TagsPerPage@ to a minimum of 100 items and the maximum of 500 items.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResources' value with any optional fields omitted.
mkGetResources
    :: GetResources
mkGetResources
  = GetResources'{excludeCompliantResources = Core.Nothing,
                  includeComplianceDetails = Core.Nothing,
                  paginationToken = Core.Nothing, resourceTypeFilters = Core.Nothing,
                  resourcesPerPage = Core.Nothing, tagFilters = Core.Nothing,
                  tagsPerPage = Core.Nothing}

-- | Specifies whether to exclude resources that are compliant with the tag policy. Set this to @true@ if you are interested in retrieving information on noncompliant resources only.
--
-- You can use this parameter only if the @IncludeComplianceDetails@ parameter is also set to @true@ .
--
-- /Note:/ Consider using 'excludeCompliantResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grExcludeCompliantResources :: Lens.Lens' GetResources (Core.Maybe Core.Bool)
grExcludeCompliantResources = Lens.field @"excludeCompliantResources"
{-# INLINEABLE grExcludeCompliantResources #-}
{-# DEPRECATED excludeCompliantResources "Use generic-lens or generic-optics with 'excludeCompliantResources' instead"  #-}

-- | Specifies whether to include details regarding the compliance with the effective tag policy. Set this to @true@ to determine whether resources are compliant with the tag policy and to get details.
--
-- /Note:/ Consider using 'includeComplianceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grIncludeComplianceDetails :: Lens.Lens' GetResources (Core.Maybe Core.Bool)
grIncludeComplianceDetails = Lens.field @"includeComplianceDetails"
{-# INLINEABLE grIncludeComplianceDetails #-}
{-# DEPRECATED includeComplianceDetails "Use generic-lens or generic-optics with 'includeComplianceDetails' instead"  #-}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grPaginationToken :: Lens.Lens' GetResources (Core.Maybe Types.PaginationToken)
grPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE grPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

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
grResourceTypeFilters :: Lens.Lens' GetResources (Core.Maybe [Types.AmazonResourceType])
grResourceTypeFilters = Lens.field @"resourceTypeFilters"
{-# INLINEABLE grResourceTypeFilters #-}
{-# DEPRECATED resourceTypeFilters "Use generic-lens or generic-optics with 'resourceTypeFilters' instead"  #-}

-- | A limit that restricts the number of resources returned by GetResources in paginated output. You can set ResourcesPerPage to a minimum of 1 item and the maximum of 100 items. 
--
-- /Note:/ Consider using 'resourcesPerPage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grResourcesPerPage :: Lens.Lens' GetResources (Core.Maybe Core.Int)
grResourcesPerPage = Lens.field @"resourcesPerPage"
{-# INLINEABLE grResourcesPerPage #-}
{-# DEPRECATED resourcesPerPage "Use generic-lens or generic-optics with 'resourcesPerPage' instead"  #-}

-- | A list of TagFilters (keys and values). Each TagFilter specified must contain a key with values as optional. A request can include up to 50 keys, and each key can include up to 20 values. 
--
-- Note the following when deciding how to use TagFilters:
--
--     * If you /do/ specify a TagFilter, the response returns only those resources that are currently associated with the specified tag. 
--
--
--     * If you /don't/ specify a TagFilter, the response includes all resources that were ever associated with tags. Resources that currently don't have associated tags are shown with an empty tag set, like this: @"Tags": []@ .
--
--
--     * If you specify more than one filter in a single request, the response returns only those resources that satisfy all specified filters.
--
--
--     * If you specify a filter that contains more than one value for a key, the response returns resources that match any of the specified values for that key.
--
--
--     * If you don't specify any values for a key, the response returns resources that are tagged with that key irrespective of the value.
-- For example, for filters: filter1 = {key1, {value1}}, filter2 = {key2, {value2,value3,value4}} , filter3 = {key3}:
--
--     * GetResources( {filter1} ) returns resources tagged with key1=value1
--
--
--     * GetResources( {filter2} ) returns resources tagged with key2=value2 or key2=value3 or key2=value4
--
--
--     * GetResources( {filter3} ) returns resources tagged with any tag containing key3 as its tag key, irrespective of its value
--
--
--     * GetResources( {filter1,filter2,filter3} ) returns resources tagged with ( key1=value1) and ( key2=value2 or key2=value3 or key2=value4) and (key3, irrespective of the value)
--
--
--
--
--
-- /Note:/ Consider using 'tagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grTagFilters :: Lens.Lens' GetResources (Core.Maybe [Types.TagFilter])
grTagFilters = Lens.field @"tagFilters"
{-# INLINEABLE grTagFilters #-}
{-# DEPRECATED tagFilters "Use generic-lens or generic-optics with 'tagFilters' instead"  #-}

-- | AWS recommends using @ResourcesPerPage@ instead of this parameter.
--
-- A limit that restricts the number of tags (key and value pairs) returned by GetResources in paginated output. A resource with no tags is counted as having one tag (one key and value pair).
-- @GetResources@ does not split a resource and its associated tags across pages. If the specified @TagsPerPage@ would cause such a break, a @PaginationToken@ is returned in place of the affected resource and its tags. Use that token in another request to get the remaining data. For example, if you specify a @TagsPerPage@ of @100@ and the account has 22 resources with 10 tags each (meaning that each resource has 10 key and value pairs), the output will consist of three pages. The first page displays the first 10 resources, each with its 10 tags. The second page displays the next 10 resources, each with its 10 tags. The third page displays the remaining 2 resources, each with its 10 tags.
-- You can set @TagsPerPage@ to a minimum of 100 items and the maximum of 500 items.
--
-- /Note:/ Consider using 'tagsPerPage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grTagsPerPage :: Lens.Lens' GetResources (Core.Maybe Core.Int)
grTagsPerPage = Lens.field @"tagsPerPage"
{-# INLINEABLE grTagsPerPage #-}
{-# DEPRECATED tagsPerPage "Use generic-lens or generic-optics with 'tagsPerPage' instead"  #-}

instance Core.ToQuery GetResources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetResources where
        toHeaders GetResources{..}
          = Core.pure
              ("X-Amz-Target", "ResourceGroupsTaggingAPI_20170126.GetResources")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetResources where
        toJSON GetResources{..}
          = Core.object
              (Core.catMaybes
                 [("ExcludeCompliantResources" Core..=) Core.<$>
                    excludeCompliantResources,
                  ("IncludeComplianceDetails" Core..=) Core.<$>
                    includeComplianceDetails,
                  ("PaginationToken" Core..=) Core.<$> paginationToken,
                  ("ResourceTypeFilters" Core..=) Core.<$> resourceTypeFilters,
                  ("ResourcesPerPage" Core..=) Core.<$> resourcesPerPage,
                  ("TagFilters" Core..=) Core.<$> tagFilters,
                  ("TagsPerPage" Core..=) Core.<$> tagsPerPage])

instance Core.AWSRequest GetResources where
        type Rs GetResources = GetResourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetResourcesResponse' Core.<$>
                   (x Core..:? "PaginationToken") Core.<*>
                     x Core..:? "ResourceTagMappingList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetResources where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"paginationToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"resourceTagMappingList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"paginationToken" Lens..~
                   rs Lens.^. Lens.field @"paginationToken")

-- | /See:/ 'mkGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { paginationToken :: Core.Maybe Types.PaginationToken
    -- ^ A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
  , resourceTagMappingList :: Core.Maybe [Types.ResourceTagMapping]
    -- ^ A list of resource ARNs and the tags (keys and values) associated with each.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcesResponse' value with any optional fields omitted.
mkGetResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetResourcesResponse
mkGetResourcesResponse responseStatus
  = GetResourcesResponse'{paginationToken = Core.Nothing,
                          resourceTagMappingList = Core.Nothing, responseStatus}

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsPaginationToken :: Lens.Lens' GetResourcesResponse (Core.Maybe Types.PaginationToken)
grrrsPaginationToken = Lens.field @"paginationToken"
{-# INLINEABLE grrrsPaginationToken #-}
{-# DEPRECATED paginationToken "Use generic-lens or generic-optics with 'paginationToken' instead"  #-}

-- | A list of resource ARNs and the tags (keys and values) associated with each.
--
-- /Note:/ Consider using 'resourceTagMappingList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResourceTagMappingList :: Lens.Lens' GetResourcesResponse (Core.Maybe [Types.ResourceTagMapping])
grrrsResourceTagMappingList = Lens.field @"resourceTagMappingList"
{-# INLINEABLE grrrsResourceTagMappingList #-}
{-# DEPRECATED resourceTagMappingList "Use generic-lens or generic-optics with 'resourceTagMappingList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetResourcesResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
