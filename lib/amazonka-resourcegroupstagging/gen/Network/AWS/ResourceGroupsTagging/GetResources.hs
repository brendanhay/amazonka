{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetResources (..),
    mkGetResources,

    -- ** Request lenses
    grPaginationToken,
    grResourcesPerPage,
    grExcludeCompliantResources,
    grIncludeComplianceDetails,
    grResourceTypeFilters,
    grTagFilters,
    grTagsPerPage,

    -- * Destructuring the response
    GetResourcesResponse (..),
    mkGetResourcesResponse,

    -- ** Response lenses
    grrsPaginationToken,
    grrsResourceTagMappingList,
    grrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResources' smart constructor.
data GetResources = GetResources'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    resourcesPerPage :: Lude.Maybe Lude.Int,
    excludeCompliantResources :: Lude.Maybe Lude.Bool,
    includeComplianceDetails :: Lude.Maybe Lude.Bool,
    resourceTypeFilters :: Lude.Maybe [Lude.Text],
    tagFilters :: Lude.Maybe [TagFilter],
    tagsPerPage :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResources' with the minimum fields required to make a request.
--
-- * 'excludeCompliantResources' - Specifies whether to exclude resources that are compliant with the tag policy. Set this to @true@ if you are interested in retrieving information on noncompliant resources only.
--
-- You can use this parameter only if the @IncludeComplianceDetails@ parameter is also set to @true@ .
-- * 'includeComplianceDetails' - Specifies whether to include details regarding the compliance with the effective tag policy. Set this to @true@ to determine whether resources are compliant with the tag policy and to get details.
-- * 'paginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
-- * 'resourceTypeFilters' - The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.
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
-- * 'resourcesPerPage' - A limit that restricts the number of resources returned by GetResources in paginated output. You can set ResourcesPerPage to a minimum of 1 item and the maximum of 100 items.
-- * 'tagFilters' - A list of TagFilters (keys and values). Each TagFilter specified must contain a key with values as optional. A request can include up to 50 keys, and each key can include up to 20 values.
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
-- * 'tagsPerPage' - AWS recommends using @ResourcesPerPage@ instead of this parameter.
--
-- A limit that restricts the number of tags (key and value pairs) returned by GetResources in paginated output. A resource with no tags is counted as having one tag (one key and value pair).
-- @GetResources@ does not split a resource and its associated tags across pages. If the specified @TagsPerPage@ would cause such a break, a @PaginationToken@ is returned in place of the affected resource and its tags. Use that token in another request to get the remaining data. For example, if you specify a @TagsPerPage@ of @100@ and the account has 22 resources with 10 tags each (meaning that each resource has 10 key and value pairs), the output will consist of three pages. The first page displays the first 10 resources, each with its 10 tags. The second page displays the next 10 resources, each with its 10 tags. The third page displays the remaining 2 resources, each with its 10 tags.
-- You can set @TagsPerPage@ to a minimum of 100 items and the maximum of 500 items.
mkGetResources ::
  GetResources
mkGetResources =
  GetResources'
    { paginationToken = Lude.Nothing,
      resourcesPerPage = Lude.Nothing,
      excludeCompliantResources = Lude.Nothing,
      includeComplianceDetails = Lude.Nothing,
      resourceTypeFilters = Lude.Nothing,
      tagFilters = Lude.Nothing,
      tagsPerPage = Lude.Nothing
    }

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grPaginationToken :: Lens.Lens' GetResources (Lude.Maybe Lude.Text)
grPaginationToken = Lens.lens (paginationToken :: GetResources -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetResources)
{-# DEPRECATED grPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A limit that restricts the number of resources returned by GetResources in paginated output. You can set ResourcesPerPage to a minimum of 1 item and the maximum of 100 items.
--
-- /Note:/ Consider using 'resourcesPerPage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grResourcesPerPage :: Lens.Lens' GetResources (Lude.Maybe Lude.Int)
grResourcesPerPage = Lens.lens (resourcesPerPage :: GetResources -> Lude.Maybe Lude.Int) (\s a -> s {resourcesPerPage = a} :: GetResources)
{-# DEPRECATED grResourcesPerPage "Use generic-lens or generic-optics with 'resourcesPerPage' instead." #-}

-- | Specifies whether to exclude resources that are compliant with the tag policy. Set this to @true@ if you are interested in retrieving information on noncompliant resources only.
--
-- You can use this parameter only if the @IncludeComplianceDetails@ parameter is also set to @true@ .
--
-- /Note:/ Consider using 'excludeCompliantResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grExcludeCompliantResources :: Lens.Lens' GetResources (Lude.Maybe Lude.Bool)
grExcludeCompliantResources = Lens.lens (excludeCompliantResources :: GetResources -> Lude.Maybe Lude.Bool) (\s a -> s {excludeCompliantResources = a} :: GetResources)
{-# DEPRECATED grExcludeCompliantResources "Use generic-lens or generic-optics with 'excludeCompliantResources' instead." #-}

-- | Specifies whether to include details regarding the compliance with the effective tag policy. Set this to @true@ to determine whether resources are compliant with the tag policy and to get details.
--
-- /Note:/ Consider using 'includeComplianceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grIncludeComplianceDetails :: Lens.Lens' GetResources (Lude.Maybe Lude.Bool)
grIncludeComplianceDetails = Lens.lens (includeComplianceDetails :: GetResources -> Lude.Maybe Lude.Bool) (\s a -> s {includeComplianceDetails = a} :: GetResources)
{-# DEPRECATED grIncludeComplianceDetails "Use generic-lens or generic-optics with 'includeComplianceDetails' instead." #-}

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
grResourceTypeFilters :: Lens.Lens' GetResources (Lude.Maybe [Lude.Text])
grResourceTypeFilters = Lens.lens (resourceTypeFilters :: GetResources -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypeFilters = a} :: GetResources)
{-# DEPRECATED grResourceTypeFilters "Use generic-lens or generic-optics with 'resourceTypeFilters' instead." #-}

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
grTagFilters :: Lens.Lens' GetResources (Lude.Maybe [TagFilter])
grTagFilters = Lens.lens (tagFilters :: GetResources -> Lude.Maybe [TagFilter]) (\s a -> s {tagFilters = a} :: GetResources)
{-# DEPRECATED grTagFilters "Use generic-lens or generic-optics with 'tagFilters' instead." #-}

-- | AWS recommends using @ResourcesPerPage@ instead of this parameter.
--
-- A limit that restricts the number of tags (key and value pairs) returned by GetResources in paginated output. A resource with no tags is counted as having one tag (one key and value pair).
-- @GetResources@ does not split a resource and its associated tags across pages. If the specified @TagsPerPage@ would cause such a break, a @PaginationToken@ is returned in place of the affected resource and its tags. Use that token in another request to get the remaining data. For example, if you specify a @TagsPerPage@ of @100@ and the account has 22 resources with 10 tags each (meaning that each resource has 10 key and value pairs), the output will consist of three pages. The first page displays the first 10 resources, each with its 10 tags. The second page displays the next 10 resources, each with its 10 tags. The third page displays the remaining 2 resources, each with its 10 tags.
-- You can set @TagsPerPage@ to a minimum of 100 items and the maximum of 500 items.
--
-- /Note:/ Consider using 'tagsPerPage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grTagsPerPage :: Lens.Lens' GetResources (Lude.Maybe Lude.Int)
grTagsPerPage = Lens.lens (tagsPerPage :: GetResources -> Lude.Maybe Lude.Int) (\s a -> s {tagsPerPage = a} :: GetResources)
{-# DEPRECATED grTagsPerPage "Use generic-lens or generic-optics with 'tagsPerPage' instead." #-}

instance Page.AWSPager GetResources where
  page rq rs
    | Page.stop (rs Lens.^. grrsPaginationToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grrsResourceTagMappingList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grPaginationToken Lens..~ rs Lens.^. grrsPaginationToken

instance Lude.AWSRequest GetResources where
  type Rs GetResources = GetResourcesResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "ResourceTagMappingList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.GetResources" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetResources where
  toJSON GetResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            ("ResourcesPerPage" Lude..=) Lude.<$> resourcesPerPage,
            ("ExcludeCompliantResources" Lude..=)
              Lude.<$> excludeCompliantResources,
            ("IncludeComplianceDetails" Lude..=)
              Lude.<$> includeComplianceDetails,
            ("ResourceTypeFilters" Lude..=) Lude.<$> resourceTypeFilters,
            ("TagFilters" Lude..=) Lude.<$> tagFilters,
            ("TagsPerPage" Lude..=) Lude.<$> tagsPerPage
          ]
      )

instance Lude.ToPath GetResources where
  toPath = Lude.const "/"

instance Lude.ToQuery GetResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    resourceTagMappingList ::
      Lude.Maybe [ResourceTagMapping],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcesResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
-- * 'resourceTagMappingList' - A list of resource ARNs and the tags (keys and values) associated with each.
-- * 'responseStatus' - The response status code.
mkGetResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcesResponse
mkGetResourcesResponse pResponseStatus_ =
  GetResourcesResponse'
    { paginationToken = Lude.Nothing,
      resourceTagMappingList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsPaginationToken :: Lens.Lens' GetResourcesResponse (Lude.Maybe Lude.Text)
grrsPaginationToken = Lens.lens (paginationToken :: GetResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetResourcesResponse)
{-# DEPRECATED grrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A list of resource ARNs and the tags (keys and values) associated with each.
--
-- /Note:/ Consider using 'resourceTagMappingList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResourceTagMappingList :: Lens.Lens' GetResourcesResponse (Lude.Maybe [ResourceTagMapping])
grrsResourceTagMappingList = Lens.lens (resourceTagMappingList :: GetResourcesResponse -> Lude.Maybe [ResourceTagMapping]) (\s a -> s {resourceTagMappingList = a} :: GetResourcesResponse)
{-# DEPRECATED grrsResourceTagMappingList "Use generic-lens or generic-optics with 'resourceTagMappingList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetResourcesResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcesResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
