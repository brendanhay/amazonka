{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--
-- Depending on what information you want returned, you can also specify the following:
--
--     * /Filters/ that specify what tags and resource types you want returned. The response includes all tags that are associated with the requested resources.
--
--     * Information about compliance with the account's effective tag policy. For more information on tag policies, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies> in the /AWS Organizations User Guide./
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetResources
  ( -- * Creating a Request
    getResources,
    GetResources,

    -- * Request Lenses
    grPaginationToken,
    grResourcesPerPage,
    grExcludeCompliantResources,
    grIncludeComplianceDetails,
    grResourceTypeFilters,
    grTagFilters,
    grTagsPerPage,

    -- * Destructuring the Response
    getResourcesResponse,
    GetResourcesResponse,

    -- * Response Lenses
    grrsPaginationToken,
    grrsResourceTagMappingList,
    grrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.Response

-- | /See:/ 'getResources' smart constructor.
data GetResources = GetResources'
  { _grPaginationToken ::
      !(Maybe Text),
    _grResourcesPerPage :: !(Maybe Int),
    _grExcludeCompliantResources :: !(Maybe Bool),
    _grIncludeComplianceDetails :: !(Maybe Bool),
    _grResourceTypeFilters :: !(Maybe [Text]),
    _grTagFilters :: !(Maybe [TagFilter]),
    _grTagsPerPage :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grPaginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- * 'grResourcesPerPage' - A limit that restricts the number of resources returned by GetResources in paginated output. You can set ResourcesPerPage to a minimum of 1 item and the maximum of 100 items.
--
-- * 'grExcludeCompliantResources' - Specifies whether to exclude resources that are compliant with the tag policy. Set this to @true@ if you are interested in retrieving information on noncompliant resources only. You can use this parameter only if the @IncludeComplianceDetails@ parameter is also set to @true@ .
--
-- * 'grIncludeComplianceDetails' - Specifies whether to include details regarding the compliance with the effective tag policy. Set this to @true@ to determine whether resources are compliant with the tag policy and to get details.
--
-- * 'grResourceTypeFilters' - The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.  The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter.
--
-- * 'grTagFilters' - A list of TagFilters (keys and values). Each TagFilter specified must contain a key with values as optional. A request can include up to 50 keys, and each key can include up to 20 values.  Note the following when deciding how to use TagFilters:     * If you /do/ specify a TagFilter, the response returns only those resources that are currently associated with the specified tag.      * If you /don't/ specify a TagFilter, the response includes all resources that were ever associated with tags. Resources that currently don't have associated tags are shown with an empty tag set, like this: @"Tags": []@ .     * If you specify more than one filter in a single request, the response returns only those resources that satisfy all specified filters.     * If you specify a filter that contains more than one value for a key, the response returns resources that match any of the specified values for that key.     * If you don't specify any values for a key, the response returns resources that are tagged with that key irrespective of the value. For example, for filters: filter1 = {key1, {value1}}, filter2 = {key2, {value2,value3,value4}} , filter3 = {key3}:     * GetResources( {filter1} ) returns resources tagged with key1=value1     * GetResources( {filter2} ) returns resources tagged with key2=value2 or key2=value3 or key2=value4     * GetResources( {filter3} ) returns resources tagged with any tag containing key3 as its tag key, irrespective of its value     * GetResources( {filter1,filter2,filter3} ) returns resources tagged with ( key1=value1) and ( key2=value2 or key2=value3 or key2=value4) and (key3, irrespective of the value)
--
-- * 'grTagsPerPage' - AWS recommends using @ResourcesPerPage@ instead of this parameter. A limit that restricts the number of tags (key and value pairs) returned by GetResources in paginated output. A resource with no tags is counted as having one tag (one key and value pair). @GetResources@ does not split a resource and its associated tags across pages. If the specified @TagsPerPage@ would cause such a break, a @PaginationToken@ is returned in place of the affected resource and its tags. Use that token in another request to get the remaining data. For example, if you specify a @TagsPerPage@ of @100@ and the account has 22 resources with 10 tags each (meaning that each resource has 10 key and value pairs), the output will consist of three pages. The first page displays the first 10 resources, each with its 10 tags. The second page displays the next 10 resources, each with its 10 tags. The third page displays the remaining 2 resources, each with its 10 tags. You can set @TagsPerPage@ to a minimum of 100 items and the maximum of 500 items.
getResources ::
  GetResources
getResources =
  GetResources'
    { _grPaginationToken = Nothing,
      _grResourcesPerPage = Nothing,
      _grExcludeCompliantResources = Nothing,
      _grIncludeComplianceDetails = Nothing,
      _grResourceTypeFilters = Nothing,
      _grTagFilters = Nothing,
      _grTagsPerPage = Nothing
    }

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
grPaginationToken :: Lens' GetResources (Maybe Text)
grPaginationToken = lens _grPaginationToken (\s a -> s {_grPaginationToken = a})

-- | A limit that restricts the number of resources returned by GetResources in paginated output. You can set ResourcesPerPage to a minimum of 1 item and the maximum of 100 items.
grResourcesPerPage :: Lens' GetResources (Maybe Int)
grResourcesPerPage = lens _grResourcesPerPage (\s a -> s {_grResourcesPerPage = a})

-- | Specifies whether to exclude resources that are compliant with the tag policy. Set this to @true@ if you are interested in retrieving information on noncompliant resources only. You can use this parameter only if the @IncludeComplianceDetails@ parameter is also set to @true@ .
grExcludeCompliantResources :: Lens' GetResources (Maybe Bool)
grExcludeCompliantResources = lens _grExcludeCompliantResources (\s a -> s {_grExcludeCompliantResources = a})

-- | Specifies whether to include details regarding the compliance with the effective tag policy. Set this to @true@ to determine whether resources are compliant with the tag policy and to get details.
grIncludeComplianceDetails :: Lens' GetResources (Maybe Bool)
grIncludeComplianceDetails = lens _grIncludeComplianceDetails (\s a -> s {_grIncludeComplianceDetails = a})

-- | The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.  The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter.
grResourceTypeFilters :: Lens' GetResources [Text]
grResourceTypeFilters = lens _grResourceTypeFilters (\s a -> s {_grResourceTypeFilters = a}) . _Default . _Coerce

-- | A list of TagFilters (keys and values). Each TagFilter specified must contain a key with values as optional. A request can include up to 50 keys, and each key can include up to 20 values.  Note the following when deciding how to use TagFilters:     * If you /do/ specify a TagFilter, the response returns only those resources that are currently associated with the specified tag.      * If you /don't/ specify a TagFilter, the response includes all resources that were ever associated with tags. Resources that currently don't have associated tags are shown with an empty tag set, like this: @"Tags": []@ .     * If you specify more than one filter in a single request, the response returns only those resources that satisfy all specified filters.     * If you specify a filter that contains more than one value for a key, the response returns resources that match any of the specified values for that key.     * If you don't specify any values for a key, the response returns resources that are tagged with that key irrespective of the value. For example, for filters: filter1 = {key1, {value1}}, filter2 = {key2, {value2,value3,value4}} , filter3 = {key3}:     * GetResources( {filter1} ) returns resources tagged with key1=value1     * GetResources( {filter2} ) returns resources tagged with key2=value2 or key2=value3 or key2=value4     * GetResources( {filter3} ) returns resources tagged with any tag containing key3 as its tag key, irrespective of its value     * GetResources( {filter1,filter2,filter3} ) returns resources tagged with ( key1=value1) and ( key2=value2 or key2=value3 or key2=value4) and (key3, irrespective of the value)
grTagFilters :: Lens' GetResources [TagFilter]
grTagFilters = lens _grTagFilters (\s a -> s {_grTagFilters = a}) . _Default . _Coerce

-- | AWS recommends using @ResourcesPerPage@ instead of this parameter. A limit that restricts the number of tags (key and value pairs) returned by GetResources in paginated output. A resource with no tags is counted as having one tag (one key and value pair). @GetResources@ does not split a resource and its associated tags across pages. If the specified @TagsPerPage@ would cause such a break, a @PaginationToken@ is returned in place of the affected resource and its tags. Use that token in another request to get the remaining data. For example, if you specify a @TagsPerPage@ of @100@ and the account has 22 resources with 10 tags each (meaning that each resource has 10 key and value pairs), the output will consist of three pages. The first page displays the first 10 resources, each with its 10 tags. The second page displays the next 10 resources, each with its 10 tags. The third page displays the remaining 2 resources, each with its 10 tags. You can set @TagsPerPage@ to a minimum of 100 items and the maximum of 500 items.
grTagsPerPage :: Lens' GetResources (Maybe Int)
grTagsPerPage = lens _grTagsPerPage (\s a -> s {_grTagsPerPage = a})

instance AWSPager GetResources where
  page rq rs
    | stop (rs ^. grrsPaginationToken) = Nothing
    | stop (rs ^. grrsResourceTagMappingList) = Nothing
    | otherwise =
      Just $ rq & grPaginationToken .~ rs ^. grrsPaginationToken

instance AWSRequest GetResources where
  type Rs GetResources = GetResourcesResponse
  request = postJSON resourceGroupsTagging
  response =
    receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            <$> (x .?> "PaginationToken")
            <*> (x .?> "ResourceTagMappingList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetResources

instance NFData GetResources

instance ToHeaders GetResources where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ResourceGroupsTaggingAPI_20170126.GetResources" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetResources where
  toJSON GetResources' {..} =
    object
      ( catMaybes
          [ ("PaginationToken" .=) <$> _grPaginationToken,
            ("ResourcesPerPage" .=) <$> _grResourcesPerPage,
            ("ExcludeCompliantResources" .=) <$> _grExcludeCompliantResources,
            ("IncludeComplianceDetails" .=) <$> _grIncludeComplianceDetails,
            ("ResourceTypeFilters" .=) <$> _grResourceTypeFilters,
            ("TagFilters" .=) <$> _grTagFilters,
            ("TagsPerPage" .=) <$> _grTagsPerPage
          ]
      )

instance ToPath GetResources where
  toPath = const "/"

instance ToQuery GetResources where
  toQuery = const mempty

-- | /See:/ 'getResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { _grrsPaginationToken ::
      !(Maybe Text),
    _grrsResourceTagMappingList ::
      !(Maybe [ResourceTagMapping]),
    _grrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsPaginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- * 'grrsResourceTagMappingList' - A list of resource ARNs and the tags (keys and values) associated with each.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getResourcesResponse ::
  -- | 'grrsResponseStatus'
  Int ->
  GetResourcesResponse
getResourcesResponse pResponseStatus_ =
  GetResourcesResponse'
    { _grrsPaginationToken = Nothing,
      _grrsResourceTagMappingList = Nothing,
      _grrsResponseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
grrsPaginationToken :: Lens' GetResourcesResponse (Maybe Text)
grrsPaginationToken = lens _grrsPaginationToken (\s a -> s {_grrsPaginationToken = a})

-- | A list of resource ARNs and the tags (keys and values) associated with each.
grrsResourceTagMappingList :: Lens' GetResourcesResponse [ResourceTagMapping]
grrsResourceTagMappingList = lens _grrsResourceTagMappingList (\s a -> s {_grrsResourceTagMappingList = a}) . _Default . _Coerce

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetResourcesResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\s a -> s {_grrsResponseStatus = a})

instance NFData GetResourcesResponse
