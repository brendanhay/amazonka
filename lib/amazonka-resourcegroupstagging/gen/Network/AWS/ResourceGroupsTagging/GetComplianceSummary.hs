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
-- Module      : Network.AWS.ResourceGroupsTagging.GetComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a table that shows counts of resources that are noncompliant with their tag policies.
--
--
-- For more information on tag policies, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html Tag Policies> in the /AWS Organizations User Guide./
--
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetComplianceSummary
  ( -- * Creating a Request
    getComplianceSummary,
    GetComplianceSummary,

    -- * Request Lenses
    gcsGroupBy,
    gcsPaginationToken,
    gcsTargetIdFilters,
    gcsResourceTypeFilters,
    gcsRegionFilters,
    gcsTagKeyFilters,
    gcsMaxResults,

    -- * Destructuring the Response
    getComplianceSummaryResponse,
    GetComplianceSummaryResponse,

    -- * Response Lenses
    gcsrsPaginationToken,
    gcsrsSummaryList,
    gcsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.Response

-- | /See:/ 'getComplianceSummary' smart constructor.
data GetComplianceSummary = GetComplianceSummary'
  { _gcsGroupBy ::
      !(Maybe [GroupByAttribute]),
    _gcsPaginationToken :: !(Maybe Text),
    _gcsTargetIdFilters :: !(Maybe (List1 Text)),
    _gcsResourceTypeFilters :: !(Maybe [Text]),
    _gcsRegionFilters :: !(Maybe (List1 Text)),
    _gcsTagKeyFilters :: !(Maybe (List1 Text)),
    _gcsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetComplianceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsGroupBy' - A list of attributes to group the counts of noncompliant resources by. If supplied, the counts are sorted by those attributes.
--
-- * 'gcsPaginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- * 'gcsTargetIdFilters' - The target identifiers (usually, specific account IDs) to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources with the specified target IDs.
--
-- * 'gcsResourceTypeFilters' - The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.  The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter.
--
-- * 'gcsRegionFilters' - A list of Regions to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources in the specified Regions.
--
-- * 'gcsTagKeyFilters' - A list of tag keys to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources that have the specified tag keys.
--
-- * 'gcsMaxResults' - A limit that restricts the number of results that are returned per page.
getComplianceSummary ::
  GetComplianceSummary
getComplianceSummary =
  GetComplianceSummary'
    { _gcsGroupBy = Nothing,
      _gcsPaginationToken = Nothing,
      _gcsTargetIdFilters = Nothing,
      _gcsResourceTypeFilters = Nothing,
      _gcsRegionFilters = Nothing,
      _gcsTagKeyFilters = Nothing,
      _gcsMaxResults = Nothing
    }

-- | A list of attributes to group the counts of noncompliant resources by. If supplied, the counts are sorted by those attributes.
gcsGroupBy :: Lens' GetComplianceSummary [GroupByAttribute]
gcsGroupBy = lens _gcsGroupBy (\s a -> s {_gcsGroupBy = a}) . _Default . _Coerce

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
gcsPaginationToken :: Lens' GetComplianceSummary (Maybe Text)
gcsPaginationToken = lens _gcsPaginationToken (\s a -> s {_gcsPaginationToken = a})

-- | The target identifiers (usually, specific account IDs) to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources with the specified target IDs.
gcsTargetIdFilters :: Lens' GetComplianceSummary (Maybe (NonEmpty Text))
gcsTargetIdFilters = lens _gcsTargetIdFilters (\s a -> s {_gcsTargetIdFilters = a}) . mapping _List1

-- | The constraints on the resources that you want returned. The format of each resource type is @service[:resourceType]@ . For example, specifying a resource type of @ec2@ returns all Amazon EC2 resources (which includes EC2 instances). Specifying a resource type of @ec2:instance@ returns only EC2 instances.  The string for each service name and resource type is the same as that embedded in a resource's Amazon Resource Name (ARN). Consult the /AWS General Reference/ for the following:     * For a list of service name strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> .     * For resource type strings, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arns-syntax Example ARNs> .     * For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> . You can specify multiple resource types by using an array. The array can include up to 100 items. Note that the length constraint requirement applies to each resource type filter.
gcsResourceTypeFilters :: Lens' GetComplianceSummary [Text]
gcsResourceTypeFilters = lens _gcsResourceTypeFilters (\s a -> s {_gcsResourceTypeFilters = a}) . _Default . _Coerce

-- | A list of Regions to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources in the specified Regions.
gcsRegionFilters :: Lens' GetComplianceSummary (Maybe (NonEmpty Text))
gcsRegionFilters = lens _gcsRegionFilters (\s a -> s {_gcsRegionFilters = a}) . mapping _List1

-- | A list of tag keys to limit the output by. If you use this parameter, the count of returned noncompliant resources includes only resources that have the specified tag keys.
gcsTagKeyFilters :: Lens' GetComplianceSummary (Maybe (NonEmpty Text))
gcsTagKeyFilters = lens _gcsTagKeyFilters (\s a -> s {_gcsTagKeyFilters = a}) . mapping _List1

-- | A limit that restricts the number of results that are returned per page.
gcsMaxResults :: Lens' GetComplianceSummary (Maybe Natural)
gcsMaxResults = lens _gcsMaxResults (\s a -> s {_gcsMaxResults = a}) . mapping _Nat

instance AWSPager GetComplianceSummary where
  page rq rs
    | stop (rs ^. gcsrsPaginationToken) = Nothing
    | stop (rs ^. gcsrsSummaryList) = Nothing
    | otherwise =
      Just $ rq & gcsPaginationToken .~ rs ^. gcsrsPaginationToken

instance AWSRequest GetComplianceSummary where
  type Rs GetComplianceSummary = GetComplianceSummaryResponse
  request = postJSON resourceGroupsTagging
  response =
    receiveJSON
      ( \s h x ->
          GetComplianceSummaryResponse'
            <$> (x .?> "PaginationToken")
            <*> (x .?> "SummaryList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetComplianceSummary

instance NFData GetComplianceSummary

instance ToHeaders GetComplianceSummary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "ResourceGroupsTaggingAPI_20170126.GetComplianceSummary" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetComplianceSummary where
  toJSON GetComplianceSummary' {..} =
    object
      ( catMaybes
          [ ("GroupBy" .=) <$> _gcsGroupBy,
            ("PaginationToken" .=) <$> _gcsPaginationToken,
            ("TargetIdFilters" .=) <$> _gcsTargetIdFilters,
            ("ResourceTypeFilters" .=) <$> _gcsResourceTypeFilters,
            ("RegionFilters" .=) <$> _gcsRegionFilters,
            ("TagKeyFilters" .=) <$> _gcsTagKeyFilters,
            ("MaxResults" .=) <$> _gcsMaxResults
          ]
      )

instance ToPath GetComplianceSummary where
  toPath = const "/"

instance ToQuery GetComplianceSummary where
  toQuery = const mempty

-- | /See:/ 'getComplianceSummaryResponse' smart constructor.
data GetComplianceSummaryResponse = GetComplianceSummaryResponse'
  { _gcsrsPaginationToken ::
      !(Maybe Text),
    _gcsrsSummaryList ::
      !(Maybe [Summary]),
    _gcsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetComplianceSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsPaginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- * 'gcsrsSummaryList' - A table that shows counts of noncompliant resources.
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
getComplianceSummaryResponse ::
  -- | 'gcsrsResponseStatus'
  Int ->
  GetComplianceSummaryResponse
getComplianceSummaryResponse pResponseStatus_ =
  GetComplianceSummaryResponse'
    { _gcsrsPaginationToken = Nothing,
      _gcsrsSummaryList = Nothing,
      _gcsrsResponseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
gcsrsPaginationToken :: Lens' GetComplianceSummaryResponse (Maybe Text)
gcsrsPaginationToken = lens _gcsrsPaginationToken (\s a -> s {_gcsrsPaginationToken = a})

-- | A table that shows counts of noncompliant resources.
gcsrsSummaryList :: Lens' GetComplianceSummaryResponse [Summary]
gcsrsSummaryList = lens _gcsrsSummaryList (\s a -> s {_gcsrsSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GetComplianceSummaryResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\s a -> s {_gcsrsResponseStatus = a})

instance NFData GetComplianceSummaryResponse
