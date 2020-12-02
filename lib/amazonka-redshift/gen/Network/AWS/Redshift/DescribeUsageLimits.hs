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
-- Module      : Network.AWS.Redshift.DescribeUsageLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shows usage limits on a cluster. Results are filtered based on the combination of input usage limit identifier, cluster identifier, and feature type parameters:
--
--
--     * If usage limit identifier, cluster identifier, and feature type are not provided, then all usage limit objects for the current account in the current region are returned.
--
--     * If usage limit identifier is provided, then the corresponding usage limit object is returned.
--
--     * If cluster identifier is provided, then all usage limit objects for the specified cluster are returned.
--
--     * If cluster identifier and feature type are provided, then all usage limit objects for the combination of cluster and feature are returned.
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeUsageLimits
  ( -- * Creating a Request
    describeUsageLimits,
    DescribeUsageLimits,

    -- * Request Lenses
    dulsTagValues,
    dulsUsageLimitId,
    dulsTagKeys,
    dulsClusterIdentifier,
    dulsFeatureType,
    dulsMarker,
    dulsMaxRecords,

    -- * Destructuring the Response
    describeUsageLimitsResponse,
    DescribeUsageLimitsResponse,

    -- * Response Lenses
    dulrsUsageLimits,
    dulrsMarker,
    dulrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUsageLimits' smart constructor.
data DescribeUsageLimits = DescribeUsageLimits'
  { _dulsTagValues ::
      !(Maybe [Text]),
    _dulsUsageLimitId :: !(Maybe Text),
    _dulsTagKeys :: !(Maybe [Text]),
    _dulsClusterIdentifier :: !(Maybe Text),
    _dulsFeatureType :: !(Maybe UsageLimitFeatureType),
    _dulsMarker :: !(Maybe Text),
    _dulsMaxRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUsageLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dulsTagValues' - A tag value or values for which you want to return all matching usage limit objects that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the usage limit objects that have either or both of these tag values associated with them.
--
-- * 'dulsUsageLimitId' - The identifier of the usage limit to describe.
--
-- * 'dulsTagKeys' - A tag key or keys for which you want to return all matching usage limit objects that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the usage limit objects have either or both of these tag keys associated with them.
--
-- * 'dulsClusterIdentifier' - The identifier of the cluster for which you want to describe usage limits.
--
-- * 'dulsFeatureType' - The feature type for which you want to describe usage limits.
--
-- * 'dulsMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeUsageLimits' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dulsMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
describeUsageLimits ::
  DescribeUsageLimits
describeUsageLimits =
  DescribeUsageLimits'
    { _dulsTagValues = Nothing,
      _dulsUsageLimitId = Nothing,
      _dulsTagKeys = Nothing,
      _dulsClusterIdentifier = Nothing,
      _dulsFeatureType = Nothing,
      _dulsMarker = Nothing,
      _dulsMaxRecords = Nothing
    }

-- | A tag value or values for which you want to return all matching usage limit objects that are associated with the specified tag value or values. For example, suppose that you have parameter groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the usage limit objects that have either or both of these tag values associated with them.
dulsTagValues :: Lens' DescribeUsageLimits [Text]
dulsTagValues = lens _dulsTagValues (\s a -> s {_dulsTagValues = a}) . _Default . _Coerce

-- | The identifier of the usage limit to describe.
dulsUsageLimitId :: Lens' DescribeUsageLimits (Maybe Text)
dulsUsageLimitId = lens _dulsUsageLimitId (\s a -> s {_dulsUsageLimitId = a})

-- | A tag key or keys for which you want to return all matching usage limit objects that are associated with the specified key or keys. For example, suppose that you have parameter groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the usage limit objects have either or both of these tag keys associated with them.
dulsTagKeys :: Lens' DescribeUsageLimits [Text]
dulsTagKeys = lens _dulsTagKeys (\s a -> s {_dulsTagKeys = a}) . _Default . _Coerce

-- | The identifier of the cluster for which you want to describe usage limits.
dulsClusterIdentifier :: Lens' DescribeUsageLimits (Maybe Text)
dulsClusterIdentifier = lens _dulsClusterIdentifier (\s a -> s {_dulsClusterIdentifier = a})

-- | The feature type for which you want to describe usage limits.
dulsFeatureType :: Lens' DescribeUsageLimits (Maybe UsageLimitFeatureType)
dulsFeatureType = lens _dulsFeatureType (\s a -> s {_dulsFeatureType = a})

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeUsageLimits' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dulsMarker :: Lens' DescribeUsageLimits (Maybe Text)
dulsMarker = lens _dulsMarker (\s a -> s {_dulsMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dulsMaxRecords :: Lens' DescribeUsageLimits (Maybe Int)
dulsMaxRecords = lens _dulsMaxRecords (\s a -> s {_dulsMaxRecords = a})

instance AWSPager DescribeUsageLimits where
  page rq rs
    | stop (rs ^. dulrsMarker) = Nothing
    | stop (rs ^. dulrsUsageLimits) = Nothing
    | otherwise = Just $ rq & dulsMarker .~ rs ^. dulrsMarker

instance AWSRequest DescribeUsageLimits where
  type Rs DescribeUsageLimits = DescribeUsageLimitsResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "DescribeUsageLimitsResult"
      ( \s h x ->
          DescribeUsageLimitsResponse'
            <$> (x .@? "UsageLimits" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeUsageLimits

instance NFData DescribeUsageLimits

instance ToHeaders DescribeUsageLimits where
  toHeaders = const mempty

instance ToPath DescribeUsageLimits where
  toPath = const "/"

instance ToQuery DescribeUsageLimits where
  toQuery DescribeUsageLimits' {..} =
    mconcat
      [ "Action" =: ("DescribeUsageLimits" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "TagValues" =: toQuery (toQueryList "TagValue" <$> _dulsTagValues),
        "UsageLimitId" =: _dulsUsageLimitId,
        "TagKeys" =: toQuery (toQueryList "TagKey" <$> _dulsTagKeys),
        "ClusterIdentifier" =: _dulsClusterIdentifier,
        "FeatureType" =: _dulsFeatureType,
        "Marker" =: _dulsMarker,
        "MaxRecords" =: _dulsMaxRecords
      ]

-- | /See:/ 'describeUsageLimitsResponse' smart constructor.
data DescribeUsageLimitsResponse = DescribeUsageLimitsResponse'
  { _dulrsUsageLimits ::
      !(Maybe [UsageLimit]),
    _dulrsMarker :: !(Maybe Text),
    _dulrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUsageLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dulrsUsageLimits' - Contains the output from the 'DescribeUsageLimits' action.
--
-- * 'dulrsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dulrsResponseStatus' - -- | The response status code.
describeUsageLimitsResponse ::
  -- | 'dulrsResponseStatus'
  Int ->
  DescribeUsageLimitsResponse
describeUsageLimitsResponse pResponseStatus_ =
  DescribeUsageLimitsResponse'
    { _dulrsUsageLimits = Nothing,
      _dulrsMarker = Nothing,
      _dulrsResponseStatus = pResponseStatus_
    }

-- | Contains the output from the 'DescribeUsageLimits' action.
dulrsUsageLimits :: Lens' DescribeUsageLimitsResponse [UsageLimit]
dulrsUsageLimits = lens _dulrsUsageLimits (\s a -> s {_dulrsUsageLimits = a}) . _Default . _Coerce

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dulrsMarker :: Lens' DescribeUsageLimitsResponse (Maybe Text)
dulrsMarker = lens _dulrsMarker (\s a -> s {_dulrsMarker = a})

-- | -- | The response status code.
dulrsResponseStatus :: Lens' DescribeUsageLimitsResponse Int
dulrsResponseStatus = lens _dulrsResponseStatus (\s a -> s {_dulrsResponseStatus = a})

instance NFData DescribeUsageLimitsResponse
