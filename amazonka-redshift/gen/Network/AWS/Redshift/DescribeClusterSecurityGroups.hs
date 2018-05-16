{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Redshift security groups. If the name of a security group is specified, the response will contain only information about only that security group.
--
--
-- For information about managing security groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all security groups that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all security groups that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, security groups are returned regardless of whether they have tag keys or values associated with them.
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterSecurityGroups
    (
    -- * Creating a Request
      describeClusterSecurityGroups
    , DescribeClusterSecurityGroups
    -- * Request Lenses
    , dcsgTagValues
    , dcsgTagKeys
    , dcsgClusterSecurityGroupName
    , dcsgMarker
    , dcsgMaxRecords

    -- * Destructuring the Response
    , describeClusterSecurityGroupsResponse
    , DescribeClusterSecurityGroupsResponse
    -- * Response Lenses
    , dcsgsrsClusterSecurityGroups
    , dcsgsrsMarker
    , dcsgsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeClusterSecurityGroups' smart constructor.
data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups'
  { _dcsgTagValues                :: !(Maybe [Text])
  , _dcsgTagKeys                  :: !(Maybe [Text])
  , _dcsgClusterSecurityGroupName :: !(Maybe Text)
  , _dcsgMarker                   :: !(Maybe Text)
  , _dcsgMaxRecords               :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgTagValues' - A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
--
-- * 'dcsgTagKeys' - A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
--
-- * 'dcsgClusterSecurityGroupName' - The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.  Example: @securitygroup1@
--
-- * 'dcsgMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.  Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
--
-- * 'dcsgMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
describeClusterSecurityGroups
    :: DescribeClusterSecurityGroups
describeClusterSecurityGroups =
  DescribeClusterSecurityGroups'
    { _dcsgTagValues = Nothing
    , _dcsgTagKeys = Nothing
    , _dcsgClusterSecurityGroupName = Nothing
    , _dcsgMarker = Nothing
    , _dcsgMaxRecords = Nothing
    }


-- | A tag value or values for which you want to return all matching cluster security groups that are associated with the specified tag value or values. For example, suppose that you have security groups that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag values associated with them.
dcsgTagValues :: Lens' DescribeClusterSecurityGroups [Text]
dcsgTagValues = lens _dcsgTagValues (\ s a -> s{_dcsgTagValues = a}) . _Default . _Coerce

-- | A tag key or keys for which you want to return all matching cluster security groups that are associated with the specified key or keys. For example, suppose that you have security groups that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the security groups that have either or both of these tag keys associated with them.
dcsgTagKeys :: Lens' DescribeClusterSecurityGroups [Text]
dcsgTagKeys = lens _dcsgTagKeys (\ s a -> s{_dcsgTagKeys = a}) . _Default . _Coerce

-- | The name of a cluster security group for which you are requesting details. You can specify either the __Marker__ parameter or a __ClusterSecurityGroupName__ parameter, but not both.  Example: @securitygroup1@
dcsgClusterSecurityGroupName :: Lens' DescribeClusterSecurityGroups (Maybe Text)
dcsgClusterSecurityGroupName = lens _dcsgClusterSecurityGroupName (\ s a -> s{_dcsgClusterSecurityGroupName = a})

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSecurityGroups' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.  Constraints: You can specify either the __ClusterSecurityGroupName__ parameter or the __Marker__ parameter, but not both.
dcsgMarker :: Lens' DescribeClusterSecurityGroups (Maybe Text)
dcsgMarker = lens _dcsgMarker (\ s a -> s{_dcsgMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dcsgMaxRecords :: Lens' DescribeClusterSecurityGroups (Maybe Int)
dcsgMaxRecords = lens _dcsgMaxRecords (\ s a -> s{_dcsgMaxRecords = a})

instance AWSPager DescribeClusterSecurityGroups where
        page rq rs
          | stop (rs ^. dcsgsrsMarker) = Nothing
          | stop (rs ^. dcsgsrsClusterSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgMarker .~ rs ^. dcsgsrsMarker

instance AWSRequest DescribeClusterSecurityGroups
         where
        type Rs DescribeClusterSecurityGroups =
             DescribeClusterSecurityGroupsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "DescribeClusterSecurityGroupsResult"
              (\ s h x ->
                 DescribeClusterSecurityGroupsResponse' <$>
                   (x .@? "ClusterSecurityGroups" .!@ mempty >>=
                      may (parseXMLList "ClusterSecurityGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusterSecurityGroups where

instance NFData DescribeClusterSecurityGroups where

instance ToHeaders DescribeClusterSecurityGroups
         where
        toHeaders = const mempty

instance ToPath DescribeClusterSecurityGroups where
        toPath = const "/"

instance ToQuery DescribeClusterSecurityGroups where
        toQuery DescribeClusterSecurityGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterSecurityGroups" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dcsgTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcsgTagKeys),
               "ClusterSecurityGroupName" =:
                 _dcsgClusterSecurityGroupName,
               "Marker" =: _dcsgMarker,
               "MaxRecords" =: _dcsgMaxRecords]

-- |
--
--
--
-- /See:/ 'describeClusterSecurityGroupsResponse' smart constructor.
data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse'
  { _dcsgsrsClusterSecurityGroups :: !(Maybe [ClusterSecurityGroup])
  , _dcsgsrsMarker                :: !(Maybe Text)
  , _dcsgsrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgsrsClusterSecurityGroups' - A list of 'ClusterSecurityGroup' instances.
--
-- * 'dcsgsrsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dcsgsrsResponseStatus' - -- | The response status code.
describeClusterSecurityGroupsResponse
    :: Int -- ^ 'dcsgsrsResponseStatus'
    -> DescribeClusterSecurityGroupsResponse
describeClusterSecurityGroupsResponse pResponseStatus_ =
  DescribeClusterSecurityGroupsResponse'
    { _dcsgsrsClusterSecurityGroups = Nothing
    , _dcsgsrsMarker = Nothing
    , _dcsgsrsResponseStatus = pResponseStatus_
    }


-- | A list of 'ClusterSecurityGroup' instances.
dcsgsrsClusterSecurityGroups :: Lens' DescribeClusterSecurityGroupsResponse [ClusterSecurityGroup]
dcsgsrsClusterSecurityGroups = lens _dcsgsrsClusterSecurityGroups (\ s a -> s{_dcsgsrsClusterSecurityGroups = a}) . _Default . _Coerce

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dcsgsrsMarker :: Lens' DescribeClusterSecurityGroupsResponse (Maybe Text)
dcsgsrsMarker = lens _dcsgsrsMarker (\ s a -> s{_dcsgsrsMarker = a})

-- | -- | The response status code.
dcsgsrsResponseStatus :: Lens' DescribeClusterSecurityGroupsResponse Int
dcsgsrsResponseStatus = lens _dcsgsrsResponseStatus (\ s a -> s{_dcsgsrsResponseStatus = a})

instance NFData DescribeClusterSecurityGroupsResponse
         where
