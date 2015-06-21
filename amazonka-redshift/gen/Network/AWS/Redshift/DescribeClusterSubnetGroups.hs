{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in you AWS
-- account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all subnet groups that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- subnet groups that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, subnet groups
-- are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterSubnetGroups.html>
module Network.AWS.Redshift.DescribeClusterSubnetGroups
    (
    -- * Request
      DescribeClusterSubnetGroups
    -- ** Request constructor
    , describeClusterSubnetGroups
    -- ** Request lenses
    , dTagValues
    , dTagKeys
    , dClusterSubnetGroupName
    , dMaxRecords
    , dMarker

    -- * Response
    , DescribeClusterSubnetGroupsResponse
    -- ** Response constructor
    , describeClusterSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrClusterSubnetGroups
    , dcsgrMarker
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClusterSubnetGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dTagValues'
--
-- * 'dTagKeys'
--
-- * 'dClusterSubnetGroupName'
--
-- * 'dMaxRecords'
--
-- * 'dMarker'
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups'{_dTagValues :: Maybe [Text], _dTagKeys :: Maybe [Text], _dClusterSubnetGroupName :: Maybe Text, _dMaxRecords :: Maybe Int, _dMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeClusterSubnetGroups' smart constructor.
describeClusterSubnetGroups :: DescribeClusterSubnetGroups
describeClusterSubnetGroups = DescribeClusterSubnetGroups'{_dTagValues = Nothing, _dTagKeys = Nothing, _dClusterSubnetGroupName = Nothing, _dMaxRecords = Nothing, _dMarker = Nothing};

-- | A tag value or values for which you want to return all matching cluster
-- subnet groups that are associated with the specified tag value or
-- values. For example, suppose that you have subnet groups that are tagged
-- with values called @admin@ and @test@. If you specify both of these tag
-- values in the request, Amazon Redshift returns a response with the
-- subnet groups that have either or both of these tag values associated
-- with them.
dTagValues :: Lens' DescribeClusterSubnetGroups [Text]
dTagValues = lens _dTagValues (\ s a -> s{_dTagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching cluster
-- subnet groups that are associated with the specified key or keys. For
-- example, suppose that you have subnet groups that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the subnet
-- groups that have either or both of these tag keys associated with them.
dTagKeys :: Lens' DescribeClusterSubnetGroups [Text]
dTagKeys = lens _dTagKeys (\ s a -> s{_dTagKeys = a}) . _Default;

-- | The name of the cluster subnet group for which information is requested.
dClusterSubnetGroupName :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dClusterSubnetGroupName = lens _dClusterSubnetGroupName (\ s a -> s{_dClusterSubnetGroupName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dMaxRecords :: Lens' DescribeClusterSubnetGroups (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dMarker :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a});

instance AWSPager DescribeClusterSubnetGroups where
        page rq rs
          | stop (rs ^. dcsgrMarker) = Nothing
          | otherwise = rq & dMarker ?~ rs ^. dcsgrMarker

instance AWSRequest DescribeClusterSubnetGroups where
        type Sv DescribeClusterSubnetGroups = Redshift
        type Rs DescribeClusterSubnetGroups =
             DescribeClusterSubnetGroupsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeClusterSubnetGroupsResult"
              (\ s h x ->
                 DescribeClusterSubnetGroupsResponse' <$>
                   (x .@? "ClusterSubnetGroups" .!@ mempty >>=
                      may (parseXMLList "ClusterSubnetGroup"))
                     <*> (x .@? "Marker"))

instance ToHeaders DescribeClusterSubnetGroups where
        toHeaders = const mempty

instance ToPath DescribeClusterSubnetGroups where
        toPath = const "/"

instance ToQuery DescribeClusterSubnetGroups where
        toQuery DescribeClusterSubnetGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterSubnetGroups" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dTagKeys),
               "ClusterSubnetGroupName" =: _dClusterSubnetGroupName,
               "MaxRecords" =: _dMaxRecords, "Marker" =: _dMarker]

-- | /See:/ 'describeClusterSubnetGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrClusterSubnetGroups'
--
-- * 'dcsgrMarker'
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'{_dcsgrClusterSubnetGroups :: Maybe [ClusterSubnetGroup], _dcsgrMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeClusterSubnetGroupsResponse' smart constructor.
describeClusterSubnetGroupsResponse :: DescribeClusterSubnetGroupsResponse
describeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'{_dcsgrClusterSubnetGroups = Nothing, _dcsgrMarker = Nothing};

-- | A list of ClusterSubnetGroup instances.
dcsgrClusterSubnetGroups :: Lens' DescribeClusterSubnetGroupsResponse [ClusterSubnetGroup]
dcsgrClusterSubnetGroups = lens _dcsgrClusterSubnetGroups (\ s a -> s{_dcsgrClusterSubnetGroups = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcsgrMarker :: Lens' DescribeClusterSubnetGroupsResponse (Maybe Text)
dcsgrMarker = lens _dcsgrMarker (\ s a -> s{_dcsgrMarker = a});
