{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more cluster subnet group objects, which contain metadata
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
    , dcsgsTagValues
    , dcsgsTagKeys
    , dcsgsClusterSubnetGroupName
    , dcsgsMaxRecords
    , dcsgsMarker

    -- * Response
    , DescribeClusterSubnetGroupsResponse
    -- ** Response constructor
    , describeClusterSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrsClusterSubnetGroups
    , dcsgrsMarker
    , dcsgrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeClusterSubnetGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgsTagValues'
--
-- * 'dcsgsTagKeys'
--
-- * 'dcsgsClusterSubnetGroupName'
--
-- * 'dcsgsMaxRecords'
--
-- * 'dcsgsMarker'
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups'
    { _dcsgsTagValues              :: !(Maybe [Text])
    , _dcsgsTagKeys                :: !(Maybe [Text])
    , _dcsgsClusterSubnetGroupName :: !(Maybe Text)
    , _dcsgsMaxRecords             :: !(Maybe Int)
    , _dcsgsMarker                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSubnetGroups' smart constructor.
describeClusterSubnetGroups :: DescribeClusterSubnetGroups
describeClusterSubnetGroups =
    DescribeClusterSubnetGroups'
    { _dcsgsTagValues = Nothing
    , _dcsgsTagKeys = Nothing
    , _dcsgsClusterSubnetGroupName = Nothing
    , _dcsgsMaxRecords = Nothing
    , _dcsgsMarker = Nothing
    }

-- | A tag value or values for which you want to return all matching cluster
-- subnet groups that are associated with the specified tag value or
-- values. For example, suppose that you have subnet groups that are tagged
-- with values called @admin@ and @test@. If you specify both of these tag
-- values in the request, Amazon Redshift returns a response with the
-- subnet groups that have either or both of these tag values associated
-- with them.
dcsgsTagValues :: Lens' DescribeClusterSubnetGroups [Text]
dcsgsTagValues = lens _dcsgsTagValues (\ s a -> s{_dcsgsTagValues = a}) . _Default . _Coerce;

-- | A tag key or keys for which you want to return all matching cluster
-- subnet groups that are associated with the specified key or keys. For
-- example, suppose that you have subnet groups that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the subnet
-- groups that have either or both of these tag keys associated with them.
dcsgsTagKeys :: Lens' DescribeClusterSubnetGroups [Text]
dcsgsTagKeys = lens _dcsgsTagKeys (\ s a -> s{_dcsgsTagKeys = a}) . _Default . _Coerce;

-- | The name of the cluster subnet group for which information is requested.
dcsgsClusterSubnetGroupName :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsgsClusterSubnetGroupName = lens _dcsgsClusterSubnetGroupName (\ s a -> s{_dcsgsClusterSubnetGroupName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcsgsMaxRecords :: Lens' DescribeClusterSubnetGroups (Maybe Int)
dcsgsMaxRecords = lens _dcsgsMaxRecords (\ s a -> s{_dcsgsMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcsgsMarker :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsgsMarker = lens _dcsgsMarker (\ s a -> s{_dcsgsMarker = a});

instance AWSPager DescribeClusterSubnetGroups where
        page rq rs
          | stop (rs ^. dcsgrsMarker) = Nothing
          | stop (rs ^. dcsgrsClusterSubnetGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgsMarker .~ rs ^. dcsgrsMarker

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
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
                 toQuery (toQueryList "TagValue" <$> _dcsgsTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcsgsTagKeys),
               "ClusterSubnetGroupName" =:
                 _dcsgsClusterSubnetGroupName,
               "MaxRecords" =: _dcsgsMaxRecords,
               "Marker" =: _dcsgsMarker]

-- | Contains the output from the DescribeClusterSubnetGroups action.
--
-- /See:/ 'describeClusterSubnetGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrsClusterSubnetGroups'
--
-- * 'dcsgrsMarker'
--
-- * 'dcsgrsStatus'
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse'
    { _dcsgrsClusterSubnetGroups :: !(Maybe [ClusterSubnetGroup])
    , _dcsgrsMarker              :: !(Maybe Text)
    , _dcsgrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSubnetGroupsResponse' smart constructor.
describeClusterSubnetGroupsResponse :: Int -> DescribeClusterSubnetGroupsResponse
describeClusterSubnetGroupsResponse pStatus_ =
    DescribeClusterSubnetGroupsResponse'
    { _dcsgrsClusterSubnetGroups = Nothing
    , _dcsgrsMarker = Nothing
    , _dcsgrsStatus = pStatus_
    }

-- | A list of ClusterSubnetGroup instances.
dcsgrsClusterSubnetGroups :: Lens' DescribeClusterSubnetGroupsResponse [ClusterSubnetGroup]
dcsgrsClusterSubnetGroups = lens _dcsgrsClusterSubnetGroups (\ s a -> s{_dcsgrsClusterSubnetGroups = a}) . _Default . _Coerce;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcsgrsMarker :: Lens' DescribeClusterSubnetGroupsResponse (Maybe Text)
dcsgrsMarker = lens _dcsgrsMarker (\ s a -> s{_dcsgrsMarker = a});

-- | FIXME: Undocumented member.
dcsgrsStatus :: Lens' DescribeClusterSubnetGroupsResponse Int
dcsgrsStatus = lens _dcsgrsStatus (\ s a -> s{_dcsgrsStatus = a});
