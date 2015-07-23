{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Redshift security groups. If the name
-- of a security group is specified, the response will contain only
-- information about only that security group.
--
-- For information about managing security groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all security groups that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- security groups that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, security
-- groups are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterSecurityGroups.html>
module Network.AWS.Redshift.DescribeClusterSecurityGroups
    (
    -- * Request
      DescribeClusterSecurityGroups
    -- ** Request constructor
    , describeClusterSecurityGroups
    -- ** Request lenses
    , dcsgrqTagValues
    , dcsgrqTagKeys
    , dcsgrqClusterSecurityGroupName
    , dcsgrqMaxRecords
    , dcsgrqMarker

    -- * Response
    , DescribeClusterSecurityGroupsResponse
    -- ** Response constructor
    , describeClusterSecurityGroupsResponse
    -- ** Response lenses
    , dcsgsrsClusterSecurityGroups
    , dcsgsrsMarker
    , dcsgsrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | ???
--
-- /See:/ 'describeClusterSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrqTagValues'
--
-- * 'dcsgrqTagKeys'
--
-- * 'dcsgrqClusterSecurityGroupName'
--
-- * 'dcsgrqMaxRecords'
--
-- * 'dcsgrqMarker'
data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups'
    { _dcsgrqTagValues                :: !(Maybe [Text])
    , _dcsgrqTagKeys                  :: !(Maybe [Text])
    , _dcsgrqClusterSecurityGroupName :: !(Maybe Text)
    , _dcsgrqMaxRecords               :: !(Maybe Int)
    , _dcsgrqMarker                   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSecurityGroups' smart constructor.
describeClusterSecurityGroups :: DescribeClusterSecurityGroups
describeClusterSecurityGroups =
    DescribeClusterSecurityGroups'
    { _dcsgrqTagValues = Nothing
    , _dcsgrqTagKeys = Nothing
    , _dcsgrqClusterSecurityGroupName = Nothing
    , _dcsgrqMaxRecords = Nothing
    , _dcsgrqMarker = Nothing
    }

-- | A tag value or values for which you want to return all matching cluster
-- security groups that are associated with the specified tag value or
-- values. For example, suppose that you have security groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the security groups that have either or both of these tag values
-- associated with them.
dcsgrqTagValues :: Lens' DescribeClusterSecurityGroups [Text]
dcsgrqTagValues = lens _dcsgrqTagValues (\ s a -> s{_dcsgrqTagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching cluster
-- security groups that are associated with the specified key or keys. For
-- example, suppose that you have security groups that are tagged with keys
-- called @owner@ and @environment@. If you specify both of these tag keys
-- in the request, Amazon Redshift returns a response with the security
-- groups that have either or both of these tag keys associated with them.
dcsgrqTagKeys :: Lens' DescribeClusterSecurityGroups [Text]
dcsgrqTagKeys = lens _dcsgrqTagKeys (\ s a -> s{_dcsgrqTagKeys = a}) . _Default;

-- | The name of a cluster security group for which you are requesting
-- details. You can specify either the __Marker__ parameter or a
-- __ClusterSecurityGroupName__ parameter, but not both.
--
-- Example: @securitygroup1@
dcsgrqClusterSecurityGroupName :: Lens' DescribeClusterSecurityGroups (Maybe Text)
dcsgrqClusterSecurityGroupName = lens _dcsgrqClusterSecurityGroupName (\ s a -> s{_dcsgrqClusterSecurityGroupName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcsgrqMaxRecords :: Lens' DescribeClusterSecurityGroups (Maybe Int)
dcsgrqMaxRecords = lens _dcsgrqMaxRecords (\ s a -> s{_dcsgrqMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSecurityGroups
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterSecurityGroupName__
-- parameter or the __Marker__ parameter, but not both.
dcsgrqMarker :: Lens' DescribeClusterSecurityGroups (Maybe Text)
dcsgrqMarker = lens _dcsgrqMarker (\ s a -> s{_dcsgrqMarker = a});

instance AWSPager DescribeClusterSecurityGroups where
        page rq rs
          | stop (rs ^. dcsgsrsMarker) = Nothing
          | stop (rs ^. dcsgsrsClusterSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgrqMarker .~ rs ^. dcsgsrsMarker

instance AWSRequest DescribeClusterSecurityGroups
         where
        type Sv DescribeClusterSecurityGroups = Redshift
        type Rs DescribeClusterSecurityGroups =
             DescribeClusterSecurityGroupsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeClusterSecurityGroupsResult"
              (\ s h x ->
                 DescribeClusterSecurityGroupsResponse' <$>
                   (x .@? "ClusterSecurityGroups" .!@ mempty >>=
                      may (parseXMLList "ClusterSecurityGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
                 toQuery
                   (toQueryList "TagValue" <$> _dcsgrqTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcsgrqTagKeys),
               "ClusterSecurityGroupName" =:
                 _dcsgrqClusterSecurityGroupName,
               "MaxRecords" =: _dcsgrqMaxRecords,
               "Marker" =: _dcsgrqMarker]

-- | Contains the output from the DescribeClusterSecurityGroups action.
--
-- /See:/ 'describeClusterSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgsrsClusterSecurityGroups'
--
-- * 'dcsgsrsMarker'
--
-- * 'dcsgsrsStatus'
data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse'
    { _dcsgsrsClusterSecurityGroups :: !(Maybe [ClusterSecurityGroup])
    , _dcsgsrsMarker                :: !(Maybe Text)
    , _dcsgsrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterSecurityGroupsResponse' smart constructor.
describeClusterSecurityGroupsResponse :: Int -> DescribeClusterSecurityGroupsResponse
describeClusterSecurityGroupsResponse pStatus_ =
    DescribeClusterSecurityGroupsResponse'
    { _dcsgsrsClusterSecurityGroups = Nothing
    , _dcsgsrsMarker = Nothing
    , _dcsgsrsStatus = pStatus_
    }

-- | A list of ClusterSecurityGroup instances.
dcsgsrsClusterSecurityGroups :: Lens' DescribeClusterSecurityGroupsResponse [ClusterSecurityGroup]
dcsgsrsClusterSecurityGroups = lens _dcsgsrsClusterSecurityGroups (\ s a -> s{_dcsgsrsClusterSecurityGroups = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcsgsrsMarker :: Lens' DescribeClusterSecurityGroupsResponse (Maybe Text)
dcsgsrsMarker = lens _dcsgsrsMarker (\ s a -> s{_dcsgsrsMarker = a});

-- | FIXME: Undocumented member.
dcsgsrsStatus :: Lens' DescribeClusterSecurityGroupsResponse Int
dcsgsrsStatus = lens _dcsgsrsStatus (\ s a -> s{_dcsgsrsStatus = a});
