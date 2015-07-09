{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Returns a list of Amazon Redshift parameter groups, including parameter
-- groups you created and the default parameter group. For each parameter
-- group, the response includes the parameter group name, description, and
-- parameter group family name. You can optionally specify a name to
-- retrieve the description of a specific parameter group.
--
-- For more information about parameters and parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all parameter groups that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- parameter groups that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, parameter
-- groups are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterParameterGroups.html>
module Network.AWS.Redshift.DescribeClusterParameterGroups
    (
    -- * Request
      DescribeClusterParameterGroups
    -- ** Request constructor
    , describeClusterParameterGroups
    -- ** Request lenses
    , dcpgTagValues
    , dcpgTagKeys
    , dcpgMaxRecords
    , dcpgMarker
    , dcpgParameterGroupName

    -- * Response
    , DescribeClusterParameterGroupsResponse
    -- ** Response constructor
    , describeClusterParameterGroupsResponse
    -- ** Response lenses
    , dcpgrMarker
    , dcpgrParameterGroups
    , dcpgrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeClusterParameterGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgTagValues'
--
-- * 'dcpgTagKeys'
--
-- * 'dcpgMaxRecords'
--
-- * 'dcpgMarker'
--
-- * 'dcpgParameterGroupName'
data DescribeClusterParameterGroups = DescribeClusterParameterGroups'
    { _dcpgTagValues          :: !(Maybe [Text])
    , _dcpgTagKeys            :: !(Maybe [Text])
    , _dcpgMaxRecords         :: !(Maybe Int)
    , _dcpgMarker             :: !(Maybe Text)
    , _dcpgParameterGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterParameterGroups' smart constructor.
describeClusterParameterGroups :: DescribeClusterParameterGroups
describeClusterParameterGroups =
    DescribeClusterParameterGroups'
    { _dcpgTagValues = Nothing
    , _dcpgTagKeys = Nothing
    , _dcpgMaxRecords = Nothing
    , _dcpgMarker = Nothing
    , _dcpgParameterGroupName = Nothing
    }

-- | A tag value or values for which you want to return all matching cluster
-- parameter groups that are associated with the specified tag value or
-- values. For example, suppose that you have parameter groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the parameter groups that have either or both of these tag values
-- associated with them.
dcpgTagValues :: Lens' DescribeClusterParameterGroups [Text]
dcpgTagValues = lens _dcpgTagValues (\ s a -> s{_dcpgTagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching cluster
-- parameter groups that are associated with the specified key or keys. For
-- example, suppose that you have parameter groups that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the
-- parameter groups that have either or both of these tag keys associated
-- with them.
dcpgTagKeys :: Lens' DescribeClusterParameterGroups [Text]
dcpgTagKeys = lens _dcpgTagKeys (\ s a -> s{_dcpgTagKeys = a}) . _Default;

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcpgMaxRecords :: Lens' DescribeClusterParameterGroups (Maybe Int)
dcpgMaxRecords = lens _dcpgMaxRecords (\ s a -> s{_dcpgMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeClusterParameterGroups request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
dcpgMarker :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgMarker = lens _dcpgMarker (\ s a -> s{_dcpgMarker = a});

-- | The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter
-- group are returned.
dcpgParameterGroupName :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgParameterGroupName = lens _dcpgParameterGroupName (\ s a -> s{_dcpgParameterGroupName = a});

instance AWSPager DescribeClusterParameterGroups
         where
        page rq rs
          | stop (rs ^. dcpgrMarker) = Nothing
          | stop (rs ^. dcpgrParameterGroups) = Nothing
          | otherwise =
            Just $ rq & dcpgMarker .~ rs ^. dcpgrMarker

instance AWSRequest DescribeClusterParameterGroups
         where
        type Sv DescribeClusterParameterGroups = Redshift
        type Rs DescribeClusterParameterGroups =
             DescribeClusterParameterGroupsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeClusterParameterGroupsResult"
              (\ s h x ->
                 DescribeClusterParameterGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ParameterGroups" .!@ mempty >>=
                        may (parseXMLList "ClusterParameterGroup"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeClusterParameterGroups
         where
        toHeaders = const mempty

instance ToPath DescribeClusterParameterGroups where
        toPath = const "/"

instance ToQuery DescribeClusterParameterGroups where
        toQuery DescribeClusterParameterGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterParameterGroups" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dcpgTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcpgTagKeys),
               "MaxRecords" =: _dcpgMaxRecords,
               "Marker" =: _dcpgMarker,
               "ParameterGroupName" =: _dcpgParameterGroupName]

-- | Contains the output from the DescribeClusterParameterGroups action.
--
-- /See:/ 'describeClusterParameterGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgrMarker'
--
-- * 'dcpgrParameterGroups'
--
-- * 'dcpgrStatus'
data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse'
    { _dcpgrMarker          :: !(Maybe Text)
    , _dcpgrParameterGroups :: !(Maybe [ClusterParameterGroup])
    , _dcpgrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterParameterGroupsResponse' smart constructor.
describeClusterParameterGroupsResponse :: Int -> DescribeClusterParameterGroupsResponse
describeClusterParameterGroupsResponse pStatus =
    DescribeClusterParameterGroupsResponse'
    { _dcpgrMarker = Nothing
    , _dcpgrParameterGroups = Nothing
    , _dcpgrStatus = pStatus
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcpgrMarker :: Lens' DescribeClusterParameterGroupsResponse (Maybe Text)
dcpgrMarker = lens _dcpgrMarker (\ s a -> s{_dcpgrMarker = a});

-- | A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
dcpgrParameterGroups :: Lens' DescribeClusterParameterGroupsResponse [ClusterParameterGroup]
dcpgrParameterGroups = lens _dcpgrParameterGroups (\ s a -> s{_dcpgrParameterGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dcpgrStatus :: Lens' DescribeClusterParameterGroupsResponse Int
dcpgrStatus = lens _dcpgrStatus (\ s a -> s{_dcpgrStatus = a});
