{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Amazon Redshift parameter groups, including parameter
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
    , dcpgrqTagValues
    , dcpgrqTagKeys
    , dcpgrqMaxRecords
    , dcpgrqMarker
    , dcpgrqParameterGroupName

    -- * Response
    , DescribeClusterParameterGroupsResponse
    -- ** Response constructor
    , describeClusterParameterGroupsResponse
    -- ** Response lenses
    , dcpgrsMarker
    , dcpgrsParameterGroups
    , dcpgrsStatus
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
-- * 'dcpgrqTagValues'
--
-- * 'dcpgrqTagKeys'
--
-- * 'dcpgrqMaxRecords'
--
-- * 'dcpgrqMarker'
--
-- * 'dcpgrqParameterGroupName'
data DescribeClusterParameterGroups = DescribeClusterParameterGroups'
    { _dcpgrqTagValues          :: !(Maybe [Text])
    , _dcpgrqTagKeys            :: !(Maybe [Text])
    , _dcpgrqMaxRecords         :: !(Maybe Int)
    , _dcpgrqMarker             :: !(Maybe Text)
    , _dcpgrqParameterGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterParameterGroups' smart constructor.
describeClusterParameterGroups :: DescribeClusterParameterGroups
describeClusterParameterGroups =
    DescribeClusterParameterGroups'
    { _dcpgrqTagValues = Nothing
    , _dcpgrqTagKeys = Nothing
    , _dcpgrqMaxRecords = Nothing
    , _dcpgrqMarker = Nothing
    , _dcpgrqParameterGroupName = Nothing
    }

-- | A tag value or values for which you want to return all matching cluster
-- parameter groups that are associated with the specified tag value or
-- values. For example, suppose that you have parameter groups that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the parameter groups that have either or both of these tag values
-- associated with them.
dcpgrqTagValues :: Lens' DescribeClusterParameterGroups [Text]
dcpgrqTagValues = lens _dcpgrqTagValues (\ s a -> s{_dcpgrqTagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching cluster
-- parameter groups that are associated with the specified key or keys. For
-- example, suppose that you have parameter groups that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the
-- parameter groups that have either or both of these tag keys associated
-- with them.
dcpgrqTagKeys :: Lens' DescribeClusterParameterGroups [Text]
dcpgrqTagKeys = lens _dcpgrqTagKeys (\ s a -> s{_dcpgrqTagKeys = a}) . _Default;

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcpgrqMaxRecords :: Lens' DescribeClusterParameterGroups (Maybe Int)
dcpgrqMaxRecords = lens _dcpgrqMaxRecords (\ s a -> s{_dcpgrqMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeClusterParameterGroups request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
dcpgrqMarker :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgrqMarker = lens _dcpgrqMarker (\ s a -> s{_dcpgrqMarker = a});

-- | The name of a specific parameter group for which to return details. By
-- default, details about all parameter groups and the default parameter
-- group are returned.
dcpgrqParameterGroupName :: Lens' DescribeClusterParameterGroups (Maybe Text)
dcpgrqParameterGroupName = lens _dcpgrqParameterGroupName (\ s a -> s{_dcpgrqParameterGroupName = a});

instance AWSPager DescribeClusterParameterGroups
         where
        page rq rs
          | stop (rs ^. dcpgrsMarker) = Nothing
          | stop (rs ^. dcpgrsParameterGroups) = Nothing
          | otherwise =
            Just $ rq & dcpgrqMarker .~ rs ^. dcpgrsMarker

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
                 toQuery
                   (toQueryList "TagValue" <$> _dcpgrqTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dcpgrqTagKeys),
               "MaxRecords" =: _dcpgrqMaxRecords,
               "Marker" =: _dcpgrqMarker,
               "ParameterGroupName" =: _dcpgrqParameterGroupName]

-- | Contains the output from the DescribeClusterParameterGroups action.
--
-- /See:/ 'describeClusterParameterGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgrsMarker'
--
-- * 'dcpgrsParameterGroups'
--
-- * 'dcpgrsStatus'
data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse'
    { _dcpgrsMarker          :: !(Maybe Text)
    , _dcpgrsParameterGroups :: !(Maybe [ClusterParameterGroup])
    , _dcpgrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterParameterGroupsResponse' smart constructor.
describeClusterParameterGroupsResponse :: Int -> DescribeClusterParameterGroupsResponse
describeClusterParameterGroupsResponse pStatus_ =
    DescribeClusterParameterGroupsResponse'
    { _dcpgrsMarker = Nothing
    , _dcpgrsParameterGroups = Nothing
    , _dcpgrsStatus = pStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcpgrsMarker :: Lens' DescribeClusterParameterGroupsResponse (Maybe Text)
dcpgrsMarker = lens _dcpgrsMarker (\ s a -> s{_dcpgrsMarker = a});

-- | A list of ClusterParameterGroup instances. Each instance describes one
-- cluster parameter group.
dcpgrsParameterGroups :: Lens' DescribeClusterParameterGroupsResponse [ClusterParameterGroup]
dcpgrsParameterGroups = lens _dcpgrsParameterGroups (\ s a -> s{_dcpgrsParameterGroups = a}) . _Default;

-- | FIXME: Undocumented member.
dcpgrsStatus :: Lens' DescribeClusterParameterGroupsResponse Int
dcpgrsStatus = lens _dcpgrsStatus (\ s a -> s{_dcpgrsStatus = a});
