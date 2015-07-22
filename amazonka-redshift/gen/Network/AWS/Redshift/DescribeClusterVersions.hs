{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of the available Amazon Redshift cluster versions.
-- You can call this operation even before creating any clusters to learn
-- more about the Amazon Redshift versions. For more information about
-- managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterVersions.html>
module Network.AWS.Redshift.DescribeClusterVersions
    (
    -- * Request
      DescribeClusterVersions
    -- ** Request constructor
    , describeClusterVersions
    -- ** Request lenses
    , dcvrqMaxRecords
    , dcvrqMarker
    , dcvrqClusterParameterGroupFamily
    , dcvrqClusterVersion

    -- * Response
    , DescribeClusterVersionsResponse
    -- ** Response constructor
    , describeClusterVersionsResponse
    -- ** Response lenses
    , dcvrsClusterVersions
    , dcvrsMarker
    , dcvrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeClusterVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcvrqMaxRecords'
--
-- * 'dcvrqMarker'
--
-- * 'dcvrqClusterParameterGroupFamily'
--
-- * 'dcvrqClusterVersion'
data DescribeClusterVersions = DescribeClusterVersions'
    { _dcvrqMaxRecords                  :: !(Maybe Int)
    , _dcvrqMarker                      :: !(Maybe Text)
    , _dcvrqClusterParameterGroupFamily :: !(Maybe Text)
    , _dcvrqClusterVersion              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterVersions' smart constructor.
describeClusterVersions :: DescribeClusterVersions
describeClusterVersions =
    DescribeClusterVersions'
    { _dcvrqMaxRecords = Nothing
    , _dcvrqMarker = Nothing
    , _dcvrqClusterParameterGroupFamily = Nothing
    , _dcvrqClusterVersion = Nothing
    }

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcvrqMaxRecords :: Lens' DescribeClusterVersions (Maybe Int)
dcvrqMaxRecords = lens _dcvrqMaxRecords (\ s a -> s{_dcvrqMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterVersions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcvrqMarker :: Lens' DescribeClusterVersions (Maybe Text)
dcvrqMarker = lens _dcvrqMarker (\ s a -> s{_dcvrqMarker = a});

-- | The name of a specific cluster parameter group family to return details
-- for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
dcvrqClusterParameterGroupFamily :: Lens' DescribeClusterVersions (Maybe Text)
dcvrqClusterParameterGroupFamily = lens _dcvrqClusterParameterGroupFamily (\ s a -> s{_dcvrqClusterParameterGroupFamily = a});

-- | The specific cluster version to return.
--
-- Example: @1.0@
dcvrqClusterVersion :: Lens' DescribeClusterVersions (Maybe Text)
dcvrqClusterVersion = lens _dcvrqClusterVersion (\ s a -> s{_dcvrqClusterVersion = a});

instance AWSPager DescribeClusterVersions where
        page rq rs
          | stop (rs ^. dcvrsMarker) = Nothing
          | stop (rs ^. dcvrsClusterVersions) = Nothing
          | otherwise =
            Just $ rq & dcvrqMarker .~ rs ^. dcvrsMarker

instance AWSRequest DescribeClusterVersions where
        type Sv DescribeClusterVersions = Redshift
        type Rs DescribeClusterVersions =
             DescribeClusterVersionsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeClusterVersionsResult"
              (\ s h x ->
                 DescribeClusterVersionsResponse' <$>
                   (x .@? "ClusterVersions" .!@ mempty >>=
                      may (parseXMLList "ClusterVersion"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeClusterVersions where
        toHeaders = const mempty

instance ToPath DescribeClusterVersions where
        toPath = const "/"

instance ToQuery DescribeClusterVersions where
        toQuery DescribeClusterVersions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterVersions" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "MaxRecords" =: _dcvrqMaxRecords,
               "Marker" =: _dcvrqMarker,
               "ClusterParameterGroupFamily" =:
                 _dcvrqClusterParameterGroupFamily,
               "ClusterVersion" =: _dcvrqClusterVersion]

-- | Contains the output from the DescribeClusterVersions action.
--
-- /See:/ 'describeClusterVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcvrsClusterVersions'
--
-- * 'dcvrsMarker'
--
-- * 'dcvrsStatus'
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse'
    { _dcvrsClusterVersions :: !(Maybe [ClusterVersion])
    , _dcvrsMarker          :: !(Maybe Text)
    , _dcvrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterVersionsResponse' smart constructor.
describeClusterVersionsResponse :: Int -> DescribeClusterVersionsResponse
describeClusterVersionsResponse pStatus =
    DescribeClusterVersionsResponse'
    { _dcvrsClusterVersions = Nothing
    , _dcvrsMarker = Nothing
    , _dcvrsStatus = pStatus
    }

-- | A list of @Version@ elements.
dcvrsClusterVersions :: Lens' DescribeClusterVersionsResponse [ClusterVersion]
dcvrsClusterVersions = lens _dcvrsClusterVersions (\ s a -> s{_dcvrsClusterVersions = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcvrsMarker :: Lens' DescribeClusterVersionsResponse (Maybe Text)
dcvrsMarker = lens _dcvrsMarker (\ s a -> s{_dcvrsMarker = a});

-- | FIXME: Undocumented member.
dcvrsStatus :: Lens' DescribeClusterVersionsResponse Int
dcvrsStatus = lens _dcvrsStatus (\ s a -> s{_dcvrsStatus = a});
