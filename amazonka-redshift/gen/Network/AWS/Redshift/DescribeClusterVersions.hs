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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of the available Amazon Redshift cluster versions.
-- You can call this operation even before creating any clusters to learn
-- more about the Amazon Redshift versions. For more information about
-- managing clusters, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterVersions.html AWS API Reference> for DescribeClusterVersions.
module Network.AWS.Redshift.DescribeClusterVersions
    (
    -- * Creating a Request
      DescribeClusterVersions
    , describeClusterVersions
    -- * Request Lenses
    , dcvMaxRecords
    , dcvMarker
    , dcvClusterParameterGroupFamily
    , dcvClusterVersion

    -- * Destructuring the Response
    , DescribeClusterVersionsResponse
    , describeClusterVersionsResponse
    -- * Response Lenses
    , dcvrsClusterVersions
    , dcvrsMarker
    , dcvrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeClusterVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcvMaxRecords'
--
-- * 'dcvMarker'
--
-- * 'dcvClusterParameterGroupFamily'
--
-- * 'dcvClusterVersion'
data DescribeClusterVersions = DescribeClusterVersions'
    { _dcvMaxRecords                  :: !(Maybe Int)
    , _dcvMarker                      :: !(Maybe Text)
    , _dcvClusterParameterGroupFamily :: !(Maybe Text)
    , _dcvClusterVersion              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterVersions' smart constructor.
describeClusterVersions :: DescribeClusterVersions
describeClusterVersions =
    DescribeClusterVersions'
    { _dcvMaxRecords = Nothing
    , _dcvMarker = Nothing
    , _dcvClusterParameterGroupFamily = Nothing
    , _dcvClusterVersion = Nothing
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
dcvMaxRecords :: Lens' DescribeClusterVersions (Maybe Int)
dcvMaxRecords = lens _dcvMaxRecords (\ s a -> s{_dcvMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterVersions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcvMarker :: Lens' DescribeClusterVersions (Maybe Text)
dcvMarker = lens _dcvMarker (\ s a -> s{_dcvMarker = a});

-- | The name of a specific cluster parameter group family to return details
-- for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
dcvClusterParameterGroupFamily :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterParameterGroupFamily = lens _dcvClusterParameterGroupFamily (\ s a -> s{_dcvClusterParameterGroupFamily = a});

-- | The specific cluster version to return.
--
-- Example: @1.0@
dcvClusterVersion :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterVersion = lens _dcvClusterVersion (\ s a -> s{_dcvClusterVersion = a});

instance AWSPager DescribeClusterVersions where
        page rq rs
          | stop (rs ^. dcvrsMarker) = Nothing
          | stop (rs ^. dcvrsClusterVersions) = Nothing
          | otherwise =
            Just $ rq & dcvMarker .~ rs ^. dcvrsMarker

instance AWSRequest DescribeClusterVersions where
        type Sv DescribeClusterVersions = Redshift
        type Rs DescribeClusterVersions =
             DescribeClusterVersionsResponse
        request = postQuery
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
               "MaxRecords" =: _dcvMaxRecords,
               "Marker" =: _dcvMarker,
               "ClusterParameterGroupFamily" =:
                 _dcvClusterParameterGroupFamily,
               "ClusterVersion" =: _dcvClusterVersion]

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
describeClusterVersionsResponse pStatus_ =
    DescribeClusterVersionsResponse'
    { _dcvrsClusterVersions = Nothing
    , _dcvrsMarker = Nothing
    , _dcvrsStatus = pStatus_
    }

-- | A list of @Version@ elements.
dcvrsClusterVersions :: Lens' DescribeClusterVersionsResponse [ClusterVersion]
dcvrsClusterVersions = lens _dcvrsClusterVersions (\ s a -> s{_dcvrsClusterVersions = a}) . _Default . _Coerce;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcvrsMarker :: Lens' DescribeClusterVersionsResponse (Maybe Text)
dcvrsMarker = lens _dcvrsMarker (\ s a -> s{_dcvrsMarker = a});

-- | Undocumented member.
dcvrsStatus :: Lens' DescribeClusterVersionsResponse Int
dcvrsStatus = lens _dcvrsStatus (\ s a -> s{_dcvrsStatus = a});
