{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Aurora DB clusters. This API
-- supports pagination.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBClusters.html AWS API Reference> for DescribeDBClusters.
module Network.AWS.RDS.DescribeDBClusters
    (
    -- * Creating a Request
      DescribeDBClusters
    , describeDBClusters
    -- * Request Lenses
    , ddcDBClusterIdentifier
    , ddcFilters
    , ddcMaxRecords
    , ddcMarker

    -- * Destructuring the Response
    , DescribeDBClustersResponse
    , describeDBClustersResponse
    -- * Response Lenses
    , ddcrsDBClusters
    , ddcrsMarker
    , ddcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBClusters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDBClusterIdentifier'
--
-- * 'ddcFilters'
--
-- * 'ddcMaxRecords'
--
-- * 'ddcMarker'
data DescribeDBClusters = DescribeDBClusters'
    { _ddcDBClusterIdentifier :: !(Maybe Text)
    , _ddcFilters             :: !(Maybe [Filter])
    , _ddcMaxRecords          :: !(Maybe Int)
    , _ddcMarker              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBClusters' smart constructor.
describeDBClusters :: DescribeDBClusters
describeDBClusters =
    DescribeDBClusters'
    { _ddcDBClusterIdentifier = Nothing
    , _ddcFilters = Nothing
    , _ddcMaxRecords = Nothing
    , _ddcMarker = Nothing
    }

-- | The user-supplied DB cluster identifier. If this parameter is specified,
-- information from only the specific DB cluster is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddcDBClusterIdentifier :: Lens' DescribeDBClusters (Maybe Text)
ddcDBClusterIdentifier = lens _ddcDBClusterIdentifier (\ s a -> s{_ddcDBClusterIdentifier = a});

-- | This parameter is not currently supported.
ddcFilters :: Lens' DescribeDBClusters [Filter]
ddcFilters = lens _ddcFilters (\ s a -> s{_ddcFilters = a}) . _Default . _Coerce;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddcMaxRecords :: Lens' DescribeDBClusters (Maybe Int)
ddcMaxRecords = lens _ddcMaxRecords (\ s a -> s{_ddcMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeDBClusters
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
ddcMarker :: Lens' DescribeDBClusters (Maybe Text)
ddcMarker = lens _ddcMarker (\ s a -> s{_ddcMarker = a});

instance AWSRequest DescribeDBClusters where
        type Sv DescribeDBClusters = RDS
        type Rs DescribeDBClusters =
             DescribeDBClustersResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeDBClustersResult"
              (\ s h x ->
                 DescribeDBClustersResponse' <$>
                   (x .@? "DBClusters" .!@ mempty >>=
                      may (parseXMLList "DBCluster"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDBClusters where
        toHeaders = const mempty

instance ToPath DescribeDBClusters where
        toPath = const "/"

instance ToQuery DescribeDBClusters where
        toQuery DescribeDBClusters'{..}
          = mconcat
              ["Action" =: ("DescribeDBClusters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _ddcDBClusterIdentifier,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddcFilters),
               "MaxRecords" =: _ddcMaxRecords,
               "Marker" =: _ddcMarker]

-- | Contains the result of a successful invocation of the DescribeDBClusters
-- action.
--
-- /See:/ 'describeDBClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcrsDBClusters'
--
-- * 'ddcrsMarker'
--
-- * 'ddcrsStatus'
data DescribeDBClustersResponse = DescribeDBClustersResponse'
    { _ddcrsDBClusters :: !(Maybe [DBCluster])
    , _ddcrsMarker     :: !(Maybe Text)
    , _ddcrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBClustersResponse' smart constructor.
describeDBClustersResponse :: Int -> DescribeDBClustersResponse
describeDBClustersResponse pStatus_ =
    DescribeDBClustersResponse'
    { _ddcrsDBClusters = Nothing
    , _ddcrsMarker = Nothing
    , _ddcrsStatus = pStatus_
    }

-- | Contains a list of DB clusters for the user.
ddcrsDBClusters :: Lens' DescribeDBClustersResponse [DBCluster]
ddcrsDBClusters = lens _ddcrsDBClusters (\ s a -> s{_ddcrsDBClusters = a}) . _Default . _Coerce;

-- | A pagination token that can be used in a subsequent DescribeDBClusters
-- request.
ddcrsMarker :: Lens' DescribeDBClustersResponse (Maybe Text)
ddcrsMarker = lens _ddcrsMarker (\ s a -> s{_ddcrsMarker = a});

-- | Undocumented member.
ddcrsStatus :: Lens' DescribeDBClustersResponse Int
ddcrsStatus = lens _ddcrsStatus (\ s a -> s{_ddcrsStatus = a});
