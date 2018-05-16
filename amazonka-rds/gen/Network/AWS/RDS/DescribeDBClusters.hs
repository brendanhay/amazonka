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
-- Module      : Network.AWS.RDS.DescribeDBClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Aurora DB clusters. This API supports pagination.
--
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.DescribeDBClusters
    (
    -- * Creating a Request
      describeDBClusters
    , DescribeDBClusters
    -- * Request Lenses
    , ddcDBClusterIdentifier
    , ddcFilters
    , ddcMarker
    , ddcMaxRecords

    -- * Destructuring the Response
    , describeDBClustersResponse
    , DescribeDBClustersResponse
    -- * Response Lenses
    , ddcrsDBClusters
    , ddcrsMarker
    , ddcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { _ddcDBClusterIdentifier :: !(Maybe Text)
  , _ddcFilters             :: !(Maybe [Filter])
  , _ddcMarker              :: !(Maybe Text)
  , _ddcMaxRecords          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDBClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive. Constraints:     * If supplied, must match an existing DBClusterIdentifier.
--
-- * 'ddcFilters' - A filter that specifies one or more DB clusters to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
-- * 'ddcMarker' - An optional pagination token provided by a previous 'DescribeDBClusters' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeDBClusters
    :: DescribeDBClusters
describeDBClusters =
  DescribeDBClusters'
    { _ddcDBClusterIdentifier = Nothing
    , _ddcFilters = Nothing
    , _ddcMarker = Nothing
    , _ddcMaxRecords = Nothing
    }


-- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive. Constraints:     * If supplied, must match an existing DBClusterIdentifier.
ddcDBClusterIdentifier :: Lens' DescribeDBClusters (Maybe Text)
ddcDBClusterIdentifier = lens _ddcDBClusterIdentifier (\ s a -> s{_ddcDBClusterIdentifier = a})

-- | A filter that specifies one or more DB clusters to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
ddcFilters :: Lens' DescribeDBClusters [Filter]
ddcFilters = lens _ddcFilters (\ s a -> s{_ddcFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous 'DescribeDBClusters' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcMarker :: Lens' DescribeDBClusters (Maybe Text)
ddcMarker = lens _ddcMarker (\ s a -> s{_ddcMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddcMaxRecords :: Lens' DescribeDBClusters (Maybe Int)
ddcMaxRecords = lens _ddcMaxRecords (\ s a -> s{_ddcMaxRecords = a})

instance AWSRequest DescribeDBClusters where
        type Rs DescribeDBClusters =
             DescribeDBClustersResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBClustersResult"
              (\ s h x ->
                 DescribeDBClustersResponse' <$>
                   (x .@? "DBClusters" .!@ mempty >>=
                      may (parseXMLList "DBCluster"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBClusters where

instance NFData DescribeDBClusters where

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
               "Marker" =: _ddcMarker,
               "MaxRecords" =: _ddcMaxRecords]

-- | Contains the result of a successful invocation of the 'DescribeDBClusters' action.
--
--
--
-- /See:/ 'describeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { _ddcrsDBClusters     :: !(Maybe [DBCluster])
  , _ddcrsMarker         :: !(Maybe Text)
  , _ddcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcrsDBClusters' - Contains a list of DB clusters for the user.
--
-- * 'ddcrsMarker' - A pagination token that can be used in a subsequent DescribeDBClusters request.
--
-- * 'ddcrsResponseStatus' - -- | The response status code.
describeDBClustersResponse
    :: Int -- ^ 'ddcrsResponseStatus'
    -> DescribeDBClustersResponse
describeDBClustersResponse pResponseStatus_ =
  DescribeDBClustersResponse'
    { _ddcrsDBClusters = Nothing
    , _ddcrsMarker = Nothing
    , _ddcrsResponseStatus = pResponseStatus_
    }


-- | Contains a list of DB clusters for the user.
ddcrsDBClusters :: Lens' DescribeDBClustersResponse [DBCluster]
ddcrsDBClusters = lens _ddcrsDBClusters (\ s a -> s{_ddcrsDBClusters = a}) . _Default . _Coerce

-- | A pagination token that can be used in a subsequent DescribeDBClusters request.
ddcrsMarker :: Lens' DescribeDBClustersResponse (Maybe Text)
ddcrsMarker = lens _ddcrsMarker (\ s a -> s{_ddcrsMarker = a})

-- | -- | The response status code.
ddcrsResponseStatus :: Lens' DescribeDBClustersResponse Int
ddcrsResponseStatus = lens _ddcrsResponseStatus (\ s a -> s{_ddcrsResponseStatus = a})

instance NFData DescribeDBClustersResponse where
