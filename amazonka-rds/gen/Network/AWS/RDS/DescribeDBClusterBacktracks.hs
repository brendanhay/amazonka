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
-- Module      : Network.AWS.RDS.DescribeDBClusterBacktracks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about backtracks for a DB cluster.
--
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.DescribeDBClusterBacktracks
    (
    -- * Creating a Request
      describeDBClusterBacktracks
    , DescribeDBClusterBacktracks
    -- * Request Lenses
    , ddcbBacktrackIdentifier
    , ddcbFilters
    , ddcbMarker
    , ddcbMaxRecords
    , ddcbDBClusterIdentifier

    -- * Destructuring the Response
    , describeDBClusterBacktracksResponse
    , DescribeDBClusterBacktracksResponse
    -- * Response Lenses
    , ddcbrsMarker
    , ddcbrsDBClusterBacktracks
    , ddcbrsResponseStatus
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
-- /See:/ 'describeDBClusterBacktracks' smart constructor.
data DescribeDBClusterBacktracks = DescribeDBClusterBacktracks'
  { _ddcbBacktrackIdentifier :: !(Maybe Text)
  , _ddcbFilters             :: !(Maybe [Filter])
  , _ddcbMarker              :: !(Maybe Text)
  , _ddcbMaxRecords          :: !(Maybe Int)
  , _ddcbDBClusterIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusterBacktracks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcbBacktrackIdentifier' - If specified, this value is the backtrack identifier of the backtrack to be described. Constraints:     * Must contain a valid universally unique identifier (UUID). For more information about UUIDs, see <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace> . Example: @123e4567-e89b-12d3-a456-426655440000@
--
-- * 'ddcbFilters' - A filter that specifies one or more DB clusters to describe. Supported filters include the following:     * @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The results list includes information about only the backtracks identified by these identifiers.     * @db-cluster-backtrack-status@ - Accepts any of the following backtrack status values:     * @applying@      * @completed@      * @failed@      * @pending@  The results list includes information about only the backtracks identified by these values. For more information about backtrack status values, see 'DBClusterBacktrack' .
--
-- * 'ddcbMarker' - An optional pagination token provided by a previous 'DescribeDBClusterBacktracks' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcbMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddcbDBClusterIdentifier' - The DB cluster identifier of the DB cluster to be described. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@
describeDBClusterBacktracks
    :: Text -- ^ 'ddcbDBClusterIdentifier'
    -> DescribeDBClusterBacktracks
describeDBClusterBacktracks pDBClusterIdentifier_ =
  DescribeDBClusterBacktracks'
    { _ddcbBacktrackIdentifier = Nothing
    , _ddcbFilters = Nothing
    , _ddcbMarker = Nothing
    , _ddcbMaxRecords = Nothing
    , _ddcbDBClusterIdentifier = pDBClusterIdentifier_
    }


-- | If specified, this value is the backtrack identifier of the backtrack to be described. Constraints:     * Must contain a valid universally unique identifier (UUID). For more information about UUIDs, see <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace> . Example: @123e4567-e89b-12d3-a456-426655440000@
ddcbBacktrackIdentifier :: Lens' DescribeDBClusterBacktracks (Maybe Text)
ddcbBacktrackIdentifier = lens _ddcbBacktrackIdentifier (\ s a -> s{_ddcbBacktrackIdentifier = a})

-- | A filter that specifies one or more DB clusters to describe. Supported filters include the following:     * @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The results list includes information about only the backtracks identified by these identifiers.     * @db-cluster-backtrack-status@ - Accepts any of the following backtrack status values:     * @applying@      * @completed@      * @failed@      * @pending@  The results list includes information about only the backtracks identified by these values. For more information about backtrack status values, see 'DBClusterBacktrack' .
ddcbFilters :: Lens' DescribeDBClusterBacktracks [Filter]
ddcbFilters = lens _ddcbFilters (\ s a -> s{_ddcbFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous 'DescribeDBClusterBacktracks' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcbMarker :: Lens' DescribeDBClusterBacktracks (Maybe Text)
ddcbMarker = lens _ddcbMarker (\ s a -> s{_ddcbMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddcbMaxRecords :: Lens' DescribeDBClusterBacktracks (Maybe Int)
ddcbMaxRecords = lens _ddcbMaxRecords (\ s a -> s{_ddcbMaxRecords = a})

-- | The DB cluster identifier of the DB cluster to be described. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@
ddcbDBClusterIdentifier :: Lens' DescribeDBClusterBacktracks Text
ddcbDBClusterIdentifier = lens _ddcbDBClusterIdentifier (\ s a -> s{_ddcbDBClusterIdentifier = a})

instance AWSRequest DescribeDBClusterBacktracks where
        type Rs DescribeDBClusterBacktracks =
             DescribeDBClusterBacktracksResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeDBClusterBacktracksResult"
              (\ s h x ->
                 DescribeDBClusterBacktracksResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBClusterBacktracks" .!@ mempty >>=
                        may (parseXMLList "DBClusterBacktrack"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBClusterBacktracks where

instance NFData DescribeDBClusterBacktracks where

instance ToHeaders DescribeDBClusterBacktracks where
        toHeaders = const mempty

instance ToPath DescribeDBClusterBacktracks where
        toPath = const "/"

instance ToQuery DescribeDBClusterBacktracks where
        toQuery DescribeDBClusterBacktracks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBClusterBacktracks" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "BacktrackIdentifier" =: _ddcbBacktrackIdentifier,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddcbFilters),
               "Marker" =: _ddcbMarker,
               "MaxRecords" =: _ddcbMaxRecords,
               "DBClusterIdentifier" =: _ddcbDBClusterIdentifier]

-- | Contains the result of a successful invocation of the 'DescribeDBClusterBacktracks' action.
--
--
--
-- /See:/ 'describeDBClusterBacktracksResponse' smart constructor.
data DescribeDBClusterBacktracksResponse = DescribeDBClusterBacktracksResponse'
  { _ddcbrsMarker              :: !(Maybe Text)
  , _ddcbrsDBClusterBacktracks :: !(Maybe [DBClusterBacktrack])
  , _ddcbrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusterBacktracksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcbrsMarker' - A pagination token that can be used in a subsequent 'DescribeDBClusterBacktracks' request.
--
-- * 'ddcbrsDBClusterBacktracks' - Contains a list of backtracks for the user.
--
-- * 'ddcbrsResponseStatus' - -- | The response status code.
describeDBClusterBacktracksResponse
    :: Int -- ^ 'ddcbrsResponseStatus'
    -> DescribeDBClusterBacktracksResponse
describeDBClusterBacktracksResponse pResponseStatus_ =
  DescribeDBClusterBacktracksResponse'
    { _ddcbrsMarker = Nothing
    , _ddcbrsDBClusterBacktracks = Nothing
    , _ddcbrsResponseStatus = pResponseStatus_
    }


-- | A pagination token that can be used in a subsequent 'DescribeDBClusterBacktracks' request.
ddcbrsMarker :: Lens' DescribeDBClusterBacktracksResponse (Maybe Text)
ddcbrsMarker = lens _ddcbrsMarker (\ s a -> s{_ddcbrsMarker = a})

-- | Contains a list of backtracks for the user.
ddcbrsDBClusterBacktracks :: Lens' DescribeDBClusterBacktracksResponse [DBClusterBacktrack]
ddcbrsDBClusterBacktracks = lens _ddcbrsDBClusterBacktracks (\ s a -> s{_ddcbrsDBClusterBacktracks = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcbrsResponseStatus :: Lens' DescribeDBClusterBacktracksResponse Int
ddcbrsResponseStatus = lens _ddcbrsResponseStatus (\ s a -> s{_ddcbrsResponseStatus = a})

instance NFData DescribeDBClusterBacktracksResponse
         where
