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
-- Module      : Network.AWS.DMS.DescribeTableStatistics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table statistics on the database migration task, including table name, rows inserted, rows updated, and rows deleted.
--
--
-- Note that the "last updated" column the DMS console only indicates the time that AWS DMS last updated the table statistics record for a table. It does not indicate the time of the last update to the table.
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeTableStatistics
    (
    -- * Creating a Request
      describeTableStatistics
    , DescribeTableStatistics
    -- * Request Lenses
    , dtsFilters
    , dtsMarker
    , dtsMaxRecords
    , dtsReplicationTaskARN

    -- * Destructuring the Response
    , describeTableStatisticsResponse
    , DescribeTableStatisticsResponse
    -- * Response Lenses
    , dtsrsReplicationTaskARN
    , dtsrsMarker
    , dtsrsTableStatistics
    , dtsrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeTableStatistics' smart constructor.
data DescribeTableStatistics = DescribeTableStatistics'
  { _dtsFilters            :: !(Maybe [Filter])
  , _dtsMarker             :: !(Maybe Text)
  , _dtsMaxRecords         :: !(Maybe Int)
  , _dtsReplicationTaskARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTableStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsFilters' - Filters applied to the describe table statistics action. Valid filter names: schema-name | table-name | table-state A combination of filters creates an AND condition where each record matches all specified filters.
--
-- * 'dtsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dtsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 500.
--
-- * 'dtsReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
describeTableStatistics
    :: Text -- ^ 'dtsReplicationTaskARN'
    -> DescribeTableStatistics
describeTableStatistics pReplicationTaskARN_ =
  DescribeTableStatistics'
    { _dtsFilters = Nothing
    , _dtsMarker = Nothing
    , _dtsMaxRecords = Nothing
    , _dtsReplicationTaskARN = pReplicationTaskARN_
    }


-- | Filters applied to the describe table statistics action. Valid filter names: schema-name | table-name | table-state A combination of filters creates an AND condition where each record matches all specified filters.
dtsFilters :: Lens' DescribeTableStatistics [Filter]
dtsFilters = lens _dtsFilters (\ s a -> s{_dtsFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dtsMarker :: Lens' DescribeTableStatistics (Maybe Text)
dtsMarker = lens _dtsMarker (\ s a -> s{_dtsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 500.
dtsMaxRecords :: Lens' DescribeTableStatistics (Maybe Int)
dtsMaxRecords = lens _dtsMaxRecords (\ s a -> s{_dtsMaxRecords = a})

-- | The Amazon Resource Name (ARN) of the replication task.
dtsReplicationTaskARN :: Lens' DescribeTableStatistics Text
dtsReplicationTaskARN = lens _dtsReplicationTaskARN (\ s a -> s{_dtsReplicationTaskARN = a})

instance AWSPager DescribeTableStatistics where
        page rq rs
          | stop (rs ^. dtsrsMarker) = Nothing
          | stop (rs ^. dtsrsTableStatistics) = Nothing
          | otherwise =
            Just $ rq & dtsMarker .~ rs ^. dtsrsMarker

instance AWSRequest DescribeTableStatistics where
        type Rs DescribeTableStatistics =
             DescribeTableStatisticsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTableStatisticsResponse' <$>
                   (x .?> "ReplicationTaskArn") <*> (x .?> "Marker") <*>
                     (x .?> "TableStatistics" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTableStatistics where

instance NFData DescribeTableStatistics where

instance ToHeaders DescribeTableStatistics where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeTableStatistics" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTableStatistics where
        toJSON DescribeTableStatistics'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dtsFilters,
                  ("Marker" .=) <$> _dtsMarker,
                  ("MaxRecords" .=) <$> _dtsMaxRecords,
                  Just
                    ("ReplicationTaskArn" .= _dtsReplicationTaskARN)])

instance ToPath DescribeTableStatistics where
        toPath = const "/"

instance ToQuery DescribeTableStatistics where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeTableStatisticsResponse' smart constructor.
data DescribeTableStatisticsResponse = DescribeTableStatisticsResponse'
  { _dtsrsReplicationTaskARN :: !(Maybe Text)
  , _dtsrsMarker             :: !(Maybe Text)
  , _dtsrsTableStatistics    :: !(Maybe [TableStatistics])
  , _dtsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTableStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsrsReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'dtsrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dtsrsTableStatistics' - The table statistics.
--
-- * 'dtsrsResponseStatus' - -- | The response status code.
describeTableStatisticsResponse
    :: Int -- ^ 'dtsrsResponseStatus'
    -> DescribeTableStatisticsResponse
describeTableStatisticsResponse pResponseStatus_ =
  DescribeTableStatisticsResponse'
    { _dtsrsReplicationTaskARN = Nothing
    , _dtsrsMarker = Nothing
    , _dtsrsTableStatistics = Nothing
    , _dtsrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the replication task.
dtsrsReplicationTaskARN :: Lens' DescribeTableStatisticsResponse (Maybe Text)
dtsrsReplicationTaskARN = lens _dtsrsReplicationTaskARN (\ s a -> s{_dtsrsReplicationTaskARN = a})

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dtsrsMarker :: Lens' DescribeTableStatisticsResponse (Maybe Text)
dtsrsMarker = lens _dtsrsMarker (\ s a -> s{_dtsrsMarker = a})

-- | The table statistics.
dtsrsTableStatistics :: Lens' DescribeTableStatisticsResponse [TableStatistics]
dtsrsTableStatistics = lens _dtsrsTableStatistics (\ s a -> s{_dtsrsTableStatistics = a}) . _Default . _Coerce

-- | -- | The response status code.
dtsrsResponseStatus :: Lens' DescribeTableStatisticsResponse Int
dtsrsResponseStatus = lens _dtsrsResponseStatus (\ s a -> s{_dtsrsResponseStatus = a})

instance NFData DescribeTableStatisticsResponse where
