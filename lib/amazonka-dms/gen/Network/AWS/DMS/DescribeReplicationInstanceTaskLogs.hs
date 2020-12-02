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
-- Module      : Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the task logs for the specified task.
--
--
module Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
    (
    -- * Creating a Request
      describeReplicationInstanceTaskLogs
    , DescribeReplicationInstanceTaskLogs
    -- * Request Lenses
    , dritlMarker
    , dritlMaxRecords
    , dritlReplicationInstanceARN

    -- * Destructuring the Response
    , describeReplicationInstanceTaskLogsResponse
    , DescribeReplicationInstanceTaskLogsResponse
    -- * Response Lenses
    , dritlrsReplicationInstanceTaskLogs
    , dritlrsMarker
    , dritlrsReplicationInstanceARN
    , dritlrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeReplicationInstanceTaskLogs' smart constructor.
data DescribeReplicationInstanceTaskLogs = DescribeReplicationInstanceTaskLogs'
  { _dritlMarker                 :: !(Maybe Text)
  , _dritlMaxRecords             :: !(Maybe Int)
  , _dritlReplicationInstanceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReplicationInstanceTaskLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dritlMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dritlMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dritlReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
describeReplicationInstanceTaskLogs
    :: Text -- ^ 'dritlReplicationInstanceARN'
    -> DescribeReplicationInstanceTaskLogs
describeReplicationInstanceTaskLogs pReplicationInstanceARN_ =
  DescribeReplicationInstanceTaskLogs'
    { _dritlMarker = Nothing
    , _dritlMaxRecords = Nothing
    , _dritlReplicationInstanceARN = pReplicationInstanceARN_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dritlMarker :: Lens' DescribeReplicationInstanceTaskLogs (Maybe Text)
dritlMarker = lens _dritlMarker (\ s a -> s{_dritlMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dritlMaxRecords :: Lens' DescribeReplicationInstanceTaskLogs (Maybe Int)
dritlMaxRecords = lens _dritlMaxRecords (\ s a -> s{_dritlMaxRecords = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
dritlReplicationInstanceARN :: Lens' DescribeReplicationInstanceTaskLogs Text
dritlReplicationInstanceARN = lens _dritlReplicationInstanceARN (\ s a -> s{_dritlReplicationInstanceARN = a})

instance AWSRequest
           DescribeReplicationInstanceTaskLogs
         where
        type Rs DescribeReplicationInstanceTaskLogs =
             DescribeReplicationInstanceTaskLogsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeReplicationInstanceTaskLogsResponse' <$>
                   (x .?> "ReplicationInstanceTaskLogs" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (x .?> "ReplicationInstanceArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReplicationInstanceTaskLogs
         where

instance NFData DescribeReplicationInstanceTaskLogs
         where

instance ToHeaders
           DescribeReplicationInstanceTaskLogs
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeReplicationInstanceTaskLogs"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeReplicationInstanceTaskLogs
         where
        toJSON DescribeReplicationInstanceTaskLogs'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _dritlMarker,
                  ("MaxRecords" .=) <$> _dritlMaxRecords,
                  Just
                    ("ReplicationInstanceArn" .=
                       _dritlReplicationInstanceARN)])

instance ToPath DescribeReplicationInstanceTaskLogs
         where
        toPath = const "/"

instance ToQuery DescribeReplicationInstanceTaskLogs
         where
        toQuery = const mempty

-- | /See:/ 'describeReplicationInstanceTaskLogsResponse' smart constructor.
data DescribeReplicationInstanceTaskLogsResponse = DescribeReplicationInstanceTaskLogsResponse'
  { _dritlrsReplicationInstanceTaskLogs :: !(Maybe [ReplicationInstanceTaskLog])
  , _dritlrsMarker                      :: !(Maybe Text)
  , _dritlrsReplicationInstanceARN      :: !(Maybe Text)
  , _dritlrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReplicationInstanceTaskLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dritlrsReplicationInstanceTaskLogs' - An array of replication task log metadata. Each member of the array contains the replication task name, ARN, and task log size (in bytes).
--
-- * 'dritlrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dritlrsReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'dritlrsResponseStatus' - -- | The response status code.
describeReplicationInstanceTaskLogsResponse
    :: Int -- ^ 'dritlrsResponseStatus'
    -> DescribeReplicationInstanceTaskLogsResponse
describeReplicationInstanceTaskLogsResponse pResponseStatus_ =
  DescribeReplicationInstanceTaskLogsResponse'
    { _dritlrsReplicationInstanceTaskLogs = Nothing
    , _dritlrsMarker = Nothing
    , _dritlrsReplicationInstanceARN = Nothing
    , _dritlrsResponseStatus = pResponseStatus_
    }


-- | An array of replication task log metadata. Each member of the array contains the replication task name, ARN, and task log size (in bytes).
dritlrsReplicationInstanceTaskLogs :: Lens' DescribeReplicationInstanceTaskLogsResponse [ReplicationInstanceTaskLog]
dritlrsReplicationInstanceTaskLogs = lens _dritlrsReplicationInstanceTaskLogs (\ s a -> s{_dritlrsReplicationInstanceTaskLogs = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dritlrsMarker :: Lens' DescribeReplicationInstanceTaskLogsResponse (Maybe Text)
dritlrsMarker = lens _dritlrsMarker (\ s a -> s{_dritlrsMarker = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
dritlrsReplicationInstanceARN :: Lens' DescribeReplicationInstanceTaskLogsResponse (Maybe Text)
dritlrsReplicationInstanceARN = lens _dritlrsReplicationInstanceARN (\ s a -> s{_dritlrsReplicationInstanceARN = a})

-- | -- | The response status code.
dritlrsResponseStatus :: Lens' DescribeReplicationInstanceTaskLogsResponse Int
dritlrsResponseStatus = lens _dritlrsResponseStatus (\ s a -> s{_dritlrsResponseStatus = a})

instance NFData
           DescribeReplicationInstanceTaskLogsResponse
         where
