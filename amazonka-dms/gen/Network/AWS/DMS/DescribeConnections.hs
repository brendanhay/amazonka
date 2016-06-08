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
-- Module      : Network.AWS.DMS.DescribeConnections
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the connections that have been made between the replication instance and an endpoint. Connections are created when you test an endpoint.
--
module Network.AWS.DMS.DescribeConnections
    (
    -- * Creating a Request
      describeConnections
    , DescribeConnections
    -- * Request Lenses
    , dcFilters
    , dcMarker
    , dcMaxRecords

    -- * Destructuring the Response
    , describeConnectionsResponse
    , DescribeConnectionsResponse
    -- * Response Lenses
    , dcrsConnections
    , dcrsMarker
    , dcrsResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeConnections' smart constructor.
data DescribeConnections = DescribeConnections'
    { _dcFilters    :: !(Maybe [Filter])
    , _dcMarker     :: !(Maybe Text)
    , _dcMaxRecords :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcFilters'
--
-- * 'dcMarker'
--
-- * 'dcMaxRecords'
describeConnections
    :: DescribeConnections
describeConnections =
    DescribeConnections'
    { _dcFilters = Nothing
    , _dcMarker = Nothing
    , _dcMaxRecords = Nothing
    }

-- | The filters applied to the connection.
--
-- Valid filter names: endpoint-arn | replication-instance-arn
dcFilters :: Lens' DescribeConnections [Filter]
dcFilters = lens _dcFilters (\ s a -> s{_dcFilters = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by 'MaxRecords'.
dcMarker :: Lens' DescribeConnections (Maybe Text)
dcMarker = lens _dcMarker (\ s a -> s{_dcMarker = a});

-- | The maximum number of records to include in the response. If more records exist than the specified 'MaxRecords' value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
dcMaxRecords :: Lens' DescribeConnections (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\ s a -> s{_dcMaxRecords = a});

instance AWSRequest DescribeConnections where
        type Rs DescribeConnections =
             DescribeConnectionsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConnectionsResponse' <$>
                   (x .?> "Connections" .!@ mempty) <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConnections

instance NFData DescribeConnections

instance ToHeaders DescribeConnections where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeConnections" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConnections where
        toJSON DescribeConnections'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dcFilters,
                  ("Marker" .=) <$> _dcMarker,
                  ("MaxRecords" .=) <$> _dcMaxRecords])

instance ToPath DescribeConnections where
        toPath = const "/"

instance ToQuery DescribeConnections where
        toQuery = const mempty

-- | /See:/ 'describeConnectionsResponse' smart constructor.
data DescribeConnectionsResponse = DescribeConnectionsResponse'
    { _dcrsConnections    :: !(Maybe [Connection])
    , _dcrsMarker         :: !(Maybe Text)
    , _dcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsConnections'
--
-- * 'dcrsMarker'
--
-- * 'dcrsResponseStatus'
describeConnectionsResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeConnectionsResponse
describeConnectionsResponse pResponseStatus_ =
    DescribeConnectionsResponse'
    { _dcrsConnections = Nothing
    , _dcrsMarker = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }

-- | A description of the connections.
dcrsConnections :: Lens' DescribeConnectionsResponse [Connection]
dcrsConnections = lens _dcrsConnections (\ s a -> s{_dcrsConnections = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by 'MaxRecords'.
dcrsMarker :: Lens' DescribeConnectionsResponse (Maybe Text)
dcrsMarker = lens _dcrsMarker (\ s a -> s{_dcrsMarker = a});

-- | The response status code.
dcrsResponseStatus :: Lens' DescribeConnectionsResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a});

instance NFData DescribeConnectionsResponse
