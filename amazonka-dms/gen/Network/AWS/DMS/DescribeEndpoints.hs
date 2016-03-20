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
-- Module      : Network.AWS.DMS.DescribeEndpoints
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the endpoints for your account in the current
-- region.
--
module Network.AWS.DMS.DescribeEndpoints
    (
    -- * Creating a Request
      describeEndpoints
    , DescribeEndpoints
    -- * Request Lenses
    , deFilters
    , deMarker
    , deMaxRecords

    -- * Destructuring the Response
    , describeEndpointsResponse
    , DescribeEndpointsResponse
    -- * Response Lenses
    , dersMarker
    , dersEndpoints
    , dersResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
    { _deFilters    :: !(Maybe [Filter])
    , _deMarker     :: !(Maybe Text)
    , _deMaxRecords :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deFilters'
--
-- * 'deMarker'
--
-- * 'deMaxRecords'
describeEndpoints
    :: DescribeEndpoints
describeEndpoints =
    DescribeEndpoints'
    { _deFilters = Nothing
    , _deMarker = Nothing
    , _deMaxRecords = Nothing
    }

-- | Filters applied to the describe action.
--
-- Valid filter names: endpoint-arn | endpoint-type | endpoint-id |
-- engine-name
deFilters :: Lens' DescribeEndpoints [Filter]
deFilters = lens _deFilters (\ s a -> s{_deFilters = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
deMarker :: Lens' DescribeEndpoints (Maybe Text)
deMarker = lens _deMarker (\ s a -> s{_deMarker = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified 'MaxRecords' value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
deMaxRecords :: Lens' DescribeEndpoints (Maybe Int)
deMaxRecords = lens _deMaxRecords (\ s a -> s{_deMaxRecords = a});

instance AWSRequest DescribeEndpoints where
        type Rs DescribeEndpoints = DescribeEndpointsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Endpoints" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEndpoints

instance ToHeaders DescribeEndpoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeEndpoints" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEndpoints where
        toJSON DescribeEndpoints'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _deFilters,
                  ("Marker" .=) <$> _deMarker,
                  ("MaxRecords" .=) <$> _deMaxRecords])

instance ToPath DescribeEndpoints where
        toPath = const "/"

instance ToQuery DescribeEndpoints where
        toQuery = const mempty

-- | /See:/ 'describeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
    { _dersMarker         :: !(Maybe Text)
    , _dersEndpoints      :: !(Maybe [Endpoint])
    , _dersResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersMarker'
--
-- * 'dersEndpoints'
--
-- * 'dersResponseStatus'
describeEndpointsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEndpointsResponse
describeEndpointsResponse pResponseStatus_ =
    DescribeEndpointsResponse'
    { _dersMarker = Nothing
    , _dersEndpoints = Nothing
    , _dersResponseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
dersMarker :: Lens' DescribeEndpointsResponse (Maybe Text)
dersMarker = lens _dersMarker (\ s a -> s{_dersMarker = a});

-- | Endpoint description.
dersEndpoints :: Lens' DescribeEndpointsResponse [Endpoint]
dersEndpoints = lens _dersEndpoints (\ s a -> s{_dersEndpoints = a}) . _Default . _Coerce;

-- | The response status code.
dersResponseStatus :: Lens' DescribeEndpointsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a});
