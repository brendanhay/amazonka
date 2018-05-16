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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the endpoints for your account in the current region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpoints
    (
    -- * Creating a Request
      describeEndpoints
    , DescribeEndpoints
    -- * Request Lenses
    , desFilters
    , desMarker
    , desMaxRecords

    -- * Destructuring the Response
    , describeEndpointsResponse
    , DescribeEndpointsResponse
    -- * Response Lenses
    , dersMarker
    , dersEndpoints
    , dersResponseStatus
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
-- /See:/ 'describeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { _desFilters    :: !(Maybe [Filter])
  , _desMarker     :: !(Maybe Text)
  , _desMaxRecords :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desFilters' - Filters applied to the describe action. Valid filter names: endpoint-arn | endpoint-type | endpoint-id | engine-name
--
-- * 'desMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'desMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeEndpoints
    :: DescribeEndpoints
describeEndpoints =
  DescribeEndpoints'
    {_desFilters = Nothing, _desMarker = Nothing, _desMaxRecords = Nothing}


-- | Filters applied to the describe action. Valid filter names: endpoint-arn | endpoint-type | endpoint-id | engine-name
desFilters :: Lens' DescribeEndpoints [Filter]
desFilters = lens _desFilters (\ s a -> s{_desFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
desMarker :: Lens' DescribeEndpoints (Maybe Text)
desMarker = lens _desMarker (\ s a -> s{_desMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
desMaxRecords :: Lens' DescribeEndpoints (Maybe Int)
desMaxRecords = lens _desMaxRecords (\ s a -> s{_desMaxRecords = a})

instance AWSPager DescribeEndpoints where
        page rq rs
          | stop (rs ^. dersMarker) = Nothing
          | stop (rs ^. dersEndpoints) = Nothing
          | otherwise =
            Just $ rq & desMarker .~ rs ^. dersMarker

instance AWSRequest DescribeEndpoints where
        type Rs DescribeEndpoints = DescribeEndpointsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Endpoints" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEndpoints where

instance NFData DescribeEndpoints where

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
                 [("Filters" .=) <$> _desFilters,
                  ("Marker" .=) <$> _desMarker,
                  ("MaxRecords" .=) <$> _desMaxRecords])

instance ToPath DescribeEndpoints where
        toPath = const "/"

instance ToQuery DescribeEndpoints where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { _dersMarker         :: !(Maybe Text)
  , _dersEndpoints      :: !(Maybe [Endpoint])
  , _dersResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dersEndpoints' - Endpoint description.
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEndpointsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEndpointsResponse
describeEndpointsResponse pResponseStatus_ =
  DescribeEndpointsResponse'
    { _dersMarker = Nothing
    , _dersEndpoints = Nothing
    , _dersResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dersMarker :: Lens' DescribeEndpointsResponse (Maybe Text)
dersMarker = lens _dersMarker (\ s a -> s{_dersMarker = a})

-- | Endpoint description.
dersEndpoints :: Lens' DescribeEndpointsResponse [Endpoint]
dersEndpoints = lens _dersEndpoints (\ s a -> s{_dersEndpoints = a}) . _Default . _Coerce

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEndpointsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEndpointsResponse where
