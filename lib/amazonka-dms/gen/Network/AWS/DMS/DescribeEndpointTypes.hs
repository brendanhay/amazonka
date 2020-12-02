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
-- Module      : Network.AWS.DMS.DescribeEndpointTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the type of endpoints available.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpointTypes
    (
    -- * Creating a Request
      describeEndpointTypes
    , DescribeEndpointTypes
    -- * Request Lenses
    , detFilters
    , detMarker
    , detMaxRecords

    -- * Destructuring the Response
    , describeEndpointTypesResponse
    , DescribeEndpointTypesResponse
    -- * Response Lenses
    , detrsSupportedEndpointTypes
    , detrsMarker
    , detrsResponseStatus
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
-- /See:/ 'describeEndpointTypes' smart constructor.
data DescribeEndpointTypes = DescribeEndpointTypes'
  { _detFilters    :: !(Maybe [Filter])
  , _detMarker     :: !(Maybe Text)
  , _detMaxRecords :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detFilters' - Filters applied to the describe action. Valid filter names: engine-name | endpoint-type
--
-- * 'detMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'detMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeEndpointTypes
    :: DescribeEndpointTypes
describeEndpointTypes =
  DescribeEndpointTypes'
    {_detFilters = Nothing, _detMarker = Nothing, _detMaxRecords = Nothing}


-- | Filters applied to the describe action. Valid filter names: engine-name | endpoint-type
detFilters :: Lens' DescribeEndpointTypes [Filter]
detFilters = lens _detFilters (\ s a -> s{_detFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
detMarker :: Lens' DescribeEndpointTypes (Maybe Text)
detMarker = lens _detMarker (\ s a -> s{_detMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
detMaxRecords :: Lens' DescribeEndpointTypes (Maybe Int)
detMaxRecords = lens _detMaxRecords (\ s a -> s{_detMaxRecords = a})

instance AWSPager DescribeEndpointTypes where
        page rq rs
          | stop (rs ^. detrsMarker) = Nothing
          | stop (rs ^. detrsSupportedEndpointTypes) = Nothing
          | otherwise =
            Just $ rq & detMarker .~ rs ^. detrsMarker

instance AWSRequest DescribeEndpointTypes where
        type Rs DescribeEndpointTypes =
             DescribeEndpointTypesResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointTypesResponse' <$>
                   (x .?> "SupportedEndpointTypes" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEndpointTypes where

instance NFData DescribeEndpointTypes where

instance ToHeaders DescribeEndpointTypes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeEndpointTypes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEndpointTypes where
        toJSON DescribeEndpointTypes'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _detFilters,
                  ("Marker" .=) <$> _detMarker,
                  ("MaxRecords" .=) <$> _detMaxRecords])

instance ToPath DescribeEndpointTypes where
        toPath = const "/"

instance ToQuery DescribeEndpointTypes where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeEndpointTypesResponse' smart constructor.
data DescribeEndpointTypesResponse = DescribeEndpointTypesResponse'
  { _detrsSupportedEndpointTypes :: !(Maybe [SupportedEndpointType])
  , _detrsMarker                 :: !(Maybe Text)
  , _detrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsSupportedEndpointTypes' - The type of endpoints that are supported.
--
-- * 'detrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeEndpointTypesResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DescribeEndpointTypesResponse
describeEndpointTypesResponse pResponseStatus_ =
  DescribeEndpointTypesResponse'
    { _detrsSupportedEndpointTypes = Nothing
    , _detrsMarker = Nothing
    , _detrsResponseStatus = pResponseStatus_
    }


-- | The type of endpoints that are supported.
detrsSupportedEndpointTypes :: Lens' DescribeEndpointTypesResponse [SupportedEndpointType]
detrsSupportedEndpointTypes = lens _detrsSupportedEndpointTypes (\ s a -> s{_detrsSupportedEndpointTypes = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
detrsMarker :: Lens' DescribeEndpointTypesResponse (Maybe Text)
detrsMarker = lens _detrsMarker (\ s a -> s{_detrsMarker = a})

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeEndpointTypesResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a})

instance NFData DescribeEndpointTypesResponse where
