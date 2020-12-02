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
-- Module      : Network.AWS.AlexaBusiness.SearchDevices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches devices and lists the ones that meet a set of filter criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchDevices
    (
    -- * Creating a Request
      searchDevices
    , SearchDevices
    -- * Request Lenses
    , sdFilters
    , sdSortCriteria
    , sdNextToken
    , sdMaxResults

    -- * Destructuring the Response
    , searchDevicesResponse
    , SearchDevicesResponse
    -- * Response Lenses
    , sdrsNextToken
    , sdrsDevices
    , sdrsTotalCount
    , sdrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchDevices' smart constructor.
data SearchDevices = SearchDevices'
  { _sdFilters      :: !(Maybe [Filter])
  , _sdSortCriteria :: !(Maybe [Sort])
  , _sdNextToken    :: !(Maybe Text)
  , _sdMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchDevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdFilters' - The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, and ConnectionStatus (ONLINE and OFFLINE).
--
-- * 'sdSortCriteria' - The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, and ConnectionStatus.
--
-- * 'sdNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'sdMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
searchDevices
    :: SearchDevices
searchDevices =
  SearchDevices'
    { _sdFilters = Nothing
    , _sdSortCriteria = Nothing
    , _sdNextToken = Nothing
    , _sdMaxResults = Nothing
    }


-- | The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, and ConnectionStatus (ONLINE and OFFLINE).
sdFilters :: Lens' SearchDevices [Filter]
sdFilters = lens _sdFilters (\ s a -> s{_sdFilters = a}) . _Default . _Coerce

-- | The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, and ConnectionStatus.
sdSortCriteria :: Lens' SearchDevices [Sort]
sdSortCriteria = lens _sdSortCriteria (\ s a -> s{_sdSortCriteria = a}) . _Default . _Coerce

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
sdNextToken :: Lens' SearchDevices (Maybe Text)
sdNextToken = lens _sdNextToken (\ s a -> s{_sdNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
sdMaxResults :: Lens' SearchDevices (Maybe Natural)
sdMaxResults = lens _sdMaxResults (\ s a -> s{_sdMaxResults = a}) . mapping _Nat

instance AWSPager SearchDevices where
        page rq rs
          | stop (rs ^. sdrsNextToken) = Nothing
          | stop (rs ^. sdrsDevices) = Nothing
          | otherwise =
            Just $ rq & sdNextToken .~ rs ^. sdrsNextToken

instance AWSRequest SearchDevices where
        type Rs SearchDevices = SearchDevicesResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 SearchDevicesResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Devices" .!@ mempty)
                     <*> (x .?> "TotalCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchDevices where

instance NFData SearchDevices where

instance ToHeaders SearchDevices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.SearchDevices" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchDevices where
        toJSON SearchDevices'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _sdFilters,
                  ("SortCriteria" .=) <$> _sdSortCriteria,
                  ("NextToken" .=) <$> _sdNextToken,
                  ("MaxResults" .=) <$> _sdMaxResults])

instance ToPath SearchDevices where
        toPath = const "/"

instance ToQuery SearchDevices where
        toQuery = const mempty

-- | /See:/ 'searchDevicesResponse' smart constructor.
data SearchDevicesResponse = SearchDevicesResponse'
  { _sdrsNextToken      :: !(Maybe Text)
  , _sdrsDevices        :: !(Maybe [DeviceData])
  , _sdrsTotalCount     :: !(Maybe Int)
  , _sdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchDevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'sdrsDevices' - The devices that meet the specified set of filter criteria, in sort order.
--
-- * 'sdrsTotalCount' - The total number of devices returned.
--
-- * 'sdrsResponseStatus' - -- | The response status code.
searchDevicesResponse
    :: Int -- ^ 'sdrsResponseStatus'
    -> SearchDevicesResponse
searchDevicesResponse pResponseStatus_ =
  SearchDevicesResponse'
    { _sdrsNextToken = Nothing
    , _sdrsDevices = Nothing
    , _sdrsTotalCount = Nothing
    , _sdrsResponseStatus = pResponseStatus_
    }


-- | The token returned to indicate that there is more data available.
sdrsNextToken :: Lens' SearchDevicesResponse (Maybe Text)
sdrsNextToken = lens _sdrsNextToken (\ s a -> s{_sdrsNextToken = a})

-- | The devices that meet the specified set of filter criteria, in sort order.
sdrsDevices :: Lens' SearchDevicesResponse [DeviceData]
sdrsDevices = lens _sdrsDevices (\ s a -> s{_sdrsDevices = a}) . _Default . _Coerce

-- | The total number of devices returned.
sdrsTotalCount :: Lens' SearchDevicesResponse (Maybe Int)
sdrsTotalCount = lens _sdrsTotalCount (\ s a -> s{_sdrsTotalCount = a})

-- | -- | The response status code.
sdrsResponseStatus :: Lens' SearchDevicesResponse Int
sdrsResponseStatus = lens _sdrsResponseStatus (\ s a -> s{_sdrsResponseStatus = a})

instance NFData SearchDevicesResponse where
