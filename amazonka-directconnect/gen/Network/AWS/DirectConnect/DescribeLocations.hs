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
-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of AWS Direct Connect locations in the current AWS region. These are the locations that may be selected when calling 'CreateConnection' or 'CreateInterconnect' .
--
--
module Network.AWS.DirectConnect.DescribeLocations
    (
    -- * Creating a Request
      describeLocations
    , DescribeLocations

    -- * Destructuring the Response
    , describeLocationsResponse
    , DescribeLocationsResponse
    -- * Response Lenses
    , dlsrsLocations
    , dlsrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLocations' smart constructor.
data DescribeLocations =
  DescribeLocations'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocations' with the minimum fields required to make a request.
--
describeLocations
    :: DescribeLocations
describeLocations = DescribeLocations'


instance AWSRequest DescribeLocations where
        type Rs DescribeLocations = DescribeLocationsResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLocationsResponse' <$>
                   (x .?> "locations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeLocations where

instance NFData DescribeLocations where

instance ToHeaders DescribeLocations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeLocations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLocations where
        toJSON = const (Object mempty)

instance ToPath DescribeLocations where
        toPath = const "/"

instance ToQuery DescribeLocations where
        toQuery = const mempty

-- | A location is a network facility where AWS Direct Connect routers are available to be connected. Generally, these are colocation hubs where many network providers have equipment, and where cross connects can be delivered. Locations include a name and facility code, and must be provided when creating a connection.
--
--
--
-- /See:/ 'describeLocationsResponse' smart constructor.
data DescribeLocationsResponse = DescribeLocationsResponse'
  { _dlsrsLocations      :: !(Maybe [Location])
  , _dlsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsLocations' - A list of colocation hubs where network providers have equipment. Most regions have multiple locations available.
--
-- * 'dlsrsResponseStatus' - -- | The response status code.
describeLocationsResponse
    :: Int -- ^ 'dlsrsResponseStatus'
    -> DescribeLocationsResponse
describeLocationsResponse pResponseStatus_ =
  DescribeLocationsResponse'
    {_dlsrsLocations = Nothing, _dlsrsResponseStatus = pResponseStatus_}


-- | A list of colocation hubs where network providers have equipment. Most regions have multiple locations available.
dlsrsLocations :: Lens' DescribeLocationsResponse [Location]
dlsrsLocations = lens _dlsrsLocations (\ s a -> s{_dlsrsLocations = a}) . _Default . _Coerce

-- | -- | The response status code.
dlsrsResponseStatus :: Lens' DescribeLocationsResponse Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\ s a -> s{_dlsrsResponseStatus = a})

instance NFData DescribeLocationsResponse where
