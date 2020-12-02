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
-- Module      : Network.AWS.DeviceFarm.ListDeviceInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the private device instances associated with one or more AWS accounts.
--
--
module Network.AWS.DeviceFarm.ListDeviceInstances
    (
    -- * Creating a Request
      listDeviceInstances
    , ListDeviceInstances
    -- * Request Lenses
    , ldiNextToken
    , ldiMaxResults

    -- * Destructuring the Response
    , listDeviceInstancesResponse
    , ListDeviceInstancesResponse
    -- * Response Lenses
    , ldirsNextToken
    , ldirsDeviceInstances
    , ldirsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeviceInstances' smart constructor.
data ListDeviceInstances = ListDeviceInstances'
  { _ldiNextToken  :: !(Maybe Text)
  , _ldiMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldiNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'ldiMaxResults' - An integer specifying the maximum number of items you want to return in the API response.
listDeviceInstances
    :: ListDeviceInstances
listDeviceInstances =
  ListDeviceInstances' {_ldiNextToken = Nothing, _ldiMaxResults = Nothing}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
ldiNextToken :: Lens' ListDeviceInstances (Maybe Text)
ldiNextToken = lens _ldiNextToken (\ s a -> s{_ldiNextToken = a})

-- | An integer specifying the maximum number of items you want to return in the API response.
ldiMaxResults :: Lens' ListDeviceInstances (Maybe Int)
ldiMaxResults = lens _ldiMaxResults (\ s a -> s{_ldiMaxResults = a})

instance AWSRequest ListDeviceInstances where
        type Rs ListDeviceInstances =
             ListDeviceInstancesResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListDeviceInstancesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "deviceInstances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeviceInstances where

instance NFData ListDeviceInstances where

instance ToHeaders ListDeviceInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListDeviceInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeviceInstances where
        toJSON ListDeviceInstances'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ldiNextToken,
                  ("maxResults" .=) <$> _ldiMaxResults])

instance ToPath ListDeviceInstances where
        toPath = const "/"

instance ToQuery ListDeviceInstances where
        toQuery = const mempty

-- | /See:/ 'listDeviceInstancesResponse' smart constructor.
data ListDeviceInstancesResponse = ListDeviceInstancesResponse'
  { _ldirsNextToken       :: !(Maybe Text)
  , _ldirsDeviceInstances :: !(Maybe [DeviceInstance])
  , _ldirsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeviceInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldirsNextToken' - An identifier that can be used in the next call to this operation to return the next set of items in the list.
--
-- * 'ldirsDeviceInstances' - An object containing information about your device instances.
--
-- * 'ldirsResponseStatus' - -- | The response status code.
listDeviceInstancesResponse
    :: Int -- ^ 'ldirsResponseStatus'
    -> ListDeviceInstancesResponse
listDeviceInstancesResponse pResponseStatus_ =
  ListDeviceInstancesResponse'
    { _ldirsNextToken = Nothing
    , _ldirsDeviceInstances = Nothing
    , _ldirsResponseStatus = pResponseStatus_
    }


-- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
ldirsNextToken :: Lens' ListDeviceInstancesResponse (Maybe Text)
ldirsNextToken = lens _ldirsNextToken (\ s a -> s{_ldirsNextToken = a})

-- | An object containing information about your device instances.
ldirsDeviceInstances :: Lens' ListDeviceInstancesResponse [DeviceInstance]
ldirsDeviceInstances = lens _ldirsDeviceInstances (\ s a -> s{_ldirsDeviceInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
ldirsResponseStatus :: Lens' ListDeviceInstancesResponse Int
ldirsResponseStatus = lens _ldirsResponseStatus (\ s a -> s{_ldirsResponseStatus = a})

instance NFData ListDeviceInstancesResponse where
