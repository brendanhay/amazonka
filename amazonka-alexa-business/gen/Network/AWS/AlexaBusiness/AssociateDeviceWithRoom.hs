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
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or else a manual sync is required.
--
--
module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
    (
    -- * Creating a Request
      associateDeviceWithRoom
    , AssociateDeviceWithRoom
    -- * Request Lenses
    , adwrDeviceARN
    , adwrRoomARN

    -- * Destructuring the Response
    , associateDeviceWithRoomResponse
    , AssociateDeviceWithRoomResponse
    -- * Response Lenses
    , adwrrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateDeviceWithRoom' smart constructor.
data AssociateDeviceWithRoom = AssociateDeviceWithRoom'
  { _adwrDeviceARN :: !(Maybe Text)
  , _adwrRoomARN   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDeviceWithRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adwrDeviceARN' - The ARN of the device to associate to a room. Required.
--
-- * 'adwrRoomARN' - The ARN of the room with which to associate the device. Required.
associateDeviceWithRoom
    :: AssociateDeviceWithRoom
associateDeviceWithRoom =
  AssociateDeviceWithRoom' {_adwrDeviceARN = Nothing, _adwrRoomARN = Nothing}


-- | The ARN of the device to associate to a room. Required.
adwrDeviceARN :: Lens' AssociateDeviceWithRoom (Maybe Text)
adwrDeviceARN = lens _adwrDeviceARN (\ s a -> s{_adwrDeviceARN = a})

-- | The ARN of the room with which to associate the device. Required.
adwrRoomARN :: Lens' AssociateDeviceWithRoom (Maybe Text)
adwrRoomARN = lens _adwrRoomARN (\ s a -> s{_adwrRoomARN = a})

instance AWSRequest AssociateDeviceWithRoom where
        type Rs AssociateDeviceWithRoom =
             AssociateDeviceWithRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateDeviceWithRoomResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateDeviceWithRoom where

instance NFData AssociateDeviceWithRoom where

instance ToHeaders AssociateDeviceWithRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.AssociateDeviceWithRoom" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateDeviceWithRoom where
        toJSON AssociateDeviceWithRoom'{..}
          = object
              (catMaybes
                 [("DeviceArn" .=) <$> _adwrDeviceARN,
                  ("RoomArn" .=) <$> _adwrRoomARN])

instance ToPath AssociateDeviceWithRoom where
        toPath = const "/"

instance ToQuery AssociateDeviceWithRoom where
        toQuery = const mempty

-- | /See:/ 'associateDeviceWithRoomResponse' smart constructor.
newtype AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse'
  { _adwrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDeviceWithRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adwrrsResponseStatus' - -- | The response status code.
associateDeviceWithRoomResponse
    :: Int -- ^ 'adwrrsResponseStatus'
    -> AssociateDeviceWithRoomResponse
associateDeviceWithRoomResponse pResponseStatus_ =
  AssociateDeviceWithRoomResponse' {_adwrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
adwrrsResponseStatus :: Lens' AssociateDeviceWithRoomResponse Int
adwrrsResponseStatus = lens _adwrrsResponseStatus (\ s a -> s{_adwrrsResponseStatus = a})

instance NFData AssociateDeviceWithRoomResponse where
