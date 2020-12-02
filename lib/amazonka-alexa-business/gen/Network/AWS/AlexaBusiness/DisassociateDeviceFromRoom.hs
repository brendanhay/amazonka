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
-- Module      : Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.
--
--
module Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
    (
    -- * Creating a Request
      disassociateDeviceFromRoom
    , DisassociateDeviceFromRoom
    -- * Request Lenses
    , ddfrDeviceARN

    -- * Destructuring the Response
    , disassociateDeviceFromRoomResponse
    , DisassociateDeviceFromRoomResponse
    -- * Response Lenses
    , ddfrrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateDeviceFromRoom' smart constructor.
newtype DisassociateDeviceFromRoom = DisassociateDeviceFromRoom'
  { _ddfrDeviceARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDeviceFromRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddfrDeviceARN' - The ARN of the device to disassociate from a room. Required.
disassociateDeviceFromRoom
    :: DisassociateDeviceFromRoom
disassociateDeviceFromRoom =
  DisassociateDeviceFromRoom' {_ddfrDeviceARN = Nothing}


-- | The ARN of the device to disassociate from a room. Required.
ddfrDeviceARN :: Lens' DisassociateDeviceFromRoom (Maybe Text)
ddfrDeviceARN = lens _ddfrDeviceARN (\ s a -> s{_ddfrDeviceARN = a})

instance AWSRequest DisassociateDeviceFromRoom where
        type Rs DisassociateDeviceFromRoom =
             DisassociateDeviceFromRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateDeviceFromRoomResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateDeviceFromRoom where

instance NFData DisassociateDeviceFromRoom where

instance ToHeaders DisassociateDeviceFromRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DisassociateDeviceFromRoom" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateDeviceFromRoom where
        toJSON DisassociateDeviceFromRoom'{..}
          = object
              (catMaybes [("DeviceArn" .=) <$> _ddfrDeviceARN])

instance ToPath DisassociateDeviceFromRoom where
        toPath = const "/"

instance ToQuery DisassociateDeviceFromRoom where
        toQuery = const mempty

-- | /See:/ 'disassociateDeviceFromRoomResponse' smart constructor.
newtype DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse'
  { _ddfrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDeviceFromRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddfrrsResponseStatus' - -- | The response status code.
disassociateDeviceFromRoomResponse
    :: Int -- ^ 'ddfrrsResponseStatus'
    -> DisassociateDeviceFromRoomResponse
disassociateDeviceFromRoomResponse pResponseStatus_ =
  DisassociateDeviceFromRoomResponse' {_ddfrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddfrrsResponseStatus :: Lens' DisassociateDeviceFromRoomResponse Int
ddfrrsResponseStatus = lens _ddfrrsResponseStatus (\ s a -> s{_ddfrrsResponseStatus = a})

instance NFData DisassociateDeviceFromRoomResponse
         where
