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
-- Module      : Network.AWS.AlexaBusiness.GetRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room details by room ARN.
--
--
module Network.AWS.AlexaBusiness.GetRoom
    (
    -- * Creating a Request
      getRoom
    , GetRoom
    -- * Request Lenses
    , grRoomARN

    -- * Destructuring the Response
    , getRoomResponse
    , GetRoomResponse
    -- * Response Lenses
    , grrsRoom
    , grrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRoom' smart constructor.
newtype GetRoom = GetRoom'
  { _grRoomARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grRoomARN' - The ARN of the room for which to request details. Required.
getRoom
    :: GetRoom
getRoom = GetRoom' {_grRoomARN = Nothing}


-- | The ARN of the room for which to request details. Required.
grRoomARN :: Lens' GetRoom (Maybe Text)
grRoomARN = lens _grRoomARN (\ s a -> s{_grRoomARN = a})

instance AWSRequest GetRoom where
        type Rs GetRoom = GetRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 GetRoomResponse' <$>
                   (x .?> "Room") <*> (pure (fromEnum s)))

instance Hashable GetRoom where

instance NFData GetRoom where

instance ToHeaders GetRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.GetRoom" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRoom where
        toJSON GetRoom'{..}
          = object (catMaybes [("RoomArn" .=) <$> _grRoomARN])

instance ToPath GetRoom where
        toPath = const "/"

instance ToQuery GetRoom where
        toQuery = const mempty

-- | /See:/ 'getRoomResponse' smart constructor.
data GetRoomResponse = GetRoomResponse'
  { _grrsRoom           :: !(Maybe Room)
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsRoom' - The details of the room requested.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRoomResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRoomResponse
getRoomResponse pResponseStatus_ =
  GetRoomResponse' {_grrsRoom = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | The details of the room requested.
grrsRoom :: Lens' GetRoomResponse (Maybe Room)
grrsRoom = lens _grrsRoom (\ s a -> s{_grrsRoom = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRoomResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRoomResponse where
