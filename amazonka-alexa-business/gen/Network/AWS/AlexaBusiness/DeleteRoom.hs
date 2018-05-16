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
-- Module      : Network.AWS.AlexaBusiness.DeleteRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room by the room ARN.
--
--
module Network.AWS.AlexaBusiness.DeleteRoom
    (
    -- * Creating a Request
      deleteRoom
    , DeleteRoom
    -- * Request Lenses
    , drRoomARN

    -- * Destructuring the Response
    , deleteRoomResponse
    , DeleteRoomResponse
    -- * Response Lenses
    , drrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRoom' smart constructor.
newtype DeleteRoom = DeleteRoom'
  { _drRoomARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRoomARN' - The ARN of the room to delete. Required.
deleteRoom
    :: DeleteRoom
deleteRoom = DeleteRoom' {_drRoomARN = Nothing}


-- | The ARN of the room to delete. Required.
drRoomARN :: Lens' DeleteRoom (Maybe Text)
drRoomARN = lens _drRoomARN (\ s a -> s{_drRoomARN = a})

instance AWSRequest DeleteRoom where
        type Rs DeleteRoom = DeleteRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteRoomResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteRoom where

instance NFData DeleteRoom where

instance ToHeaders DeleteRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DeleteRoom" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRoom where
        toJSON DeleteRoom'{..}
          = object (catMaybes [("RoomArn" .=) <$> _drRoomARN])

instance ToPath DeleteRoom where
        toPath = const "/"

instance ToQuery DeleteRoom where
        toQuery = const mempty

-- | /See:/ 'deleteRoomResponse' smart constructor.
newtype DeleteRoomResponse = DeleteRoomResponse'
  { _drrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteRoomResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DeleteRoomResponse
deleteRoomResponse pResponseStatus_ =
  DeleteRoomResponse' {_drrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteRoomResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DeleteRoomResponse where
