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
-- Module      : Network.AWS.AlexaBusiness.CreateRoom
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room with the specified details.
--
--
module Network.AWS.AlexaBusiness.CreateRoom
    (
    -- * Creating a Request
      createRoom
    , CreateRoom
    -- * Request Lenses
    , crProfileARN
    , crProviderCalendarId
    , crClientRequestToken
    , crDescription
    , crTags
    , crRoomName

    -- * Destructuring the Response
    , createRoomResponse
    , CreateRoomResponse
    -- * Response Lenses
    , crrsRoomARN
    , crrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRoom' smart constructor.
data CreateRoom = CreateRoom'
  { _crProfileARN         :: !(Maybe Text)
  , _crProviderCalendarId :: !(Maybe Text)
  , _crClientRequestToken :: !(Maybe Text)
  , _crDescription        :: !(Maybe Text)
  , _crTags               :: !(Maybe [Tag])
  , _crRoomName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRoom' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crProfileARN' - The profile ARN for the room.
--
-- * 'crProviderCalendarId' - The calendar ARN for the room.
--
-- * 'crClientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
--
-- * 'crDescription' - The description for the room.
--
-- * 'crTags' - The tags for the room.
--
-- * 'crRoomName' - The name for the room.
createRoom
    :: Text -- ^ 'crRoomName'
    -> CreateRoom
createRoom pRoomName_ =
  CreateRoom'
    { _crProfileARN = Nothing
    , _crProviderCalendarId = Nothing
    , _crClientRequestToken = Nothing
    , _crDescription = Nothing
    , _crTags = Nothing
    , _crRoomName = pRoomName_
    }


-- | The profile ARN for the room.
crProfileARN :: Lens' CreateRoom (Maybe Text)
crProfileARN = lens _crProfileARN (\ s a -> s{_crProfileARN = a})

-- | The calendar ARN for the room.
crProviderCalendarId :: Lens' CreateRoom (Maybe Text)
crProviderCalendarId = lens _crProviderCalendarId (\ s a -> s{_crProviderCalendarId = a})

-- | A unique, user-specified identifier for this request that ensures idempotency.
crClientRequestToken :: Lens' CreateRoom (Maybe Text)
crClientRequestToken = lens _crClientRequestToken (\ s a -> s{_crClientRequestToken = a})

-- | The description for the room.
crDescription :: Lens' CreateRoom (Maybe Text)
crDescription = lens _crDescription (\ s a -> s{_crDescription = a})

-- | The tags for the room.
crTags :: Lens' CreateRoom [Tag]
crTags = lens _crTags (\ s a -> s{_crTags = a}) . _Default . _Coerce

-- | The name for the room.
crRoomName :: Lens' CreateRoom Text
crRoomName = lens _crRoomName (\ s a -> s{_crRoomName = a})

instance AWSRequest CreateRoom where
        type Rs CreateRoom = CreateRoomResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateRoomResponse' <$>
                   (x .?> "RoomArn") <*> (pure (fromEnum s)))

instance Hashable CreateRoom where

instance NFData CreateRoom where

instance ToHeaders CreateRoom where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateRoom" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRoom where
        toJSON CreateRoom'{..}
          = object
              (catMaybes
                 [("ProfileArn" .=) <$> _crProfileARN,
                  ("ProviderCalendarId" .=) <$> _crProviderCalendarId,
                  ("ClientRequestToken" .=) <$> _crClientRequestToken,
                  ("Description" .=) <$> _crDescription,
                  ("Tags" .=) <$> _crTags,
                  Just ("RoomName" .= _crRoomName)])

instance ToPath CreateRoom where
        toPath = const "/"

instance ToQuery CreateRoom where
        toQuery = const mempty

-- | /See:/ 'createRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { _crrsRoomARN        :: !(Maybe Text)
  , _crrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRoomResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRoomARN' - The ARN of the newly created room in the response.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createRoomResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRoomResponse
createRoomResponse pResponseStatus_ =
  CreateRoomResponse'
    {_crrsRoomARN = Nothing, _crrsResponseStatus = pResponseStatus_}


-- | The ARN of the newly created room in the response.
crrsRoomARN :: Lens' CreateRoomResponse (Maybe Text)
crrsRoomARN = lens _crrsRoomARN (\ s a -> s{_crrsRoomARN = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRoomResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a})

instance NFData CreateRoomResponse where
