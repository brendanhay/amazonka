{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SendAnnouncement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Triggers an asynchronous flow to send text, SSML, or audio announcements to rooms that are identified by a search or filter.
module Network.AWS.AlexaBusiness.SendAnnouncement
  ( -- * Creating a Request
    sendAnnouncement,
    SendAnnouncement,

    -- * Request Lenses
    saTimeToLiveInSeconds,
    saRoomFilters,
    saContent,
    saClientRequestToken,

    -- * Destructuring the Response
    sendAnnouncementResponse,
    SendAnnouncementResponse,

    -- * Response Lenses
    sarsAnnouncementARN,
    sarsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendAnnouncement' smart constructor.
data SendAnnouncement = SendAnnouncement'
  { _saTimeToLiveInSeconds ::
      !(Maybe Nat),
    _saRoomFilters :: ![Filter],
    _saContent :: !Content,
    _saClientRequestToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendAnnouncement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saTimeToLiveInSeconds' - The time to live for an announcement. Default is 300. If delivery doesn't occur within this time, the announcement is not delivered.
--
-- * 'saRoomFilters' - The filters to use to send an announcement to a specified list of rooms. The supported filter keys are RoomName, ProfileName, RoomArn, and ProfileArn. To send to all rooms, specify an empty RoomFilters list.
--
-- * 'saContent' - The announcement content. This can contain only one of the three possible announcement types (text, SSML or audio).
--
-- * 'saClientRequestToken' - The unique, user-specified identifier for the request that ensures idempotency.
sendAnnouncement ::
  -- | 'saContent'
  Content ->
  -- | 'saClientRequestToken'
  Text ->
  SendAnnouncement
sendAnnouncement pContent_ pClientRequestToken_ =
  SendAnnouncement'
    { _saTimeToLiveInSeconds = Nothing,
      _saRoomFilters = mempty,
      _saContent = pContent_,
      _saClientRequestToken = pClientRequestToken_
    }

-- | The time to live for an announcement. Default is 300. If delivery doesn't occur within this time, the announcement is not delivered.
saTimeToLiveInSeconds :: Lens' SendAnnouncement (Maybe Natural)
saTimeToLiveInSeconds = lens _saTimeToLiveInSeconds (\s a -> s {_saTimeToLiveInSeconds = a}) . mapping _Nat

-- | The filters to use to send an announcement to a specified list of rooms. The supported filter keys are RoomName, ProfileName, RoomArn, and ProfileArn. To send to all rooms, specify an empty RoomFilters list.
saRoomFilters :: Lens' SendAnnouncement [Filter]
saRoomFilters = lens _saRoomFilters (\s a -> s {_saRoomFilters = a}) . _Coerce

-- | The announcement content. This can contain only one of the three possible announcement types (text, SSML or audio).
saContent :: Lens' SendAnnouncement Content
saContent = lens _saContent (\s a -> s {_saContent = a})

-- | The unique, user-specified identifier for the request that ensures idempotency.
saClientRequestToken :: Lens' SendAnnouncement Text
saClientRequestToken = lens _saClientRequestToken (\s a -> s {_saClientRequestToken = a})

instance AWSRequest SendAnnouncement where
  type Rs SendAnnouncement = SendAnnouncementResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          SendAnnouncementResponse'
            <$> (x .?> "AnnouncementArn") <*> (pure (fromEnum s))
      )

instance Hashable SendAnnouncement

instance NFData SendAnnouncement

instance ToHeaders SendAnnouncement where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.SendAnnouncement" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SendAnnouncement where
  toJSON SendAnnouncement' {..} =
    object
      ( catMaybes
          [ ("TimeToLiveInSeconds" .=) <$> _saTimeToLiveInSeconds,
            Just ("RoomFilters" .= _saRoomFilters),
            Just ("Content" .= _saContent),
            Just ("ClientRequestToken" .= _saClientRequestToken)
          ]
      )

instance ToPath SendAnnouncement where
  toPath = const "/"

instance ToQuery SendAnnouncement where
  toQuery = const mempty

-- | /See:/ 'sendAnnouncementResponse' smart constructor.
data SendAnnouncementResponse = SendAnnouncementResponse'
  { _sarsAnnouncementARN ::
      !(Maybe Text),
    _sarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendAnnouncementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarsAnnouncementARN' - The identifier of the announcement.
--
-- * 'sarsResponseStatus' - -- | The response status code.
sendAnnouncementResponse ::
  -- | 'sarsResponseStatus'
  Int ->
  SendAnnouncementResponse
sendAnnouncementResponse pResponseStatus_ =
  SendAnnouncementResponse'
    { _sarsAnnouncementARN = Nothing,
      _sarsResponseStatus = pResponseStatus_
    }

-- | The identifier of the announcement.
sarsAnnouncementARN :: Lens' SendAnnouncementResponse (Maybe Text)
sarsAnnouncementARN = lens _sarsAnnouncementARN (\s a -> s {_sarsAnnouncementARN = a})

-- | -- | The response status code.
sarsResponseStatus :: Lens' SendAnnouncementResponse Int
sarsResponseStatus = lens _sarsResponseStatus (\s a -> s {_sarsResponseStatus = a})

instance NFData SendAnnouncementResponse
