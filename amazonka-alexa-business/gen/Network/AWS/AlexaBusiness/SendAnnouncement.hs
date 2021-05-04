{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SendAnnouncement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Triggers an asynchronous flow to send text, SSML, or audio announcements
-- to rooms that are identified by a search or filter.
module Network.AWS.AlexaBusiness.SendAnnouncement
  ( -- * Creating a Request
    SendAnnouncement (..),
    newSendAnnouncement,

    -- * Request Lenses
    sendAnnouncement_timeToLiveInSeconds,
    sendAnnouncement_roomFilters,
    sendAnnouncement_content,
    sendAnnouncement_clientRequestToken,

    -- * Destructuring the Response
    SendAnnouncementResponse (..),
    newSendAnnouncementResponse,

    -- * Response Lenses
    sendAnnouncementResponse_announcementArn,
    sendAnnouncementResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSendAnnouncement' smart constructor.
data SendAnnouncement = SendAnnouncement'
  { -- | The time to live for an announcement. Default is 300. If delivery
    -- doesn\'t occur within this time, the announcement is not delivered.
    timeToLiveInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The filters to use to send an announcement to a specified list of rooms.
    -- The supported filter keys are RoomName, ProfileName, RoomArn, and
    -- ProfileArn. To send to all rooms, specify an empty RoomFilters list.
    roomFilters :: [Filter],
    -- | The announcement content. This can contain only one of the three
    -- possible announcement types (text, SSML or audio).
    content :: Content,
    -- | The unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendAnnouncement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLiveInSeconds', 'sendAnnouncement_timeToLiveInSeconds' - The time to live for an announcement. Default is 300. If delivery
-- doesn\'t occur within this time, the announcement is not delivered.
--
-- 'roomFilters', 'sendAnnouncement_roomFilters' - The filters to use to send an announcement to a specified list of rooms.
-- The supported filter keys are RoomName, ProfileName, RoomArn, and
-- ProfileArn. To send to all rooms, specify an empty RoomFilters list.
--
-- 'content', 'sendAnnouncement_content' - The announcement content. This can contain only one of the three
-- possible announcement types (text, SSML or audio).
--
-- 'clientRequestToken', 'sendAnnouncement_clientRequestToken' - The unique, user-specified identifier for the request that ensures
-- idempotency.
newSendAnnouncement ::
  -- | 'content'
  Content ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  SendAnnouncement
newSendAnnouncement pContent_ pClientRequestToken_ =
  SendAnnouncement'
    { timeToLiveInSeconds =
        Prelude.Nothing,
      roomFilters = Prelude.mempty,
      content = pContent_,
      clientRequestToken = pClientRequestToken_
    }

-- | The time to live for an announcement. Default is 300. If delivery
-- doesn\'t occur within this time, the announcement is not delivered.
sendAnnouncement_timeToLiveInSeconds :: Lens.Lens' SendAnnouncement (Prelude.Maybe Prelude.Natural)
sendAnnouncement_timeToLiveInSeconds = Lens.lens (\SendAnnouncement' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@SendAnnouncement' {} a -> s {timeToLiveInSeconds = a} :: SendAnnouncement)

-- | The filters to use to send an announcement to a specified list of rooms.
-- The supported filter keys are RoomName, ProfileName, RoomArn, and
-- ProfileArn. To send to all rooms, specify an empty RoomFilters list.
sendAnnouncement_roomFilters :: Lens.Lens' SendAnnouncement [Filter]
sendAnnouncement_roomFilters = Lens.lens (\SendAnnouncement' {roomFilters} -> roomFilters) (\s@SendAnnouncement' {} a -> s {roomFilters = a} :: SendAnnouncement) Prelude.. Prelude._Coerce

-- | The announcement content. This can contain only one of the three
-- possible announcement types (text, SSML or audio).
sendAnnouncement_content :: Lens.Lens' SendAnnouncement Content
sendAnnouncement_content = Lens.lens (\SendAnnouncement' {content} -> content) (\s@SendAnnouncement' {} a -> s {content = a} :: SendAnnouncement)

-- | The unique, user-specified identifier for the request that ensures
-- idempotency.
sendAnnouncement_clientRequestToken :: Lens.Lens' SendAnnouncement Prelude.Text
sendAnnouncement_clientRequestToken = Lens.lens (\SendAnnouncement' {clientRequestToken} -> clientRequestToken) (\s@SendAnnouncement' {} a -> s {clientRequestToken = a} :: SendAnnouncement)

instance Prelude.AWSRequest SendAnnouncement where
  type Rs SendAnnouncement = SendAnnouncementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendAnnouncementResponse'
            Prelude.<$> (x Prelude..?> "AnnouncementArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendAnnouncement

instance Prelude.NFData SendAnnouncement

instance Prelude.ToHeaders SendAnnouncement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.SendAnnouncement" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SendAnnouncement where
  toJSON SendAnnouncement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TimeToLiveInSeconds" Prelude..=)
              Prelude.<$> timeToLiveInSeconds,
            Prelude.Just ("RoomFilters" Prelude..= roomFilters),
            Prelude.Just ("Content" Prelude..= content),
            Prelude.Just
              ( "ClientRequestToken"
                  Prelude..= clientRequestToken
              )
          ]
      )

instance Prelude.ToPath SendAnnouncement where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SendAnnouncement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendAnnouncementResponse' smart constructor.
data SendAnnouncementResponse = SendAnnouncementResponse'
  { -- | The identifier of the announcement.
    announcementArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendAnnouncementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'announcementArn', 'sendAnnouncementResponse_announcementArn' - The identifier of the announcement.
--
-- 'httpStatus', 'sendAnnouncementResponse_httpStatus' - The response's http status code.
newSendAnnouncementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendAnnouncementResponse
newSendAnnouncementResponse pHttpStatus_ =
  SendAnnouncementResponse'
    { announcementArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the announcement.
sendAnnouncementResponse_announcementArn :: Lens.Lens' SendAnnouncementResponse (Prelude.Maybe Prelude.Text)
sendAnnouncementResponse_announcementArn = Lens.lens (\SendAnnouncementResponse' {announcementArn} -> announcementArn) (\s@SendAnnouncementResponse' {} a -> s {announcementArn = a} :: SendAnnouncementResponse)

-- | The response's http status code.
sendAnnouncementResponse_httpStatus :: Lens.Lens' SendAnnouncementResponse Prelude.Int
sendAnnouncementResponse_httpStatus = Lens.lens (\SendAnnouncementResponse' {httpStatus} -> httpStatus) (\s@SendAnnouncementResponse' {} a -> s {httpStatus = a} :: SendAnnouncementResponse)

instance Prelude.NFData SendAnnouncementResponse
