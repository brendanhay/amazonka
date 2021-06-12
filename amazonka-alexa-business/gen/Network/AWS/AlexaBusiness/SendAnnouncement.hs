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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSendAnnouncement' smart constructor.
data SendAnnouncement = SendAnnouncement'
  { -- | The time to live for an announcement. Default is 300. If delivery
    -- doesn\'t occur within this time, the announcement is not delivered.
    timeToLiveInSeconds :: Core.Maybe Core.Natural,
    -- | The filters to use to send an announcement to a specified list of rooms.
    -- The supported filter keys are RoomName, ProfileName, RoomArn, and
    -- ProfileArn. To send to all rooms, specify an empty RoomFilters list.
    roomFilters :: [Filter],
    -- | The announcement content. This can contain only one of the three
    -- possible announcement types (text, SSML or audio).
    content :: Content,
    -- | The unique, user-specified identifier for the request that ensures
    -- idempotency.
    clientRequestToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  SendAnnouncement
newSendAnnouncement pContent_ pClientRequestToken_ =
  SendAnnouncement'
    { timeToLiveInSeconds =
        Core.Nothing,
      roomFilters = Core.mempty,
      content = pContent_,
      clientRequestToken = pClientRequestToken_
    }

-- | The time to live for an announcement. Default is 300. If delivery
-- doesn\'t occur within this time, the announcement is not delivered.
sendAnnouncement_timeToLiveInSeconds :: Lens.Lens' SendAnnouncement (Core.Maybe Core.Natural)
sendAnnouncement_timeToLiveInSeconds = Lens.lens (\SendAnnouncement' {timeToLiveInSeconds} -> timeToLiveInSeconds) (\s@SendAnnouncement' {} a -> s {timeToLiveInSeconds = a} :: SendAnnouncement)

-- | The filters to use to send an announcement to a specified list of rooms.
-- The supported filter keys are RoomName, ProfileName, RoomArn, and
-- ProfileArn. To send to all rooms, specify an empty RoomFilters list.
sendAnnouncement_roomFilters :: Lens.Lens' SendAnnouncement [Filter]
sendAnnouncement_roomFilters = Lens.lens (\SendAnnouncement' {roomFilters} -> roomFilters) (\s@SendAnnouncement' {} a -> s {roomFilters = a} :: SendAnnouncement) Core.. Lens._Coerce

-- | The announcement content. This can contain only one of the three
-- possible announcement types (text, SSML or audio).
sendAnnouncement_content :: Lens.Lens' SendAnnouncement Content
sendAnnouncement_content = Lens.lens (\SendAnnouncement' {content} -> content) (\s@SendAnnouncement' {} a -> s {content = a} :: SendAnnouncement)

-- | The unique, user-specified identifier for the request that ensures
-- idempotency.
sendAnnouncement_clientRequestToken :: Lens.Lens' SendAnnouncement Core.Text
sendAnnouncement_clientRequestToken = Lens.lens (\SendAnnouncement' {clientRequestToken} -> clientRequestToken) (\s@SendAnnouncement' {} a -> s {clientRequestToken = a} :: SendAnnouncement)

instance Core.AWSRequest SendAnnouncement where
  type
    AWSResponse SendAnnouncement =
      SendAnnouncementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SendAnnouncementResponse'
            Core.<$> (x Core..?> "AnnouncementArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SendAnnouncement

instance Core.NFData SendAnnouncement

instance Core.ToHeaders SendAnnouncement where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SendAnnouncement" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SendAnnouncement where
  toJSON SendAnnouncement' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TimeToLiveInSeconds" Core..=)
              Core.<$> timeToLiveInSeconds,
            Core.Just ("RoomFilters" Core..= roomFilters),
            Core.Just ("Content" Core..= content),
            Core.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath SendAnnouncement where
  toPath = Core.const "/"

instance Core.ToQuery SendAnnouncement where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSendAnnouncementResponse' smart constructor.
data SendAnnouncementResponse = SendAnnouncementResponse'
  { -- | The identifier of the announcement.
    announcementArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SendAnnouncementResponse
newSendAnnouncementResponse pHttpStatus_ =
  SendAnnouncementResponse'
    { announcementArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the announcement.
sendAnnouncementResponse_announcementArn :: Lens.Lens' SendAnnouncementResponse (Core.Maybe Core.Text)
sendAnnouncementResponse_announcementArn = Lens.lens (\SendAnnouncementResponse' {announcementArn} -> announcementArn) (\s@SendAnnouncementResponse' {} a -> s {announcementArn = a} :: SendAnnouncementResponse)

-- | The response's http status code.
sendAnnouncementResponse_httpStatus :: Lens.Lens' SendAnnouncementResponse Core.Int
sendAnnouncementResponse_httpStatus = Lens.lens (\SendAnnouncementResponse' {httpStatus} -> httpStatus) (\s@SendAnnouncementResponse' {} a -> s {httpStatus = a} :: SendAnnouncementResponse)

instance Core.NFData SendAnnouncementResponse
