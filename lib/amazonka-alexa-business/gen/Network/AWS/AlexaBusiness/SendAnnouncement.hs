{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    SendAnnouncement (..),
    mkSendAnnouncement,

    -- ** Request lenses
    saTimeToLiveInSeconds,
    saRoomFilters,
    saContent,
    saClientRequestToken,

    -- * Destructuring the response
    SendAnnouncementResponse (..),
    mkSendAnnouncementResponse,

    -- ** Response lenses
    sarsAnnouncementARN,
    sarsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSendAnnouncement' smart constructor.
data SendAnnouncement = SendAnnouncement'
  { timeToLiveInSeconds ::
      Lude.Maybe Lude.Natural,
    roomFilters :: [Filter],
    content :: Content,
    clientRequestToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendAnnouncement' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - The unique, user-specified identifier for the request that ensures idempotency.
-- * 'content' - The announcement content. This can contain only one of the three possible announcement types (text, SSML or audio).
-- * 'roomFilters' - The filters to use to send an announcement to a specified list of rooms. The supported filter keys are RoomName, ProfileName, RoomArn, and ProfileArn. To send to all rooms, specify an empty RoomFilters list.
-- * 'timeToLiveInSeconds' - The time to live for an announcement. Default is 300. If delivery doesn't occur within this time, the announcement is not delivered.
mkSendAnnouncement ::
  -- | 'content'
  Content ->
  -- | 'clientRequestToken'
  Lude.Text ->
  SendAnnouncement
mkSendAnnouncement pContent_ pClientRequestToken_ =
  SendAnnouncement'
    { timeToLiveInSeconds = Lude.Nothing,
      roomFilters = Lude.mempty,
      content = pContent_,
      clientRequestToken = pClientRequestToken_
    }

-- | The time to live for an announcement. Default is 300. If delivery doesn't occur within this time, the announcement is not delivered.
--
-- /Note:/ Consider using 'timeToLiveInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saTimeToLiveInSeconds :: Lens.Lens' SendAnnouncement (Lude.Maybe Lude.Natural)
saTimeToLiveInSeconds = Lens.lens (timeToLiveInSeconds :: SendAnnouncement -> Lude.Maybe Lude.Natural) (\s a -> s {timeToLiveInSeconds = a} :: SendAnnouncement)
{-# DEPRECATED saTimeToLiveInSeconds "Use generic-lens or generic-optics with 'timeToLiveInSeconds' instead." #-}

-- | The filters to use to send an announcement to a specified list of rooms. The supported filter keys are RoomName, ProfileName, RoomArn, and ProfileArn. To send to all rooms, specify an empty RoomFilters list.
--
-- /Note:/ Consider using 'roomFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saRoomFilters :: Lens.Lens' SendAnnouncement [Filter]
saRoomFilters = Lens.lens (roomFilters :: SendAnnouncement -> [Filter]) (\s a -> s {roomFilters = a} :: SendAnnouncement)
{-# DEPRECATED saRoomFilters "Use generic-lens or generic-optics with 'roomFilters' instead." #-}

-- | The announcement content. This can contain only one of the three possible announcement types (text, SSML or audio).
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saContent :: Lens.Lens' SendAnnouncement Content
saContent = Lens.lens (content :: SendAnnouncement -> Content) (\s a -> s {content = a} :: SendAnnouncement)
{-# DEPRECATED saContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saClientRequestToken :: Lens.Lens' SendAnnouncement Lude.Text
saClientRequestToken = Lens.lens (clientRequestToken :: SendAnnouncement -> Lude.Text) (\s a -> s {clientRequestToken = a} :: SendAnnouncement)
{-# DEPRECATED saClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest SendAnnouncement where
  type Rs SendAnnouncement = SendAnnouncementResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SendAnnouncementResponse'
            Lude.<$> (x Lude..?> "AnnouncementArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendAnnouncement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SendAnnouncement" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SendAnnouncement where
  toJSON SendAnnouncement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TimeToLiveInSeconds" Lude..=) Lude.<$> timeToLiveInSeconds,
            Lude.Just ("RoomFilters" Lude..= roomFilters),
            Lude.Just ("Content" Lude..= content),
            Lude.Just ("ClientRequestToken" Lude..= clientRequestToken)
          ]
      )

instance Lude.ToPath SendAnnouncement where
  toPath = Lude.const "/"

instance Lude.ToQuery SendAnnouncement where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSendAnnouncementResponse' smart constructor.
data SendAnnouncementResponse = SendAnnouncementResponse'
  { announcementARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendAnnouncementResponse' with the minimum fields required to make a request.
--
-- * 'announcementARN' - The identifier of the announcement.
-- * 'responseStatus' - The response status code.
mkSendAnnouncementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendAnnouncementResponse
mkSendAnnouncementResponse pResponseStatus_ =
  SendAnnouncementResponse'
    { announcementARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the announcement.
--
-- /Note:/ Consider using 'announcementARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarsAnnouncementARN :: Lens.Lens' SendAnnouncementResponse (Lude.Maybe Lude.Text)
sarsAnnouncementARN = Lens.lens (announcementARN :: SendAnnouncementResponse -> Lude.Maybe Lude.Text) (\s a -> s {announcementARN = a} :: SendAnnouncementResponse)
{-# DEPRECATED sarsAnnouncementARN "Use generic-lens or generic-optics with 'announcementARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarsResponseStatus :: Lens.Lens' SendAnnouncementResponse Lude.Int
sarsResponseStatus = Lens.lens (responseStatus :: SendAnnouncementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendAnnouncementResponse)
{-# DEPRECATED sarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
