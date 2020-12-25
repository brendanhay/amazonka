{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    SendAnnouncement (..),
    mkSendAnnouncement,

    -- ** Request lenses
    saRoomFilters,
    saContent,
    saClientRequestToken,
    saTimeToLiveInSeconds,

    -- * Destructuring the response
    SendAnnouncementResponse (..),
    mkSendAnnouncementResponse,

    -- ** Response lenses
    sarrsAnnouncementArn,
    sarrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSendAnnouncement' smart constructor.
data SendAnnouncement = SendAnnouncement'
  { -- | The filters to use to send an announcement to a specified list of rooms. The supported filter keys are RoomName, ProfileName, RoomArn, and ProfileArn. To send to all rooms, specify an empty RoomFilters list.
    roomFilters :: [Types.Filter],
    -- | The announcement content. This can contain only one of the three possible announcement types (text, SSML or audio).
    content :: Types.Content,
    -- | The unique, user-specified identifier for the request that ensures idempotency.
    clientRequestToken :: Types.ClientRequestToken,
    -- | The time to live for an announcement. Default is 300. If delivery doesn't occur within this time, the announcement is not delivered.
    timeToLiveInSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendAnnouncement' value with any optional fields omitted.
mkSendAnnouncement ::
  -- | 'content'
  Types.Content ->
  -- | 'clientRequestToken'
  Types.ClientRequestToken ->
  SendAnnouncement
mkSendAnnouncement content clientRequestToken =
  SendAnnouncement'
    { roomFilters = Core.mempty,
      content,
      clientRequestToken,
      timeToLiveInSeconds = Core.Nothing
    }

-- | The filters to use to send an announcement to a specified list of rooms. The supported filter keys are RoomName, ProfileName, RoomArn, and ProfileArn. To send to all rooms, specify an empty RoomFilters list.
--
-- /Note:/ Consider using 'roomFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saRoomFilters :: Lens.Lens' SendAnnouncement [Types.Filter]
saRoomFilters = Lens.field @"roomFilters"
{-# DEPRECATED saRoomFilters "Use generic-lens or generic-optics with 'roomFilters' instead." #-}

-- | The announcement content. This can contain only one of the three possible announcement types (text, SSML or audio).
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saContent :: Lens.Lens' SendAnnouncement Types.Content
saContent = Lens.field @"content"
{-# DEPRECATED saContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saClientRequestToken :: Lens.Lens' SendAnnouncement Types.ClientRequestToken
saClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED saClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The time to live for an announcement. Default is 300. If delivery doesn't occur within this time, the announcement is not delivered.
--
-- /Note:/ Consider using 'timeToLiveInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saTimeToLiveInSeconds :: Lens.Lens' SendAnnouncement (Core.Maybe Core.Natural)
saTimeToLiveInSeconds = Lens.field @"timeToLiveInSeconds"
{-# DEPRECATED saTimeToLiveInSeconds "Use generic-lens or generic-optics with 'timeToLiveInSeconds' instead." #-}

instance Core.FromJSON SendAnnouncement where
  toJSON SendAnnouncement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RoomFilters" Core..= roomFilters),
            Core.Just ("Content" Core..= content),
            Core.Just ("ClientRequestToken" Core..= clientRequestToken),
            ("TimeToLiveInSeconds" Core..=) Core.<$> timeToLiveInSeconds
          ]
      )

instance Core.AWSRequest SendAnnouncement where
  type Rs SendAnnouncement = SendAnnouncementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.SendAnnouncement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SendAnnouncementResponse'
            Core.<$> (x Core..:? "AnnouncementArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSendAnnouncementResponse' smart constructor.
data SendAnnouncementResponse = SendAnnouncementResponse'
  { -- | The identifier of the announcement.
    announcementArn :: Core.Maybe Types.Arn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendAnnouncementResponse' value with any optional fields omitted.
mkSendAnnouncementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendAnnouncementResponse
mkSendAnnouncementResponse responseStatus =
  SendAnnouncementResponse'
    { announcementArn = Core.Nothing,
      responseStatus
    }

-- | The identifier of the announcement.
--
-- /Note:/ Consider using 'announcementArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsAnnouncementArn :: Lens.Lens' SendAnnouncementResponse (Core.Maybe Types.Arn)
sarrsAnnouncementArn = Lens.field @"announcementArn"
{-# DEPRECATED sarrsAnnouncementArn "Use generic-lens or generic-optics with 'announcementArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrsResponseStatus :: Lens.Lens' SendAnnouncementResponse Core.Int
sarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
