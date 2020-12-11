{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete all schedule actions on a channel.
module Network.AWS.MediaLive.DeleteSchedule
  ( -- * Creating a request
    DeleteSchedule (..),
    mkDeleteSchedule,

    -- ** Request lenses
    dsChannelId,

    -- * Destructuring the response
    DeleteScheduleResponse (..),
    mkDeleteScheduleResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DeleteScheduleRequest
--
-- /See:/ 'mkDeleteSchedule' smart constructor.
newtype DeleteSchedule = DeleteSchedule' {channelId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSchedule' with the minimum fields required to make a request.
--
-- * 'channelId' - Id of the channel whose schedule is being deleted.
mkDeleteSchedule ::
  -- | 'channelId'
  Lude.Text ->
  DeleteSchedule
mkDeleteSchedule pChannelId_ =
  DeleteSchedule' {channelId = pChannelId_}

-- | Id of the channel whose schedule is being deleted.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsChannelId :: Lens.Lens' DeleteSchedule Lude.Text
dsChannelId = Lens.lens (channelId :: DeleteSchedule -> Lude.Text) (\s a -> s {channelId = a} :: DeleteSchedule)
{-# DEPRECATED dsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.AWSRequest DeleteSchedule where
  type Rs DeleteSchedule = DeleteScheduleResponse
  request = Req.delete mediaLiveService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteScheduleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSchedule where
  toPath DeleteSchedule' {..} =
    Lude.mconcat
      ["/prod/channels/", Lude.toBS channelId, "/schedule"]

instance Lude.ToQuery DeleteSchedule where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DeleteScheduleResponse
--
-- /See:/ 'mkDeleteScheduleResponse' smart constructor.
newtype DeleteScheduleResponse = DeleteScheduleResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteScheduleResponse
mkDeleteScheduleResponse pResponseStatus_ =
  DeleteScheduleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteScheduleResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteScheduleResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
