{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchUpdateSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a channel schedule
module Network.AWS.MediaLive.BatchUpdateSchedule
  ( -- * Creating a request
    BatchUpdateSchedule (..),
    mkBatchUpdateSchedule,

    -- ** Request lenses
    busCreates,
    busChannelId,
    busDeletes,

    -- * Destructuring the response
    BatchUpdateScheduleResponse (..),
    mkBatchUpdateScheduleResponse,

    -- ** Response lenses
    busrsCreates,
    busrsDeletes,
    busrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | List of actions to create and list of actions to delete.
--
-- /See:/ 'mkBatchUpdateSchedule' smart constructor.
data BatchUpdateSchedule = BatchUpdateSchedule'
  { -- | Schedule actions to create in the schedule.
    creates :: Lude.Maybe BatchScheduleActionCreateRequest,
    -- | Id of the channel whose schedule is being updated.
    channelId :: Lude.Text,
    -- | Schedule actions to delete from the schedule.
    deletes :: Lude.Maybe BatchScheduleActionDeleteRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdateSchedule' with the minimum fields required to make a request.
--
-- * 'creates' - Schedule actions to create in the schedule.
-- * 'channelId' - Id of the channel whose schedule is being updated.
-- * 'deletes' - Schedule actions to delete from the schedule.
mkBatchUpdateSchedule ::
  -- | 'channelId'
  Lude.Text ->
  BatchUpdateSchedule
mkBatchUpdateSchedule pChannelId_ =
  BatchUpdateSchedule'
    { creates = Lude.Nothing,
      channelId = pChannelId_,
      deletes = Lude.Nothing
    }

-- | Schedule actions to create in the schedule.
--
-- /Note:/ Consider using 'creates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busCreates :: Lens.Lens' BatchUpdateSchedule (Lude.Maybe BatchScheduleActionCreateRequest)
busCreates = Lens.lens (creates :: BatchUpdateSchedule -> Lude.Maybe BatchScheduleActionCreateRequest) (\s a -> s {creates = a} :: BatchUpdateSchedule)
{-# DEPRECATED busCreates "Use generic-lens or generic-optics with 'creates' instead." #-}

-- | Id of the channel whose schedule is being updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busChannelId :: Lens.Lens' BatchUpdateSchedule Lude.Text
busChannelId = Lens.lens (channelId :: BatchUpdateSchedule -> Lude.Text) (\s a -> s {channelId = a} :: BatchUpdateSchedule)
{-# DEPRECATED busChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Schedule actions to delete from the schedule.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busDeletes :: Lens.Lens' BatchUpdateSchedule (Lude.Maybe BatchScheduleActionDeleteRequest)
busDeletes = Lens.lens (deletes :: BatchUpdateSchedule -> Lude.Maybe BatchScheduleActionDeleteRequest) (\s a -> s {deletes = a} :: BatchUpdateSchedule)
{-# DEPRECATED busDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

instance Lude.AWSRequest BatchUpdateSchedule where
  type Rs BatchUpdateSchedule = BatchUpdateScheduleResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchUpdateScheduleResponse'
            Lude.<$> (x Lude..?> "creates")
            Lude.<*> (x Lude..?> "deletes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchUpdateSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchUpdateSchedule where
  toJSON BatchUpdateSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("creates" Lude..=) Lude.<$> creates,
            ("deletes" Lude..=) Lude.<$> deletes
          ]
      )

instance Lude.ToPath BatchUpdateSchedule where
  toPath BatchUpdateSchedule' {..} =
    Lude.mconcat
      ["/prod/channels/", Lude.toBS channelId, "/schedule"]

instance Lude.ToQuery BatchUpdateSchedule where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for BatchUpdateScheduleResponse
--
-- /See:/ 'mkBatchUpdateScheduleResponse' smart constructor.
data BatchUpdateScheduleResponse = BatchUpdateScheduleResponse'
  { -- | Schedule actions created in the schedule.
    creates :: Lude.Maybe BatchScheduleActionCreateResult,
    -- | Schedule actions deleted from the schedule.
    deletes :: Lude.Maybe BatchScheduleActionDeleteResult,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchUpdateScheduleResponse' with the minimum fields required to make a request.
--
-- * 'creates' - Schedule actions created in the schedule.
-- * 'deletes' - Schedule actions deleted from the schedule.
-- * 'responseStatus' - The response status code.
mkBatchUpdateScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchUpdateScheduleResponse
mkBatchUpdateScheduleResponse pResponseStatus_ =
  BatchUpdateScheduleResponse'
    { creates = Lude.Nothing,
      deletes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Schedule actions created in the schedule.
--
-- /Note:/ Consider using 'creates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busrsCreates :: Lens.Lens' BatchUpdateScheduleResponse (Lude.Maybe BatchScheduleActionCreateResult)
busrsCreates = Lens.lens (creates :: BatchUpdateScheduleResponse -> Lude.Maybe BatchScheduleActionCreateResult) (\s a -> s {creates = a} :: BatchUpdateScheduleResponse)
{-# DEPRECATED busrsCreates "Use generic-lens or generic-optics with 'creates' instead." #-}

-- | Schedule actions deleted from the schedule.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busrsDeletes :: Lens.Lens' BatchUpdateScheduleResponse (Lude.Maybe BatchScheduleActionDeleteResult)
busrsDeletes = Lens.lens (deletes :: BatchUpdateScheduleResponse -> Lude.Maybe BatchScheduleActionDeleteResult) (\s a -> s {deletes = a} :: BatchUpdateScheduleResponse)
{-# DEPRECATED busrsDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busrsResponseStatus :: Lens.Lens' BatchUpdateScheduleResponse Lude.Int
busrsResponseStatus = Lens.lens (responseStatus :: BatchUpdateScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchUpdateScheduleResponse)
{-# DEPRECATED busrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
