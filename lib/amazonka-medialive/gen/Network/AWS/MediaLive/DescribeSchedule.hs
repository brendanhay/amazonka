{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a channel schedule
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.DescribeSchedule
  ( -- * Creating a request
    DescribeSchedule (..),
    mkDescribeSchedule,

    -- ** Request lenses
    dNextToken,
    dMaxResults,
    dChannelId,

    -- * Destructuring the response
    DescribeScheduleResponse (..),
    mkDescribeScheduleResponse,

    -- ** Response lenses
    dssrsNextToken,
    dssrsScheduleActions,
    dssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'mkDescribeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    channelId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSchedule' with the minimum fields required to make a request.
--
-- * 'channelId' - Id of the channel whose schedule is being updated.
-- * 'maxResults' - Undocumented field.
-- * 'nextToken' - Undocumented field.
mkDescribeSchedule ::
  -- | 'channelId'
  Lude.Text ->
  DescribeSchedule
mkDescribeSchedule pChannelId_ =
  DescribeSchedule'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      channelId = pChannelId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeSchedule (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeSchedule -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSchedule)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeSchedule (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeSchedule -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeSchedule)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Id of the channel whose schedule is being updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelId :: Lens.Lens' DescribeSchedule Lude.Text
dChannelId = Lens.lens (channelId :: DescribeSchedule -> Lude.Text) (\s a -> s {channelId = a} :: DescribeSchedule)
{-# DEPRECATED dChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Page.AWSPager DescribeSchedule where
  page rq rs
    | Page.stop (rs Lens.^. dssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dssrsScheduleActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. dssrsNextToken

instance Lude.AWSRequest DescribeSchedule where
  type Rs DescribeSchedule = DescribeScheduleResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "scheduleActions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeSchedule where
  toPath DescribeSchedule' {..} =
    Lude.mconcat
      ["/prod/channels/", Lude.toBS channelId, "/schedule"]

instance Lude.ToQuery DescribeSchedule where
  toQuery DescribeSchedule' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Placeholder documentation for DescribeScheduleResponse
--
-- /See:/ 'mkDescribeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    scheduleActions ::
      Lude.Maybe [ScheduleAction],
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

-- | Creates a value of 'DescribeScheduleResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The next token; for use in pagination.
-- * 'responseStatus' - The response status code.
-- * 'scheduleActions' - The list of actions in the schedule.
mkDescribeScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScheduleResponse
mkDescribeScheduleResponse pResponseStatus_ =
  DescribeScheduleResponse'
    { nextToken = Lude.Nothing,
      scheduleActions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The next token; for use in pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsNextToken :: Lens.Lens' DescribeScheduleResponse (Lude.Maybe Lude.Text)
dssrsNextToken = Lens.lens (nextToken :: DescribeScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduleResponse)
{-# DEPRECATED dssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of actions in the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsScheduleActions :: Lens.Lens' DescribeScheduleResponse (Lude.Maybe [ScheduleAction])
dssrsScheduleActions = Lens.lens (scheduleActions :: DescribeScheduleResponse -> Lude.Maybe [ScheduleAction]) (\s a -> s {scheduleActions = a} :: DescribeScheduleResponse)
{-# DEPRECATED dssrsScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeScheduleResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduleResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
