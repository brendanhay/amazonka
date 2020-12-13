{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dChannelId,
    dNextToken,
    dMaxResults,

    -- * Destructuring the response
    DescribeScheduleResponse (..),
    mkDescribeScheduleResponse,

    -- ** Response lenses
    dsfrsNextToken,
    dsfrsScheduleActions,
    dsfrsResponseStatus,
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
  { -- | Id of the channel whose schedule is being updated.
    channelId :: Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSchedule' with the minimum fields required to make a request.
--
-- * 'channelId' - Id of the channel whose schedule is being updated.
-- * 'nextToken' -
-- * 'maxResults' -
mkDescribeSchedule ::
  -- | 'channelId'
  Lude.Text ->
  DescribeSchedule
mkDescribeSchedule pChannelId_ =
  DescribeSchedule'
    { channelId = pChannelId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Id of the channel whose schedule is being updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelId :: Lens.Lens' DescribeSchedule Lude.Text
dChannelId = Lens.lens (channelId :: DescribeSchedule -> Lude.Text) (\s a -> s {channelId = a} :: DescribeSchedule)
{-# DEPRECATED dChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

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

instance Page.AWSPager DescribeSchedule where
  page rq rs
    | Page.stop (rs Lens.^. dsfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsfrsScheduleActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. dsfrsNextToken

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
  { -- | The next token; for use in pagination.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of actions in the schedule.
    scheduleActions :: Lude.Maybe [ScheduleAction],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduleResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The next token; for use in pagination.
-- * 'scheduleActions' - The list of actions in the schedule.
-- * 'responseStatus' - The response status code.
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
dsfrsNextToken :: Lens.Lens' DescribeScheduleResponse (Lude.Maybe Lude.Text)
dsfrsNextToken = Lens.lens (nextToken :: DescribeScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduleResponse)
{-# DEPRECATED dsfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of actions in the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsScheduleActions :: Lens.Lens' DescribeScheduleResponse (Lude.Maybe [ScheduleAction])
dsfrsScheduleActions = Lens.lens (scheduleActions :: DescribeScheduleResponse -> Lude.Maybe [ScheduleAction]) (\s a -> s {scheduleActions = a} :: DescribeScheduleResponse)
{-# DEPRECATED dsfrsScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsResponseStatus :: Lens.Lens' DescribeScheduleResponse Lude.Int
dsfrsResponseStatus = Lens.lens (responseStatus :: DescribeScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduleResponse)
{-# DEPRECATED dsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
