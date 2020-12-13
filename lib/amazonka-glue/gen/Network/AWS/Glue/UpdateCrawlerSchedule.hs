{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateCrawlerSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schedule of a crawler using a @cron@ expression.
module Network.AWS.Glue.UpdateCrawlerSchedule
  ( -- * Creating a request
    UpdateCrawlerSchedule (..),
    mkUpdateCrawlerSchedule,

    -- ** Request lenses
    ucsSchedule,
    ucsCrawlerName,

    -- * Destructuring the response
    UpdateCrawlerScheduleResponse (..),
    mkUpdateCrawlerScheduleResponse,

    -- ** Response lenses
    ucsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCrawlerSchedule' smart constructor.
data UpdateCrawlerSchedule = UpdateCrawlerSchedule'
  { -- | The updated @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Lude.Maybe Lude.Text,
    -- | The name of the crawler whose schedule to update.
    crawlerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCrawlerSchedule' with the minimum fields required to make a request.
--
-- * 'schedule' - The updated @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'crawlerName' - The name of the crawler whose schedule to update.
mkUpdateCrawlerSchedule ::
  -- | 'crawlerName'
  Lude.Text ->
  UpdateCrawlerSchedule
mkUpdateCrawlerSchedule pCrawlerName_ =
  UpdateCrawlerSchedule'
    { schedule = Lude.Nothing,
      crawlerName = pCrawlerName_
    }

-- | The updated @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsSchedule :: Lens.Lens' UpdateCrawlerSchedule (Lude.Maybe Lude.Text)
ucsSchedule = Lens.lens (schedule :: UpdateCrawlerSchedule -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: UpdateCrawlerSchedule)
{-# DEPRECATED ucsSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The name of the crawler whose schedule to update.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsCrawlerName :: Lens.Lens' UpdateCrawlerSchedule Lude.Text
ucsCrawlerName = Lens.lens (crawlerName :: UpdateCrawlerSchedule -> Lude.Text) (\s a -> s {crawlerName = a} :: UpdateCrawlerSchedule)
{-# DEPRECATED ucsCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Lude.AWSRequest UpdateCrawlerSchedule where
  type Rs UpdateCrawlerSchedule = UpdateCrawlerScheduleResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateCrawlerScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCrawlerSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateCrawlerSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCrawlerSchedule where
  toJSON UpdateCrawlerSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Schedule" Lude..=) Lude.<$> schedule,
            Lude.Just ("CrawlerName" Lude..= crawlerName)
          ]
      )

instance Lude.ToPath UpdateCrawlerSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCrawlerSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCrawlerScheduleResponse' smart constructor.
newtype UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCrawlerScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateCrawlerScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCrawlerScheduleResponse
mkUpdateCrawlerScheduleResponse pResponseStatus_ =
  UpdateCrawlerScheduleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrsResponseStatus :: Lens.Lens' UpdateCrawlerScheduleResponse Lude.Int
ucsrsResponseStatus = Lens.lens (responseStatus :: UpdateCrawlerScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCrawlerScheduleResponse)
{-# DEPRECATED ucsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
