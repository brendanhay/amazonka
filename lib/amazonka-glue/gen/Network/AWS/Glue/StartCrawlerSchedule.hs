{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartCrawlerSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the schedule state of the specified crawler to @SCHEDULED@ , unless the crawler is already running or the schedule state is already @SCHEDULED@ .
module Network.AWS.Glue.StartCrawlerSchedule
  ( -- * Creating a request
    StartCrawlerSchedule (..),
    mkStartCrawlerSchedule,

    -- ** Request lenses
    scsCrawlerName,

    -- * Destructuring the response
    StartCrawlerScheduleResponse (..),
    mkStartCrawlerScheduleResponse,

    -- ** Response lenses
    scsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartCrawlerSchedule' smart constructor.
newtype StartCrawlerSchedule = StartCrawlerSchedule'
  { -- | Name of the crawler to schedule.
    crawlerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartCrawlerSchedule' with the minimum fields required to make a request.
--
-- * 'crawlerName' - Name of the crawler to schedule.
mkStartCrawlerSchedule ::
  -- | 'crawlerName'
  Lude.Text ->
  StartCrawlerSchedule
mkStartCrawlerSchedule pCrawlerName_ =
  StartCrawlerSchedule' {crawlerName = pCrawlerName_}

-- | Name of the crawler to schedule.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsCrawlerName :: Lens.Lens' StartCrawlerSchedule Lude.Text
scsCrawlerName = Lens.lens (crawlerName :: StartCrawlerSchedule -> Lude.Text) (\s a -> s {crawlerName = a} :: StartCrawlerSchedule)
{-# DEPRECATED scsCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Lude.AWSRequest StartCrawlerSchedule where
  type Rs StartCrawlerSchedule = StartCrawlerScheduleResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartCrawlerScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartCrawlerSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartCrawlerSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartCrawlerSchedule where
  toJSON StartCrawlerSchedule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CrawlerName" Lude..= crawlerName)])

instance Lude.ToPath StartCrawlerSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery StartCrawlerSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartCrawlerScheduleResponse' smart constructor.
newtype StartCrawlerScheduleResponse = StartCrawlerScheduleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartCrawlerScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartCrawlerScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartCrawlerScheduleResponse
mkStartCrawlerScheduleResponse pResponseStatus_ =
  StartCrawlerScheduleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsrsResponseStatus :: Lens.Lens' StartCrawlerScheduleResponse Lude.Int
scsrsResponseStatus = Lens.lens (responseStatus :: StartCrawlerScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartCrawlerScheduleResponse)
{-# DEPRECATED scsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
