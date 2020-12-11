{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopCrawlerSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the schedule state of the specified crawler to @NOT_SCHEDULED@ , but does not stop the crawler if it is already running.
module Network.AWS.Glue.StopCrawlerSchedule
  ( -- * Creating a request
    StopCrawlerSchedule (..),
    mkStopCrawlerSchedule,

    -- ** Request lenses
    sCrawlerName,

    -- * Destructuring the response
    StopCrawlerScheduleResponse (..),
    mkStopCrawlerScheduleResponse,

    -- ** Response lenses
    storsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopCrawlerSchedule' smart constructor.
newtype StopCrawlerSchedule = StopCrawlerSchedule'
  { crawlerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopCrawlerSchedule' with the minimum fields required to make a request.
--
-- * 'crawlerName' - Name of the crawler whose schedule state to set.
mkStopCrawlerSchedule ::
  -- | 'crawlerName'
  Lude.Text ->
  StopCrawlerSchedule
mkStopCrawlerSchedule pCrawlerName_ =
  StopCrawlerSchedule' {crawlerName = pCrawlerName_}

-- | Name of the crawler whose schedule state to set.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCrawlerName :: Lens.Lens' StopCrawlerSchedule Lude.Text
sCrawlerName = Lens.lens (crawlerName :: StopCrawlerSchedule -> Lude.Text) (\s a -> s {crawlerName = a} :: StopCrawlerSchedule)
{-# DEPRECATED sCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Lude.AWSRequest StopCrawlerSchedule where
  type Rs StopCrawlerSchedule = StopCrawlerScheduleResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopCrawlerScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopCrawlerSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StopCrawlerSchedule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopCrawlerSchedule where
  toJSON StopCrawlerSchedule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CrawlerName" Lude..= crawlerName)])

instance Lude.ToPath StopCrawlerSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery StopCrawlerSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopCrawlerScheduleResponse' smart constructor.
newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
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

-- | Creates a value of 'StopCrawlerScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopCrawlerScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopCrawlerScheduleResponse
mkStopCrawlerScheduleResponse pResponseStatus_ =
  StopCrawlerScheduleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
storsResponseStatus :: Lens.Lens' StopCrawlerScheduleResponse Lude.Int
storsResponseStatus = Lens.lens (responseStatus :: StopCrawlerScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopCrawlerScheduleResponse)
{-# DEPRECATED storsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
