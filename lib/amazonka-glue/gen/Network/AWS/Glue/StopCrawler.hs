{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the specified crawler is running, stops the crawl.
module Network.AWS.Glue.StopCrawler
  ( -- * Creating a request
    StopCrawler (..),
    mkStopCrawler,

    -- ** Request lenses
    stoName,

    -- * Destructuring the response
    StopCrawlerResponse (..),
    mkStopCrawlerResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopCrawler' smart constructor.
newtype StopCrawler = StopCrawler' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopCrawler' with the minimum fields required to make a request.
--
-- * 'name' - Name of the crawler to stop.
mkStopCrawler ::
  -- | 'name'
  Lude.Text ->
  StopCrawler
mkStopCrawler pName_ = StopCrawler' {name = pName_}

-- | Name of the crawler to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stoName :: Lens.Lens' StopCrawler Lude.Text
stoName = Lens.lens (name :: StopCrawler -> Lude.Text) (\s a -> s {name = a} :: StopCrawler)
{-# DEPRECATED stoName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StopCrawler where
  type Rs StopCrawler = StopCrawlerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopCrawlerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopCrawler where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.StopCrawler" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopCrawler where
  toJSON StopCrawler' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StopCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery StopCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopCrawlerResponse' smart constructor.
newtype StopCrawlerResponse = StopCrawlerResponse'
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

-- | Creates a value of 'StopCrawlerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopCrawlerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopCrawlerResponse
mkStopCrawlerResponse pResponseStatus_ =
  StopCrawlerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopCrawlerResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopCrawlerResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
