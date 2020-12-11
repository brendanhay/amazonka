{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a specified crawler.
module Network.AWS.Glue.GetCrawler
  ( -- * Creating a request
    GetCrawler (..),
    mkGetCrawler,

    -- ** Request lenses
    gccName,

    -- * Destructuring the response
    GetCrawlerResponse (..),
    mkGetCrawlerResponse,

    -- ** Response lenses
    getcrawlerersCrawler,
    getcrawlerersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCrawler' smart constructor.
newtype GetCrawler = GetCrawler' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCrawler' with the minimum fields required to make a request.
--
-- * 'name' - The name of the crawler to retrieve metadata for.
mkGetCrawler ::
  -- | 'name'
  Lude.Text ->
  GetCrawler
mkGetCrawler pName_ = GetCrawler' {name = pName_}

-- | The name of the crawler to retrieve metadata for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccName :: Lens.Lens' GetCrawler Lude.Text
gccName = Lens.lens (name :: GetCrawler -> Lude.Text) (\s a -> s {name = a} :: GetCrawler)
{-# DEPRECATED gccName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetCrawler where
  type Rs GetCrawler = GetCrawlerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCrawlerResponse'
            Lude.<$> (x Lude..?> "Crawler") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCrawler where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetCrawler" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCrawler where
  toJSON GetCrawler' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'
  { crawler ::
      Lude.Maybe Crawler,
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

-- | Creates a value of 'GetCrawlerResponse' with the minimum fields required to make a request.
--
-- * 'crawler' - The metadata for the specified crawler.
-- * 'responseStatus' - The response status code.
mkGetCrawlerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCrawlerResponse
mkGetCrawlerResponse pResponseStatus_ =
  GetCrawlerResponse'
    { crawler = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The metadata for the specified crawler.
--
-- /Note:/ Consider using 'crawler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getcrawlerersCrawler :: Lens.Lens' GetCrawlerResponse (Lude.Maybe Crawler)
getcrawlerersCrawler = Lens.lens (crawler :: GetCrawlerResponse -> Lude.Maybe Crawler) (\s a -> s {crawler = a} :: GetCrawlerResponse)
{-# DEPRECATED getcrawlerersCrawler "Use generic-lens or generic-optics with 'crawler' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getcrawlerersResponseStatus :: Lens.Lens' GetCrawlerResponse Lude.Int
getcrawlerersResponseStatus = Lens.lens (responseStatus :: GetCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCrawlerResponse)
{-# DEPRECATED getcrawlerersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
