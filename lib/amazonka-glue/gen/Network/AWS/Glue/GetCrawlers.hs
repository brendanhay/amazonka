{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all crawlers defined in the customer account.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlers
  ( -- * Creating a request
    GetCrawlers (..),
    mkGetCrawlers,

    -- ** Request lenses
    gNextToken,
    gMaxResults,

    -- * Destructuring the response
    GetCrawlersResponse (..),
    mkGetCrawlersResponse,

    -- ** Response lenses
    grsNextToken,
    grsCrawlers,
    grsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCrawlers' smart constructor.
data GetCrawlers = GetCrawlers'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCrawlers' with the minimum fields required to make a request.
--
-- * 'maxResults' - The number of crawlers to return on each call.
-- * 'nextToken' - A continuation token, if this is a continuation request.
mkGetCrawlers ::
  GetCrawlers
mkGetCrawlers =
  GetCrawlers' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gNextToken :: Lens.Lens' GetCrawlers (Lude.Maybe Lude.Text)
gNextToken = Lens.lens (nextToken :: GetCrawlers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCrawlers)
{-# DEPRECATED gNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of crawlers to return on each call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gMaxResults :: Lens.Lens' GetCrawlers (Lude.Maybe Lude.Natural)
gMaxResults = Lens.lens (maxResults :: GetCrawlers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetCrawlers)
{-# DEPRECATED gMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetCrawlers where
  page rq rs
    | Page.stop (rs Lens.^. grsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grsCrawlers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gNextToken Lens..~ rs Lens.^. grsNextToken

instance Lude.AWSRequest GetCrawlers where
  type Rs GetCrawlers = GetCrawlersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCrawlersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Crawlers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCrawlers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetCrawlers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCrawlers where
  toJSON GetCrawlers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetCrawlers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCrawlers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCrawlersResponse' smart constructor.
data GetCrawlersResponse = GetCrawlersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    crawlers :: Lude.Maybe [Crawler],
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

-- | Creates a value of 'GetCrawlersResponse' with the minimum fields required to make a request.
--
-- * 'crawlers' - A list of crawler metadata.
-- * 'nextToken' - A continuation token, if the returned list has not reached the end of those defined in this customer account.
-- * 'responseStatus' - The response status code.
mkGetCrawlersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCrawlersResponse
mkGetCrawlersResponse pResponseStatus_ =
  GetCrawlersResponse'
    { nextToken = Lude.Nothing,
      crawlers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if the returned list has not reached the end of those defined in this customer account.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextToken :: Lens.Lens' GetCrawlersResponse (Lude.Maybe Lude.Text)
grsNextToken = Lens.lens (nextToken :: GetCrawlersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCrawlersResponse)
{-# DEPRECATED grsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of crawler metadata.
--
-- /Note:/ Consider using 'crawlers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCrawlers :: Lens.Lens' GetCrawlersResponse (Lude.Maybe [Crawler])
grsCrawlers = Lens.lens (crawlers :: GetCrawlersResponse -> Lude.Maybe [Crawler]) (\s a -> s {crawlers = a} :: GetCrawlersResponse)
{-# DEPRECATED grsCrawlers "Use generic-lens or generic-optics with 'crawlers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCrawlersResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetCrawlersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCrawlersResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
