{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of crawler names. After calling the @ListCrawlers@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetCrawlers
  ( -- * Creating a request
    BatchGetCrawlers (..),
    mkBatchGetCrawlers,

    -- ** Request lenses
    bgcCrawlerNames,

    -- * Destructuring the response
    BatchGetCrawlersResponse (..),
    mkBatchGetCrawlersResponse,

    -- ** Response lenses
    bgcrsCrawlersNotFound,
    bgcrsCrawlers,
    bgcrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetCrawlers' smart constructor.
newtype BatchGetCrawlers = BatchGetCrawlers'
  { -- | A list of crawler names, which might be the names returned from the @ListCrawlers@ operation.
    crawlerNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetCrawlers' with the minimum fields required to make a request.
--
-- * 'crawlerNames' - A list of crawler names, which might be the names returned from the @ListCrawlers@ operation.
mkBatchGetCrawlers ::
  BatchGetCrawlers
mkBatchGetCrawlers = BatchGetCrawlers' {crawlerNames = Lude.mempty}

-- | A list of crawler names, which might be the names returned from the @ListCrawlers@ operation.
--
-- /Note:/ Consider using 'crawlerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcCrawlerNames :: Lens.Lens' BatchGetCrawlers [Lude.Text]
bgcCrawlerNames = Lens.lens (crawlerNames :: BatchGetCrawlers -> [Lude.Text]) (\s a -> s {crawlerNames = a} :: BatchGetCrawlers)
{-# DEPRECATED bgcCrawlerNames "Use generic-lens or generic-optics with 'crawlerNames' instead." #-}

instance Lude.AWSRequest BatchGetCrawlers where
  type Rs BatchGetCrawlers = BatchGetCrawlersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetCrawlersResponse'
            Lude.<$> (x Lude..?> "CrawlersNotFound" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Crawlers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetCrawlers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchGetCrawlers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetCrawlers where
  toJSON BatchGetCrawlers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CrawlerNames" Lude..= crawlerNames)])

instance Lude.ToPath BatchGetCrawlers where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetCrawlers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetCrawlersResponse' smart constructor.
data BatchGetCrawlersResponse = BatchGetCrawlersResponse'
  { -- | A list of names of crawlers that were not found.
    crawlersNotFound :: Lude.Maybe [Lude.Text],
    -- | A list of crawler definitions.
    crawlers :: Lude.Maybe [Crawler],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetCrawlersResponse' with the minimum fields required to make a request.
--
-- * 'crawlersNotFound' - A list of names of crawlers that were not found.
-- * 'crawlers' - A list of crawler definitions.
-- * 'responseStatus' - The response status code.
mkBatchGetCrawlersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetCrawlersResponse
mkBatchGetCrawlersResponse pResponseStatus_ =
  BatchGetCrawlersResponse'
    { crawlersNotFound = Lude.Nothing,
      crawlers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of names of crawlers that were not found.
--
-- /Note:/ Consider using 'crawlersNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrsCrawlersNotFound :: Lens.Lens' BatchGetCrawlersResponse (Lude.Maybe [Lude.Text])
bgcrsCrawlersNotFound = Lens.lens (crawlersNotFound :: BatchGetCrawlersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {crawlersNotFound = a} :: BatchGetCrawlersResponse)
{-# DEPRECATED bgcrsCrawlersNotFound "Use generic-lens or generic-optics with 'crawlersNotFound' instead." #-}

-- | A list of crawler definitions.
--
-- /Note:/ Consider using 'crawlers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrsCrawlers :: Lens.Lens' BatchGetCrawlersResponse (Lude.Maybe [Crawler])
bgcrsCrawlers = Lens.lens (crawlers :: BatchGetCrawlersResponse -> Lude.Maybe [Crawler]) (\s a -> s {crawlers = a} :: BatchGetCrawlersResponse)
{-# DEPRECATED bgcrsCrawlers "Use generic-lens or generic-optics with 'crawlers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrsResponseStatus :: Lens.Lens' BatchGetCrawlersResponse Lude.Int
bgcrsResponseStatus = Lens.lens (responseStatus :: BatchGetCrawlersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetCrawlersResponse)
{-# DEPRECATED bgcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
