{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all crawler resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListCrawlers
  ( -- * Creating a request
    ListCrawlers (..),
    mkListCrawlers,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,
    lcTags,

    -- * Destructuring the response
    ListCrawlersResponse (..),
    mkListCrawlersResponse,

    -- ** Response lenses
    lcrsNextToken,
    lcrsCrawlerNames,
    lcrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCrawlers' smart constructor.
data ListCrawlers = ListCrawlers'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | Specifies to return only these tagged resources.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCrawlers' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if this is a continuation request.
-- * 'maxResults' - The maximum size of a list to return.
-- * 'tags' - Specifies to return only these tagged resources.
mkListCrawlers ::
  ListCrawlers
mkListCrawlers =
  ListCrawlers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCrawlers (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListCrawlers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCrawlers)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListCrawlers (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListCrawlers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCrawlers)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcTags :: Lens.Lens' ListCrawlers (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
lcTags = Lens.lens (tags :: ListCrawlers -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListCrawlers)
{-# DEPRECATED lcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest ListCrawlers where
  type Rs ListCrawlers = ListCrawlersResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCrawlersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "CrawlerNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCrawlers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.ListCrawlers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCrawlers where
  toJSON ListCrawlers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath ListCrawlers where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCrawlers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCrawlersResponse' smart constructor.
data ListCrawlersResponse = ListCrawlersResponse'
  { -- | A continuation token, if the returned list does not contain the last metric available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of all crawlers in the account, or the crawlers with the specified tags.
    crawlerNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCrawlersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if the returned list does not contain the last metric available.
-- * 'crawlerNames' - The names of all crawlers in the account, or the crawlers with the specified tags.
-- * 'responseStatus' - The response status code.
mkListCrawlersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCrawlersResponse
mkListCrawlersResponse pResponseStatus_ =
  ListCrawlersResponse'
    { nextToken = Lude.Nothing,
      crawlerNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListCrawlersResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListCrawlersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCrawlersResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of all crawlers in the account, or the crawlers with the specified tags.
--
-- /Note:/ Consider using 'crawlerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsCrawlerNames :: Lens.Lens' ListCrawlersResponse (Lude.Maybe [Lude.Text])
lcrsCrawlerNames = Lens.lens (crawlerNames :: ListCrawlersResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {crawlerNames = a} :: ListCrawlersResponse)
{-# DEPRECATED lcrsCrawlerNames "Use generic-lens or generic-optics with 'crawlerNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListCrawlersResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListCrawlersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCrawlersResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
