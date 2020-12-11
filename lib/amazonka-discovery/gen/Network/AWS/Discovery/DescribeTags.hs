{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items that have tags as specified by the key-value pairs, name and value, passed to the optional parameter @filters@ .
--
-- There are three valid tag filter names:
--
--     * tagKey
--
--
--     * tagValue
--
--
--     * configurationId
--
--
-- Also, all configuration items associated with your user account that have tags can be listed if you call @DescribeTags@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtFilters,
    dtNextToken,
    dtMaxResults,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsNextToken,
    dtrsTags,
    dtrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { filters ::
      Lude.Maybe [TagFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'filters' - You can filter the list using a /key/ -/value/ format. You can separate these items by using logical operators. Allowed filters include @tagKey@ , @tagValue@ , and @configurationId@ .
-- * 'maxResults' - The total number of items to return in a single page of output. The maximum value is 100.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
mkDescribeTags ::
  DescribeTags
mkDescribeTags =
  DescribeTags'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can filter the list using a /key/ -/value/ format. You can separate these items by using logical operators. Allowed filters include @tagKey@ , @tagValue@ , and @configurationId@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtFilters :: Lens.Lens' DescribeTags (Lude.Maybe [TagFilter])
dtFilters = Lens.lens (filters :: DescribeTags -> Lude.Maybe [TagFilter]) (\s a -> s {filters = a} :: DescribeTags)
{-# DEPRECATED dtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtNextToken :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Text)
dtNextToken = Lens.lens (nextToken :: DescribeTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTags)
{-# DEPRECATED dtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of items to return in a single page of output. The maximum value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtMaxResults :: Lens.Lens' DescribeTags (Lude.Maybe Lude.Int)
dtMaxResults = Lens.lens (maxResults :: DescribeTags -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeTags)
{-# DEPRECATED dtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTags where
  page rq rs
    | Page.stop (rs Lens.^. dtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtNextToken Lens..~ rs Lens.^. dtrsNextToken

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSPoseidonService_V2015_11_01.DescribeTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("filters" Lude..=) Lude.<$> filters,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [ConfigurationTag],
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

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The call returns a token. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Depending on the input, this is a list of configuration items tagged with a specific tag, or a list of tags for a specific configuration item.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The call returns a token. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsNextToken :: Lens.Lens' DescribeTagsResponse (Lude.Maybe Lude.Text)
dtrsNextToken = Lens.lens (nextToken :: DescribeTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Depending on the input, this is a list of configuration items tagged with a specific tag, or a list of tags for a specific configuration item.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTags :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [ConfigurationTag])
dtrsTags = Lens.lens (tags :: DescribeTagsResponse -> Lude.Maybe [ConfigurationTag]) (\s a -> s {tags = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
