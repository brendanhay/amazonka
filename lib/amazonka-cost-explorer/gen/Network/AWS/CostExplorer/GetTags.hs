{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for available tag keys and tag values for a specified period. You can search the tag values for an arbitrary string.
module Network.AWS.CostExplorer.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtNextPageToken,
    gtTimePeriod,
    gtSearchString,
    gtTagKey,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrsNextPageToken,
    gtrsReturnSize,
    gtrsTotalSize,
    gtrsTags,
    gtrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTags' smart constructor.
data GetTags = GetTags'
  { -- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
    timePeriod :: DateInterval,
    -- | The value that you want to search for.
    searchString :: Lude.Maybe Lude.Text,
    -- | The key of the tag that you want to return values for.
    tagKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'timePeriod' - The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
-- * 'searchString' - The value that you want to search for.
-- * 'tagKey' - The key of the tag that you want to return values for.
mkGetTags ::
  -- | 'timePeriod'
  DateInterval ->
  GetTags
mkGetTags pTimePeriod_ =
  GetTags'
    { nextPageToken = Lude.Nothing,
      timePeriod = pTimePeriod_,
      searchString = Lude.Nothing,
      tagKey = Lude.Nothing
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtNextPageToken :: Lens.Lens' GetTags (Lude.Maybe Lude.Text)
gtNextPageToken = Lens.lens (nextPageToken :: GetTags -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetTags)
{-# DEPRECATED gtNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The start and end dates for retrieving the dimension values. The start date is inclusive, but the end date is exclusive. For example, if @start@ is @2017-01-01@ and @end@ is @2017-05-01@ , then the cost and usage data is retrieved from @2017-01-01@ up to and including @2017-04-30@ but not including @2017-05-01@ .
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTimePeriod :: Lens.Lens' GetTags DateInterval
gtTimePeriod = Lens.lens (timePeriod :: GetTags -> DateInterval) (\s a -> s {timePeriod = a} :: GetTags)
{-# DEPRECATED gtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The value that you want to search for.
--
-- /Note:/ Consider using 'searchString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtSearchString :: Lens.Lens' GetTags (Lude.Maybe Lude.Text)
gtSearchString = Lens.lens (searchString :: GetTags -> Lude.Maybe Lude.Text) (\s a -> s {searchString = a} :: GetTags)
{-# DEPRECATED gtSearchString "Use generic-lens or generic-optics with 'searchString' instead." #-}

-- | The key of the tag that you want to return values for.
--
-- /Note:/ Consider using 'tagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTagKey :: Lens.Lens' GetTags (Lude.Maybe Lude.Text)
gtTagKey = Lens.lens (tagKey :: GetTags -> Lude.Maybe Lude.Text) (\s a -> s {tagKey = a} :: GetTags)
{-# DEPRECATED gtTagKey "Use generic-lens or generic-optics with 'tagKey' instead." #-}

instance Lude.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..:> "ReturnSize")
            Lude.<*> (x Lude..:> "TotalSize")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSInsightsIndexService.GetTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTags where
  toJSON GetTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextPageToken" Lude..=) Lude.<$> nextPageToken,
            Lude.Just ("TimePeriod" Lude..= timePeriod),
            ("SearchString" Lude..=) Lude.<$> searchString,
            ("TagKey" Lude..=) Lude.<$> tagKey
          ]
      )

instance Lude.ToPath GetTags where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The number of query results that AWS returns at a time.
    returnSize :: Lude.Int,
    -- | The total number of query results.
    totalSize :: Lude.Int,
    -- | The tags that match your request.
    tags :: [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
-- * 'returnSize' - The number of query results that AWS returns at a time.
-- * 'totalSize' - The total number of query results.
-- * 'tags' - The tags that match your request.
-- * 'responseStatus' - The response status code.
mkGetTagsResponse ::
  -- | 'returnSize'
  Lude.Int ->
  -- | 'totalSize'
  Lude.Int ->
  -- | 'responseStatus'
  Lude.Int ->
  GetTagsResponse
mkGetTagsResponse pReturnSize_ pTotalSize_ pResponseStatus_ =
  GetTagsResponse'
    { nextPageToken = Lude.Nothing,
      returnSize = pReturnSize_,
      totalSize = pTotalSize_,
      tags = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsNextPageToken :: Lens.Lens' GetTagsResponse (Lude.Maybe Lude.Text)
gtrsNextPageToken = Lens.lens (nextPageToken :: GetTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetTagsResponse)
{-# DEPRECATED gtrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The number of query results that AWS returns at a time.
--
-- /Note:/ Consider using 'returnSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsReturnSize :: Lens.Lens' GetTagsResponse Lude.Int
gtrsReturnSize = Lens.lens (returnSize :: GetTagsResponse -> Lude.Int) (\s a -> s {returnSize = a} :: GetTagsResponse)
{-# DEPRECATED gtrsReturnSize "Use generic-lens or generic-optics with 'returnSize' instead." #-}

-- | The total number of query results.
--
-- /Note:/ Consider using 'totalSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTotalSize :: Lens.Lens' GetTagsResponse Lude.Int
gtrsTotalSize = Lens.lens (totalSize :: GetTagsResponse -> Lude.Int) (\s a -> s {totalSize = a} :: GetTagsResponse)
{-# DEPRECATED gtrsTotalSize "Use generic-lens or generic-optics with 'totalSize' instead." #-}

-- | The tags that match your request.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTags :: Lens.Lens' GetTagsResponse [Lude.Text]
gtrsTags = Lens.lens (tags :: GetTagsResponse -> [Lude.Text]) (\s a -> s {tags = a} :: GetTagsResponse)
{-# DEPRECATED gtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTagsResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTagsResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
