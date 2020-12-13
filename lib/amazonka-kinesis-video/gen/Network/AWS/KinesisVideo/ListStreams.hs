{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @StreamInfo@ objects. Each object describes a stream. To retrieve only streams that satisfy a specific condition, you can specify a @StreamNameCondition@ .
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideo.ListStreams
  ( -- * Creating a request
    ListStreams (..),
    mkListStreams,

    -- ** Request lenses
    lsNextToken,
    lsStreamNameCondition,
    lsMaxResults,

    -- * Destructuring the response
    ListStreamsResponse (..),
    mkListStreamsResponse,

    -- ** Response lenses
    lsrsStreamInfoList,
    lsrsNextToken,
    lsrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition.
    streamNameCondition :: Lude.Maybe StreamNameCondition,
    -- | The maximum number of streams to return in the response. The default is 10,000.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreams' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
-- * 'streamNameCondition' - Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition.
-- * 'maxResults' - The maximum number of streams to return in the response. The default is 10,000.
mkListStreams ::
  ListStreams
mkListStreams =
  ListStreams'
    { nextToken = Lude.Nothing,
      streamNameCondition = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListStreams (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListStreams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreams)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition.
--
-- /Note:/ Consider using 'streamNameCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStreamNameCondition :: Lens.Lens' ListStreams (Lude.Maybe StreamNameCondition)
lsStreamNameCondition = Lens.lens (streamNameCondition :: ListStreams -> Lude.Maybe StreamNameCondition) (\s a -> s {streamNameCondition = a} :: ListStreams)
{-# DEPRECATED lsStreamNameCondition "Use generic-lens or generic-optics with 'streamNameCondition' instead." #-}

-- | The maximum number of streams to return in the response. The default is 10,000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListStreams (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListStreams -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListStreams)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListStreams where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsStreamInfoList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListStreams where
  type Rs ListStreams = ListStreamsResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Lude.<$> (x Lude..?> "StreamInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreams where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StreamNameCondition" Lude..=) Lude.<$> streamNameCondition,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListStreams where
  toPath = Lude.const "/listStreams"

instance Lude.ToQuery ListStreams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | An array of @StreamInfo@ objects.
    streamInfoList :: Lude.Maybe [StreamInfo],
    -- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamsResponse' with the minimum fields required to make a request.
--
-- * 'streamInfoList' - An array of @StreamInfo@ objects.
-- * 'nextToken' - If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
-- * 'responseStatus' - The response status code.
mkListStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamsResponse
mkListStreamsResponse pResponseStatus_ =
  ListStreamsResponse'
    { streamInfoList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @StreamInfo@ objects.
--
-- /Note:/ Consider using 'streamInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsStreamInfoList :: Lens.Lens' ListStreamsResponse (Lude.Maybe [StreamInfo])
lsrsStreamInfoList = Lens.lens (streamInfoList :: ListStreamsResponse -> Lude.Maybe [StreamInfo]) (\s a -> s {streamInfoList = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsStreamInfoList "Use generic-lens or generic-optics with 'streamInfoList' instead." #-}

-- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListStreamsResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListStreamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListStreamsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
