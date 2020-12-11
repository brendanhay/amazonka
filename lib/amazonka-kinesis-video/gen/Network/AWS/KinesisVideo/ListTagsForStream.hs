{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListTagsForStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified stream.
--
-- In the request, you must specify either the @StreamName@ or the @StreamARN@ .
module Network.AWS.KinesisVideo.ListTagsForStream
  ( -- * Creating a request
    ListTagsForStream (..),
    mkListTagsForStream,

    -- ** Request lenses
    ltfsStreamARN,
    ltfsNextToken,
    ltfsStreamName,

    -- * Destructuring the response
    ListTagsForStreamResponse (..),
    mkListTagsForStreamResponse,

    -- ** Response lenses
    ltfsrsNextToken,
    ltfsrsTags,
    ltfsrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { streamARN ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForStream' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream that you want to list tags for.
-- * 'streamName' - The name of the stream that you want to list tags for.
mkListTagsForStream ::
  ListTagsForStream
mkListTagsForStream =
  ListTagsForStream'
    { streamARN = Lude.Nothing,
      nextToken = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to list tags for.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamARN :: Lens.Lens' ListTagsForStream (Lude.Maybe Lude.Text)
ltfsStreamARN = Lens.lens (streamARN :: ListTagsForStream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: ListTagsForStream)
{-# DEPRECATED ltfsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsNextToken :: Lens.Lens' ListTagsForStream (Lude.Maybe Lude.Text)
ltfsNextToken = Lens.lens (nextToken :: ListTagsForStream -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForStream)
{-# DEPRECATED ltfsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the stream that you want to list tags for.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamName :: Lens.Lens' ListTagsForStream (Lude.Maybe Lude.Text)
ltfsStreamName = Lens.lens (streamName :: ListTagsForStream -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: ListTagsForStream)
{-# DEPRECATED ltfsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest ListTagsForStream where
  type Rs ListTagsForStream = ListTagsForStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForStreamResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath ListTagsForStream where
  toPath = Lude.const "/listTagsForStream"

instance Lude.ToQuery ListTagsForStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'ListTagsForStreamResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
-- * 'responseStatus' - The response status code.
-- * 'tags' - A map of tag keys and values associated with the specified stream.
mkListTagsForStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForStreamResponse
mkListTagsForStreamResponse pResponseStatus_ =
  ListTagsForStreamResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrsNextToken :: Lens.Lens' ListTagsForStreamResponse (Lude.Maybe Lude.Text)
ltfsrsNextToken = Lens.lens (nextToken :: ListTagsForStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForStreamResponse)
{-# DEPRECATED ltfsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A map of tag keys and values associated with the specified stream.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrsTags :: Lens.Lens' ListTagsForStreamResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ltfsrsTags = Lens.lens (tags :: ListTagsForStreamResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListTagsForStreamResponse)
{-# DEPRECATED ltfsrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrsResponseStatus :: Lens.Lens' ListTagsForStreamResponse Lude.Int
ltfsrsResponseStatus = Lens.lens (responseStatus :: ListTagsForStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForStreamResponse)
{-# DEPRECATED ltfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
