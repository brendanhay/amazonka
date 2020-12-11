{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified Kinesis data stream. This operation has a limit of five transactions per second per account.
module Network.AWS.Kinesis.ListTagsForStream
  ( -- * Creating a request
    ListTagsForStream (..),
    mkListTagsForStream,

    -- ** Request lenses
    ltfsLimit,
    ltfsExclusiveStartTagKey,
    ltfsStreamName,

    -- * Destructuring the response
    ListTagsForStreamResponse (..),
    mkListTagsForStreamResponse,

    -- ** Response lenses
    ltfsrsResponseStatus,
    ltfsrsTags,
    ltfsrsHasMoreTags,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @ListTagsForStream@ .
--
-- /See:/ 'mkListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { limit ::
      Lude.Maybe Lude.Natural,
    exclusiveStartTagKey :: Lude.Maybe Lude.Text,
    streamName :: Lude.Text
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
-- * 'exclusiveStartTagKey' - The key to use as the starting point for the list of tags. If this parameter is set, @ListTagsForStream@ gets all tags that occur after @ExclusiveStartTagKey@ .
-- * 'limit' - The number of tags to return. If this number is less than the total number of tags associated with the stream, @HasMoreTags@ is set to @true@ . To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response.
-- * 'streamName' - The name of the stream.
mkListTagsForStream ::
  -- | 'streamName'
  Lude.Text ->
  ListTagsForStream
mkListTagsForStream pStreamName_ =
  ListTagsForStream'
    { limit = Lude.Nothing,
      exclusiveStartTagKey = Lude.Nothing,
      streamName = pStreamName_
    }

-- | The number of tags to return. If this number is less than the total number of tags associated with the stream, @HasMoreTags@ is set to @true@ . To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsLimit :: Lens.Lens' ListTagsForStream (Lude.Maybe Lude.Natural)
ltfsLimit = Lens.lens (limit :: ListTagsForStream -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTagsForStream)
{-# DEPRECATED ltfsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The key to use as the starting point for the list of tags. If this parameter is set, @ListTagsForStream@ gets all tags that occur after @ExclusiveStartTagKey@ .
--
-- /Note:/ Consider using 'exclusiveStartTagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsExclusiveStartTagKey :: Lens.Lens' ListTagsForStream (Lude.Maybe Lude.Text)
ltfsExclusiveStartTagKey = Lens.lens (exclusiveStartTagKey :: ListTagsForStream -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartTagKey = a} :: ListTagsForStream)
{-# DEPRECATED ltfsExclusiveStartTagKey "Use generic-lens or generic-optics with 'exclusiveStartTagKey' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamName :: Lens.Lens' ListTagsForStream Lude.Text
ltfsStreamName = Lens.lens (streamName :: ListTagsForStream -> Lude.Text) (\s a -> s {streamName = a} :: ListTagsForStream)
{-# DEPRECATED ltfsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest ListTagsForStream where
  type Rs ListTagsForStream = ListTagsForStreamResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "HasMoreTags")
      )

instance Lude.ToHeaders ListTagsForStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.ListTagsForStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Limit" Lude..=) Lude.<$> limit,
            ("ExclusiveStartTagKey" Lude..=) Lude.<$> exclusiveStartTagKey,
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath ListTagsForStream where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForStream where
  toQuery = Lude.const Lude.mempty

-- | Represents the output for @ListTagsForStream@ .
--
-- /See:/ 'mkListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { responseStatus ::
      Lude.Int,
    tags :: [Tag],
    hasMoreTags :: Lude.Bool
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
-- * 'hasMoreTags' - If set to @true@ , more tags are available. To request additional tags, set @ExclusiveStartTagKey@ to the key of the last tag returned.
-- * 'responseStatus' - The response status code.
-- * 'tags' - A list of tags associated with @StreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ .
mkListTagsForStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'hasMoreTags'
  Lude.Bool ->
  ListTagsForStreamResponse
mkListTagsForStreamResponse pResponseStatus_ pHasMoreTags_ =
  ListTagsForStreamResponse'
    { responseStatus = pResponseStatus_,
      tags = Lude.mempty,
      hasMoreTags = pHasMoreTags_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrsResponseStatus :: Lens.Lens' ListTagsForStreamResponse Lude.Int
ltfsrsResponseStatus = Lens.lens (responseStatus :: ListTagsForStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForStreamResponse)
{-# DEPRECATED ltfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of tags associated with @StreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrsTags :: Lens.Lens' ListTagsForStreamResponse [Tag]
ltfsrsTags = Lens.lens (tags :: ListTagsForStreamResponse -> [Tag]) (\s a -> s {tags = a} :: ListTagsForStreamResponse)
{-# DEPRECATED ltfsrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | If set to @true@ , more tags are available. To request additional tags, set @ExclusiveStartTagKey@ to the key of the last tag returned.
--
-- /Note:/ Consider using 'hasMoreTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrsHasMoreTags :: Lens.Lens' ListTagsForStreamResponse Lude.Bool
ltfsrsHasMoreTags = Lens.lens (hasMoreTags :: ListTagsForStreamResponse -> Lude.Bool) (\s a -> s {hasMoreTags = a} :: ListTagsForStreamResponse)
{-# DEPRECATED ltfsrsHasMoreTags "Use generic-lens or generic-optics with 'hasMoreTags' instead." #-}
