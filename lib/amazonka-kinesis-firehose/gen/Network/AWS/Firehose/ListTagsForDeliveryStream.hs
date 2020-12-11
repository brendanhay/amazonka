{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.ListTagsForDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified delivery stream. This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.ListTagsForDeliveryStream
  ( -- * Creating a request
    ListTagsForDeliveryStream (..),
    mkListTagsForDeliveryStream,

    -- ** Request lenses
    ltfdsLimit,
    ltfdsExclusiveStartTagKey,
    ltfdsDeliveryStreamName,

    -- * Destructuring the response
    ListTagsForDeliveryStreamResponse (..),
    mkListTagsForDeliveryStreamResponse,

    -- ** Response lenses
    ltfdsrsResponseStatus,
    ltfdsrsTags,
    ltfdsrsHasMoreTags,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForDeliveryStream' smart constructor.
data ListTagsForDeliveryStream = ListTagsForDeliveryStream'
  { limit ::
      Lude.Maybe Lude.Natural,
    exclusiveStartTagKey ::
      Lude.Maybe Lude.Text,
    deliveryStreamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForDeliveryStream' with the minimum fields required to make a request.
--
-- * 'deliveryStreamName' - The name of the delivery stream whose tags you want to list.
-- * 'exclusiveStartTagKey' - The key to use as the starting point for the list of tags. If you set this parameter, @ListTagsForDeliveryStream@ gets all tags that occur after @ExclusiveStartTagKey@ .
-- * 'limit' - The number of tags to return. If this number is less than the total number of tags associated with the delivery stream, @HasMoreTags@ is set to @true@ in the response. To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response.
mkListTagsForDeliveryStream ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  ListTagsForDeliveryStream
mkListTagsForDeliveryStream pDeliveryStreamName_ =
  ListTagsForDeliveryStream'
    { limit = Lude.Nothing,
      exclusiveStartTagKey = Lude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | The number of tags to return. If this number is less than the total number of tags associated with the delivery stream, @HasMoreTags@ is set to @true@ in the response. To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsLimit :: Lens.Lens' ListTagsForDeliveryStream (Lude.Maybe Lude.Natural)
ltfdsLimit = Lens.lens (limit :: ListTagsForDeliveryStream -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTagsForDeliveryStream)
{-# DEPRECATED ltfdsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The key to use as the starting point for the list of tags. If you set this parameter, @ListTagsForDeliveryStream@ gets all tags that occur after @ExclusiveStartTagKey@ .
--
-- /Note:/ Consider using 'exclusiveStartTagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsExclusiveStartTagKey :: Lens.Lens' ListTagsForDeliveryStream (Lude.Maybe Lude.Text)
ltfdsExclusiveStartTagKey = Lens.lens (exclusiveStartTagKey :: ListTagsForDeliveryStream -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartTagKey = a} :: ListTagsForDeliveryStream)
{-# DEPRECATED ltfdsExclusiveStartTagKey "Use generic-lens or generic-optics with 'exclusiveStartTagKey' instead." #-}

-- | The name of the delivery stream whose tags you want to list.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsDeliveryStreamName :: Lens.Lens' ListTagsForDeliveryStream Lude.Text
ltfdsDeliveryStreamName = Lens.lens (deliveryStreamName :: ListTagsForDeliveryStream -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: ListTagsForDeliveryStream)
{-# DEPRECATED ltfdsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

instance Lude.AWSRequest ListTagsForDeliveryStream where
  type
    Rs ListTagsForDeliveryStream =
      ListTagsForDeliveryStreamResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForDeliveryStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "HasMoreTags")
      )

instance Lude.ToHeaders ListTagsForDeliveryStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.ListTagsForDeliveryStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForDeliveryStream where
  toJSON ListTagsForDeliveryStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Limit" Lude..=) Lude.<$> limit,
            ("ExclusiveStartTagKey" Lude..=) Lude.<$> exclusiveStartTagKey,
            Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName)
          ]
      )

instance Lude.ToPath ListTagsForDeliveryStream where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForDeliveryStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForDeliveryStreamResponse' smart constructor.
data ListTagsForDeliveryStreamResponse = ListTagsForDeliveryStreamResponse'
  { responseStatus ::
      Lude.Int,
    tags :: [Tag],
    hasMoreTags ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- * 'hasMoreTags' - If this is @true@ in the response, more tags are available. To list the remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag returned and call @ListTagsForDeliveryStream@ again.
-- * 'responseStatus' - The response status code.
-- * 'tags' - A list of tags associated with @DeliveryStreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ .
mkListTagsForDeliveryStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'hasMoreTags'
  Lude.Bool ->
  ListTagsForDeliveryStreamResponse
mkListTagsForDeliveryStreamResponse pResponseStatus_ pHasMoreTags_ =
  ListTagsForDeliveryStreamResponse'
    { responseStatus =
        pResponseStatus_,
      tags = Lude.mempty,
      hasMoreTags = pHasMoreTags_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsrsResponseStatus :: Lens.Lens' ListTagsForDeliveryStreamResponse Lude.Int
ltfdsrsResponseStatus = Lens.lens (responseStatus :: ListTagsForDeliveryStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForDeliveryStreamResponse)
{-# DEPRECATED ltfdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of tags associated with @DeliveryStreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsrsTags :: Lens.Lens' ListTagsForDeliveryStreamResponse [Tag]
ltfdsrsTags = Lens.lens (tags :: ListTagsForDeliveryStreamResponse -> [Tag]) (\s a -> s {tags = a} :: ListTagsForDeliveryStreamResponse)
{-# DEPRECATED ltfdsrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | If this is @true@ in the response, more tags are available. To list the remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag returned and call @ListTagsForDeliveryStream@ again.
--
-- /Note:/ Consider using 'hasMoreTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsrsHasMoreTags :: Lens.Lens' ListTagsForDeliveryStreamResponse Lude.Bool
ltfdsrsHasMoreTags = Lens.lens (hasMoreTags :: ListTagsForDeliveryStreamResponse -> Lude.Bool) (\s a -> s {hasMoreTags = a} :: ListTagsForDeliveryStreamResponse)
{-# DEPRECATED ltfdsrsHasMoreTags "Use generic-lens or generic-optics with 'hasMoreTags' instead." #-}
