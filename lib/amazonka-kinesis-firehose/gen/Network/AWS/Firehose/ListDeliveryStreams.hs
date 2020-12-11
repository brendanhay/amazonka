{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.ListDeliveryStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your delivery streams in alphabetical order of their names.
--
-- The number of delivery streams might be too large to return using a single call to @ListDeliveryStreams@ . You can limit the number of delivery streams returned, using the @Limit@ parameter. To determine whether there are more delivery streams to list, check the value of @HasMoreDeliveryStreams@ in the output. If there are more delivery streams to list, you can request them by calling this operation again and setting the @ExclusiveStartDeliveryStreamName@ parameter to the name of the last delivery stream returned in the last call.
module Network.AWS.Firehose.ListDeliveryStreams
  ( -- * Creating a request
    ListDeliveryStreams (..),
    mkListDeliveryStreams,

    -- ** Request lenses
    ldsLimit,
    ldsDeliveryStreamType,
    ldsExclusiveStartDeliveryStreamName,

    -- * Destructuring the response
    ListDeliveryStreamsResponse (..),
    mkListDeliveryStreamsResponse,

    -- ** Response lenses
    ldsrsResponseStatus,
    ldsrsDeliveryStreamNames,
    ldsrsHasMoreDeliveryStreams,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeliveryStreams' smart constructor.
data ListDeliveryStreams = ListDeliveryStreams'
  { limit ::
      Lude.Maybe Lude.Natural,
    deliveryStreamType :: Lude.Maybe DeliveryStreamType,
    exclusiveStartDeliveryStreamName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeliveryStreams' with the minimum fields required to make a request.
--
-- * 'deliveryStreamType' - The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
-- This parameter is optional. If this parameter is omitted, delivery streams of all types are returned.
-- * 'exclusiveStartDeliveryStreamName' - The list of delivery streams returned by this call to @ListDeliveryStreams@ will start with the delivery stream whose name comes alphabetically immediately after the name you specify in @ExclusiveStartDeliveryStreamName@ .
-- * 'limit' - The maximum number of delivery streams to list. The default value is 10.
mkListDeliveryStreams ::
  ListDeliveryStreams
mkListDeliveryStreams =
  ListDeliveryStreams'
    { limit = Lude.Nothing,
      deliveryStreamType = Lude.Nothing,
      exclusiveStartDeliveryStreamName = Lude.Nothing
    }

-- | The maximum number of delivery streams to list. The default value is 10.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsLimit :: Lens.Lens' ListDeliveryStreams (Lude.Maybe Lude.Natural)
ldsLimit = Lens.lens (limit :: ListDeliveryStreams -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListDeliveryStreams)
{-# DEPRECATED ldsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
-- This parameter is optional. If this parameter is omitted, delivery streams of all types are returned.
--
-- /Note:/ Consider using 'deliveryStreamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsDeliveryStreamType :: Lens.Lens' ListDeliveryStreams (Lude.Maybe DeliveryStreamType)
ldsDeliveryStreamType = Lens.lens (deliveryStreamType :: ListDeliveryStreams -> Lude.Maybe DeliveryStreamType) (\s a -> s {deliveryStreamType = a} :: ListDeliveryStreams)
{-# DEPRECATED ldsDeliveryStreamType "Use generic-lens or generic-optics with 'deliveryStreamType' instead." #-}

-- | The list of delivery streams returned by this call to @ListDeliveryStreams@ will start with the delivery stream whose name comes alphabetically immediately after the name you specify in @ExclusiveStartDeliveryStreamName@ .
--
-- /Note:/ Consider using 'exclusiveStartDeliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsExclusiveStartDeliveryStreamName :: Lens.Lens' ListDeliveryStreams (Lude.Maybe Lude.Text)
ldsExclusiveStartDeliveryStreamName = Lens.lens (exclusiveStartDeliveryStreamName :: ListDeliveryStreams -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartDeliveryStreamName = a} :: ListDeliveryStreams)
{-# DEPRECATED ldsExclusiveStartDeliveryStreamName "Use generic-lens or generic-optics with 'exclusiveStartDeliveryStreamName' instead." #-}

instance Lude.AWSRequest ListDeliveryStreams where
  type Rs ListDeliveryStreams = ListDeliveryStreamsResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeliveryStreamsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "DeliveryStreamNames" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "HasMoreDeliveryStreams")
      )

instance Lude.ToHeaders ListDeliveryStreams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.ListDeliveryStreams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeliveryStreams where
  toJSON ListDeliveryStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Limit" Lude..=) Lude.<$> limit,
            ("DeliveryStreamType" Lude..=) Lude.<$> deliveryStreamType,
            ("ExclusiveStartDeliveryStreamName" Lude..=)
              Lude.<$> exclusiveStartDeliveryStreamName
          ]
      )

instance Lude.ToPath ListDeliveryStreams where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeliveryStreams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDeliveryStreamsResponse' smart constructor.
data ListDeliveryStreamsResponse = ListDeliveryStreamsResponse'
  { responseStatus ::
      Lude.Int,
    deliveryStreamNames :: [Lude.Text],
    hasMoreDeliveryStreams :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeliveryStreamsResponse' with the minimum fields required to make a request.
--
-- * 'deliveryStreamNames' - The names of the delivery streams.
-- * 'hasMoreDeliveryStreams' - Indicates whether there are more delivery streams available to list.
-- * 'responseStatus' - The response status code.
mkListDeliveryStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'hasMoreDeliveryStreams'
  Lude.Bool ->
  ListDeliveryStreamsResponse
mkListDeliveryStreamsResponse
  pResponseStatus_
  pHasMoreDeliveryStreams_ =
    ListDeliveryStreamsResponse'
      { responseStatus = pResponseStatus_,
        deliveryStreamNames = Lude.mempty,
        hasMoreDeliveryStreams = pHasMoreDeliveryStreams_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrsResponseStatus :: Lens.Lens' ListDeliveryStreamsResponse Lude.Int
ldsrsResponseStatus = Lens.lens (responseStatus :: ListDeliveryStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeliveryStreamsResponse)
{-# DEPRECATED ldsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The names of the delivery streams.
--
-- /Note:/ Consider using 'deliveryStreamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrsDeliveryStreamNames :: Lens.Lens' ListDeliveryStreamsResponse [Lude.Text]
ldsrsDeliveryStreamNames = Lens.lens (deliveryStreamNames :: ListDeliveryStreamsResponse -> [Lude.Text]) (\s a -> s {deliveryStreamNames = a} :: ListDeliveryStreamsResponse)
{-# DEPRECATED ldsrsDeliveryStreamNames "Use generic-lens or generic-optics with 'deliveryStreamNames' instead." #-}

-- | Indicates whether there are more delivery streams available to list.
--
-- /Note:/ Consider using 'hasMoreDeliveryStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrsHasMoreDeliveryStreams :: Lens.Lens' ListDeliveryStreamsResponse Lude.Bool
ldsrsHasMoreDeliveryStreams = Lens.lens (hasMoreDeliveryStreams :: ListDeliveryStreamsResponse -> Lude.Bool) (\s a -> s {hasMoreDeliveryStreams = a} :: ListDeliveryStreamsResponse)
{-# DEPRECATED ldsrsHasMoreDeliveryStreams "Use generic-lens or generic-optics with 'hasMoreDeliveryStreams' instead." #-}
