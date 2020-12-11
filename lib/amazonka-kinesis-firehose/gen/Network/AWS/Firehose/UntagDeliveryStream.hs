{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.UntagDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified delivery stream. Removed tags are deleted, and you can't recover them after this operation successfully completes.
--
-- If you specify a tag that doesn't exist, the operation ignores it.
-- This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.UntagDeliveryStream
  ( -- * Creating a request
    UntagDeliveryStream (..),
    mkUntagDeliveryStream,

    -- ** Request lenses
    udsDeliveryStreamName,
    udsTagKeys,

    -- * Destructuring the response
    UntagDeliveryStreamResponse (..),
    mkUntagDeliveryStreamResponse,

    -- ** Response lenses
    udsrsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagDeliveryStream' smart constructor.
data UntagDeliveryStream = UntagDeliveryStream'
  { deliveryStreamName ::
      Lude.Text,
    tagKeys :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagDeliveryStream' with the minimum fields required to make a request.
--
-- * 'deliveryStreamName' - The name of the delivery stream.
-- * 'tagKeys' - A list of tag keys. Each corresponding tag is removed from the delivery stream.
mkUntagDeliveryStream ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'tagKeys'
  Lude.NonEmpty Lude.Text ->
  UntagDeliveryStream
mkUntagDeliveryStream pDeliveryStreamName_ pTagKeys_ =
  UntagDeliveryStream'
    { deliveryStreamName = pDeliveryStreamName_,
      tagKeys = pTagKeys_
    }

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDeliveryStreamName :: Lens.Lens' UntagDeliveryStream Lude.Text
udsDeliveryStreamName = Lens.lens (deliveryStreamName :: UntagDeliveryStream -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: UntagDeliveryStream)
{-# DEPRECATED udsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | A list of tag keys. Each corresponding tag is removed from the delivery stream.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsTagKeys :: Lens.Lens' UntagDeliveryStream (Lude.NonEmpty Lude.Text)
udsTagKeys = Lens.lens (tagKeys :: UntagDeliveryStream -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeys = a} :: UntagDeliveryStream)
{-# DEPRECATED udsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Lude.AWSRequest UntagDeliveryStream where
  type Rs UntagDeliveryStream = UntagDeliveryStreamResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UntagDeliveryStreamResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagDeliveryStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.UntagDeliveryStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagDeliveryStream where
  toJSON UntagDeliveryStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            Lude.Just ("TagKeys" Lude..= tagKeys)
          ]
      )

instance Lude.ToPath UntagDeliveryStream where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagDeliveryStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagDeliveryStreamResponse' smart constructor.
newtype UntagDeliveryStreamResponse = UntagDeliveryStreamResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUntagDeliveryStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagDeliveryStreamResponse
mkUntagDeliveryStreamResponse pResponseStatus_ =
  UntagDeliveryStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrsResponseStatus :: Lens.Lens' UntagDeliveryStreamResponse Lude.Int
udsrsResponseStatus = Lens.lens (responseStatus :: UntagDeliveryStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagDeliveryStreamResponse)
{-# DEPRECATED udsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
