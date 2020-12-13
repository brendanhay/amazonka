{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.TagDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- Each delivery stream can have up to 50 tags.
-- This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.TagDeliveryStream
  ( -- * Creating a request
    TagDeliveryStream (..),
    mkTagDeliveryStream,

    -- ** Request lenses
    tdsDeliveryStreamName,
    tdsTags,

    -- * Destructuring the response
    TagDeliveryStreamResponse (..),
    mkTagDeliveryStreamResponse,

    -- ** Response lenses
    tdsrsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagDeliveryStream' smart constructor.
data TagDeliveryStream = TagDeliveryStream'
  { -- | The name of the delivery stream to which you want to add the tags.
    deliveryStreamName :: Lude.Text,
    -- | A set of key-value pairs to use to create the tags.
    tags :: Lude.NonEmpty Tag
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagDeliveryStream' with the minimum fields required to make a request.
--
-- * 'deliveryStreamName' - The name of the delivery stream to which you want to add the tags.
-- * 'tags' - A set of key-value pairs to use to create the tags.
mkTagDeliveryStream ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'tags'
  Lude.NonEmpty Tag ->
  TagDeliveryStream
mkTagDeliveryStream pDeliveryStreamName_ pTags_ =
  TagDeliveryStream'
    { deliveryStreamName = pDeliveryStreamName_,
      tags = pTags_
    }

-- | The name of the delivery stream to which you want to add the tags.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsDeliveryStreamName :: Lens.Lens' TagDeliveryStream Lude.Text
tdsDeliveryStreamName = Lens.lens (deliveryStreamName :: TagDeliveryStream -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: TagDeliveryStream)
{-# DEPRECATED tdsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | A set of key-value pairs to use to create the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsTags :: Lens.Lens' TagDeliveryStream (Lude.NonEmpty Tag)
tdsTags = Lens.lens (tags :: TagDeliveryStream -> Lude.NonEmpty Tag) (\s a -> s {tags = a} :: TagDeliveryStream)
{-# DEPRECATED tdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagDeliveryStream where
  type Rs TagDeliveryStream = TagDeliveryStreamResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TagDeliveryStreamResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagDeliveryStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.TagDeliveryStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagDeliveryStream where
  toJSON TagDeliveryStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagDeliveryStream where
  toPath = Lude.const "/"

instance Lude.ToQuery TagDeliveryStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagDeliveryStreamResponse' smart constructor.
newtype TagDeliveryStreamResponse = TagDeliveryStreamResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTagDeliveryStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagDeliveryStreamResponse
mkTagDeliveryStreamResponse pResponseStatus_ =
  TagDeliveryStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsrsResponseStatus :: Lens.Lens' TagDeliveryStreamResponse Lude.Int
tdsrsResponseStatus = Lens.lens (responseStatus :: TagDeliveryStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagDeliveryStreamResponse)
{-# DEPRECATED tdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
