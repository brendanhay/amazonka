{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.DescribeDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified delivery stream and its status. For example, after your delivery stream is created, call @DescribeDeliveryStream@ to see whether the delivery stream is @ACTIVE@ and therefore ready for data to be sent to it.
--
-- If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke 'CreateDeliveryStream' again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it. If the status is @DELETING_FAILED@ , you can force deletion by invoking 'DeleteDeliveryStream' again but with 'DeleteDeliveryStreamInput$AllowForceDelete' set to true.
module Network.AWS.Firehose.DescribeDeliveryStream
  ( -- * Creating a request
    DescribeDeliveryStream (..),
    mkDescribeDeliveryStream,

    -- ** Request lenses
    dExclusiveStartDestinationId,
    dDeliveryStreamName,
    dLimit,

    -- * Destructuring the response
    DescribeDeliveryStreamResponse (..),
    mkDescribeDeliveryStreamResponse,

    -- ** Response lenses
    ddsrsDeliveryStreamDescription,
    ddsrsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDeliveryStream' smart constructor.
data DescribeDeliveryStream = DescribeDeliveryStream'
  { -- | The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
    exclusiveStartDestinationId :: Lude.Maybe Lude.Text,
    -- | The name of the delivery stream.
    deliveryStreamName :: Lude.Text,
    -- | The limit on the number of destinations to return. You can have one destination per delivery stream.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeliveryStream' with the minimum fields required to make a request.
--
-- * 'exclusiveStartDestinationId' - The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
-- * 'deliveryStreamName' - The name of the delivery stream.
-- * 'limit' - The limit on the number of destinations to return. You can have one destination per delivery stream.
mkDescribeDeliveryStream ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  DescribeDeliveryStream
mkDescribeDeliveryStream pDeliveryStreamName_ =
  DescribeDeliveryStream'
    { exclusiveStartDestinationId =
        Lude.Nothing,
      deliveryStreamName = pDeliveryStreamName_,
      limit = Lude.Nothing
    }

-- | The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
--
-- /Note:/ Consider using 'exclusiveStartDestinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExclusiveStartDestinationId :: Lens.Lens' DescribeDeliveryStream (Lude.Maybe Lude.Text)
dExclusiveStartDestinationId = Lens.lens (exclusiveStartDestinationId :: DescribeDeliveryStream -> Lude.Maybe Lude.Text) (\s a -> s {exclusiveStartDestinationId = a} :: DescribeDeliveryStream)
{-# DEPRECATED dExclusiveStartDestinationId "Use generic-lens or generic-optics with 'exclusiveStartDestinationId' instead." #-}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeliveryStreamName :: Lens.Lens' DescribeDeliveryStream Lude.Text
dDeliveryStreamName = Lens.lens (deliveryStreamName :: DescribeDeliveryStream -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: DescribeDeliveryStream)
{-# DEPRECATED dDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | The limit on the number of destinations to return. You can have one destination per delivery stream.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLimit :: Lens.Lens' DescribeDeliveryStream (Lude.Maybe Lude.Natural)
dLimit = Lens.lens (limit :: DescribeDeliveryStream -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeDeliveryStream)
{-# DEPRECATED dLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.AWSRequest DescribeDeliveryStream where
  type Rs DescribeDeliveryStream = DescribeDeliveryStreamResponse
  request = Req.postJSON firehoseService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDeliveryStreamResponse'
            Lude.<$> (x Lude..:> "DeliveryStreamDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDeliveryStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Firehose_20150804.DescribeDeliveryStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDeliveryStream where
  toJSON DescribeDeliveryStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExclusiveStartDestinationId" Lude..=)
              Lude.<$> exclusiveStartDestinationId,
            Lude.Just ("DeliveryStreamName" Lude..= deliveryStreamName),
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeDeliveryStream where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDeliveryStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDeliveryStreamResponse' smart constructor.
data DescribeDeliveryStreamResponse = DescribeDeliveryStreamResponse'
  { -- | Information about the delivery stream.
    deliveryStreamDescription :: DeliveryStreamDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- * 'deliveryStreamDescription' - Information about the delivery stream.
-- * 'responseStatus' - The response status code.
mkDescribeDeliveryStreamResponse ::
  -- | 'deliveryStreamDescription'
  DeliveryStreamDescription ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDeliveryStreamResponse
mkDescribeDeliveryStreamResponse
  pDeliveryStreamDescription_
  pResponseStatus_ =
    DescribeDeliveryStreamResponse'
      { deliveryStreamDescription =
          pDeliveryStreamDescription_,
        responseStatus = pResponseStatus_
      }

-- | Information about the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsDeliveryStreamDescription :: Lens.Lens' DescribeDeliveryStreamResponse DeliveryStreamDescription
ddsrsDeliveryStreamDescription = Lens.lens (deliveryStreamDescription :: DescribeDeliveryStreamResponse -> DeliveryStreamDescription) (\s a -> s {deliveryStreamDescription = a} :: DescribeDeliveryStreamResponse)
{-# DEPRECATED ddsrsDeliveryStreamDescription "Use generic-lens or generic-optics with 'deliveryStreamDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsResponseStatus :: Lens.Lens' DescribeDeliveryStreamResponse Lude.Int
ddsrsResponseStatus = Lens.lens (responseStatus :: DescribeDeliveryStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDeliveryStreamResponse)
{-# DEPRECATED ddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
