{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DescribeStreamConsumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To get the description of a registered consumer, provide the ARN of the consumer. Alternatively, you can provide the ARN of the data stream and the name you gave the consumer when you registered it. You may also provide all three parameters, as long as they don't conflict with each other. If you don't know the name or ARN of the consumer that you want to describe, you can use the 'ListStreamConsumers' operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream.
--
-- This operation has a limit of 20 transactions per second per stream.
module Network.AWS.Kinesis.DescribeStreamConsumer
  ( -- * Creating a request
    DescribeStreamConsumer (..),
    mkDescribeStreamConsumer,

    -- ** Request lenses
    dConsumerARN,
    dStreamARN,
    dConsumerName,

    -- * Destructuring the response
    DescribeStreamConsumerResponse (..),
    mkDescribeStreamConsumerResponse,

    -- ** Response lenses
    dscrsResponseStatus,
    dscrsConsumerDescription,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStreamConsumer' smart constructor.
data DescribeStreamConsumer = DescribeStreamConsumer'
  { consumerARN ::
      Lude.Maybe Lude.Text,
    streamARN :: Lude.Maybe Lude.Text,
    consumerName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamConsumer' with the minimum fields required to make a request.
--
-- * 'consumerARN' - The ARN returned by Kinesis Data Streams when you registered the consumer.
-- * 'consumerName' - The name that you gave to the consumer.
-- * 'streamARN' - The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkDescribeStreamConsumer ::
  DescribeStreamConsumer
mkDescribeStreamConsumer =
  DescribeStreamConsumer'
    { consumerARN = Lude.Nothing,
      streamARN = Lude.Nothing,
      consumerName = Lude.Nothing
    }

-- | The ARN returned by Kinesis Data Streams when you registered the consumer.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConsumerARN :: Lens.Lens' DescribeStreamConsumer (Lude.Maybe Lude.Text)
dConsumerARN = Lens.lens (consumerARN :: DescribeStreamConsumer -> Lude.Maybe Lude.Text) (\s a -> s {consumerARN = a} :: DescribeStreamConsumer)
{-# DEPRECATED dConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamARN :: Lens.Lens' DescribeStreamConsumer (Lude.Maybe Lude.Text)
dStreamARN = Lens.lens (streamARN :: DescribeStreamConsumer -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: DescribeStreamConsumer)
{-# DEPRECATED dStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name that you gave to the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConsumerName :: Lens.Lens' DescribeStreamConsumer (Lude.Maybe Lude.Text)
dConsumerName = Lens.lens (consumerName :: DescribeStreamConsumer -> Lude.Maybe Lude.Text) (\s a -> s {consumerName = a} :: DescribeStreamConsumer)
{-# DEPRECATED dConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

instance Lude.AWSRequest DescribeStreamConsumer where
  type Rs DescribeStreamConsumer = DescribeStreamConsumerResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStreamConsumerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ConsumerDescription")
      )

instance Lude.ToHeaders DescribeStreamConsumer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DescribeStreamConsumer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStreamConsumer where
  toJSON DescribeStreamConsumer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConsumerARN" Lude..=) Lude.<$> consumerARN,
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("ConsumerName" Lude..=) Lude.<$> consumerName
          ]
      )

instance Lude.ToPath DescribeStreamConsumer where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStreamConsumer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStreamConsumerResponse' smart constructor.
data DescribeStreamConsumerResponse = DescribeStreamConsumerResponse'
  { responseStatus ::
      Lude.Int,
    consumerDescription ::
      ConsumerDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamConsumerResponse' with the minimum fields required to make a request.
--
-- * 'consumerDescription' - An object that represents the details of the consumer.
-- * 'responseStatus' - The response status code.
mkDescribeStreamConsumerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'consumerDescription'
  ConsumerDescription ->
  DescribeStreamConsumerResponse
mkDescribeStreamConsumerResponse
  pResponseStatus_
  pConsumerDescription_ =
    DescribeStreamConsumerResponse'
      { responseStatus =
          pResponseStatus_,
        consumerDescription = pConsumerDescription_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DescribeStreamConsumerResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DescribeStreamConsumerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamConsumerResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An object that represents the details of the consumer.
--
-- /Note:/ Consider using 'consumerDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsConsumerDescription :: Lens.Lens' DescribeStreamConsumerResponse ConsumerDescription
dscrsConsumerDescription = Lens.lens (consumerDescription :: DescribeStreamConsumerResponse -> ConsumerDescription) (\s a -> s {consumerDescription = a} :: DescribeStreamConsumerResponse)
{-# DEPRECATED dscrsConsumerDescription "Use generic-lens or generic-optics with 'consumerDescription' instead." #-}
