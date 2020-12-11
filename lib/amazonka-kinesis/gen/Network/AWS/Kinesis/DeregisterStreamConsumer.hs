{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DeregisterStreamConsumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To deregister a consumer, provide its ARN. Alternatively, you can provide the ARN of the data stream and the name you gave the consumer when you registered it. You may also provide all three parameters, as long as they don't conflict with each other. If you don't know the name or ARN of the consumer that you want to deregister, you can use the 'ListStreamConsumers' operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its name and ARN.
--
-- This operation has a limit of five transactions per second per stream.
module Network.AWS.Kinesis.DeregisterStreamConsumer
  ( -- * Creating a request
    DeregisterStreamConsumer (..),
    mkDeregisterStreamConsumer,

    -- ** Request lenses
    dscConsumerARN,
    dscStreamARN,
    dscConsumerName,

    -- * Destructuring the response
    DeregisterStreamConsumerResponse (..),
    mkDeregisterStreamConsumerResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterStreamConsumer' smart constructor.
data DeregisterStreamConsumer = DeregisterStreamConsumer'
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

-- | Creates a value of 'DeregisterStreamConsumer' with the minimum fields required to make a request.
--
-- * 'consumerARN' - The ARN returned by Kinesis Data Streams when you registered the consumer. If you don't know the ARN of the consumer that you want to deregister, you can use the ListStreamConsumers operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its ARN.
-- * 'consumerName' - The name that you gave to the consumer.
-- * 'streamARN' - The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkDeregisterStreamConsumer ::
  DeregisterStreamConsumer
mkDeregisterStreamConsumer =
  DeregisterStreamConsumer'
    { consumerARN = Lude.Nothing,
      streamARN = Lude.Nothing,
      consumerName = Lude.Nothing
    }

-- | The ARN returned by Kinesis Data Streams when you registered the consumer. If you don't know the ARN of the consumer that you want to deregister, you can use the ListStreamConsumers operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its ARN.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscConsumerARN :: Lens.Lens' DeregisterStreamConsumer (Lude.Maybe Lude.Text)
dscConsumerARN = Lens.lens (consumerARN :: DeregisterStreamConsumer -> Lude.Maybe Lude.Text) (\s a -> s {consumerARN = a} :: DeregisterStreamConsumer)
{-# DEPRECATED dscConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscStreamARN :: Lens.Lens' DeregisterStreamConsumer (Lude.Maybe Lude.Text)
dscStreamARN = Lens.lens (streamARN :: DeregisterStreamConsumer -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: DeregisterStreamConsumer)
{-# DEPRECATED dscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name that you gave to the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscConsumerName :: Lens.Lens' DeregisterStreamConsumer (Lude.Maybe Lude.Text)
dscConsumerName = Lens.lens (consumerName :: DeregisterStreamConsumer -> Lude.Maybe Lude.Text) (\s a -> s {consumerName = a} :: DeregisterStreamConsumer)
{-# DEPRECATED dscConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

instance Lude.AWSRequest DeregisterStreamConsumer where
  type Rs DeregisterStreamConsumer = DeregisterStreamConsumerResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull DeregisterStreamConsumerResponse'

instance Lude.ToHeaders DeregisterStreamConsumer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.DeregisterStreamConsumer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterStreamConsumer where
  toJSON DeregisterStreamConsumer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConsumerARN" Lude..=) Lude.<$> consumerARN,
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("ConsumerName" Lude..=) Lude.<$> consumerName
          ]
      )

instance Lude.ToPath DeregisterStreamConsumer where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterStreamConsumer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterStreamConsumerResponse' smart constructor.
data DeregisterStreamConsumerResponse = DeregisterStreamConsumerResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterStreamConsumerResponse' with the minimum fields required to make a request.
mkDeregisterStreamConsumerResponse ::
  DeregisterStreamConsumerResponse
mkDeregisterStreamConsumerResponse =
  DeregisterStreamConsumerResponse'
