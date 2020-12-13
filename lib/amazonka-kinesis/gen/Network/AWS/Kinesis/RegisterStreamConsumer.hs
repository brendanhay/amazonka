{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.RegisterStreamConsumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a consumer with a Kinesis data stream. When you use this operation, the consumer you register can then call 'SubscribeToShard' to receive data from the stream using enhanced fan-out, at a rate of up to 2 MiB per second for every shard you subscribe to. This rate is unaffected by the total number of consumers that read from the same stream.
--
-- You can register up to 20 consumers per stream. A given consumer can only be registered with one stream at a time.
-- For an example of how to use this operations, see </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API> .
-- The use of this operation has a limit of five transactions per second per account. Also, only 5 consumers can be created simultaneously. In other words, you cannot have more than 5 consumers in a @CREATING@ status at the same time. Registering a 6th consumer while there are 5 in a @CREATING@ status results in a @LimitExceededException@ .
module Network.AWS.Kinesis.RegisterStreamConsumer
  ( -- * Creating a request
    RegisterStreamConsumer (..),
    mkRegisterStreamConsumer,

    -- ** Request lenses
    rscStreamARN,
    rscConsumerName,

    -- * Destructuring the response
    RegisterStreamConsumerResponse (..),
    mkRegisterStreamConsumerResponse,

    -- ** Response lenses
    rscrsConsumer,
    rscrsResponseStatus,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterStreamConsumer' smart constructor.
data RegisterStreamConsumer = RegisterStreamConsumer'
  { -- | The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    streamARN :: Lude.Text,
    -- | For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
    consumerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterStreamConsumer' with the minimum fields required to make a request.
--
-- * 'streamARN' - The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
-- * 'consumerName' - For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
mkRegisterStreamConsumer ::
  -- | 'streamARN'
  Lude.Text ->
  -- | 'consumerName'
  Lude.Text ->
  RegisterStreamConsumer
mkRegisterStreamConsumer pStreamARN_ pConsumerName_ =
  RegisterStreamConsumer'
    { streamARN = pStreamARN_,
      consumerName = pConsumerName_
    }

-- | The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscStreamARN :: Lens.Lens' RegisterStreamConsumer Lude.Text
rscStreamARN = Lens.lens (streamARN :: RegisterStreamConsumer -> Lude.Text) (\s a -> s {streamARN = a} :: RegisterStreamConsumer)
{-# DEPRECATED rscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscConsumerName :: Lens.Lens' RegisterStreamConsumer Lude.Text
rscConsumerName = Lens.lens (consumerName :: RegisterStreamConsumer -> Lude.Text) (\s a -> s {consumerName = a} :: RegisterStreamConsumer)
{-# DEPRECATED rscConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

instance Lude.AWSRequest RegisterStreamConsumer where
  type Rs RegisterStreamConsumer = RegisterStreamConsumerResponse
  request = Req.postJSON kinesisService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterStreamConsumerResponse'
            Lude.<$> (x Lude..:> "Consumer") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterStreamConsumer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.RegisterStreamConsumer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterStreamConsumer where
  toJSON RegisterStreamConsumer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamARN" Lude..= streamARN),
            Lude.Just ("ConsumerName" Lude..= consumerName)
          ]
      )

instance Lude.ToPath RegisterStreamConsumer where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterStreamConsumer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterStreamConsumerResponse' smart constructor.
data RegisterStreamConsumerResponse = RegisterStreamConsumerResponse'
  { -- | An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
    consumer :: Consumer,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterStreamConsumerResponse' with the minimum fields required to make a request.
--
-- * 'consumer' - An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
-- * 'responseStatus' - The response status code.
mkRegisterStreamConsumerResponse ::
  -- | 'consumer'
  Consumer ->
  -- | 'responseStatus'
  Lude.Int ->
  RegisterStreamConsumerResponse
mkRegisterStreamConsumerResponse pConsumer_ pResponseStatus_ =
  RegisterStreamConsumerResponse'
    { consumer = pConsumer_,
      responseStatus = pResponseStatus_
    }

-- | An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
--
-- /Note:/ Consider using 'consumer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscrsConsumer :: Lens.Lens' RegisterStreamConsumerResponse Consumer
rscrsConsumer = Lens.lens (consumer :: RegisterStreamConsumerResponse -> Consumer) (\s a -> s {consumer = a} :: RegisterStreamConsumerResponse)
{-# DEPRECATED rscrsConsumer "Use generic-lens or generic-optics with 'consumer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscrsResponseStatus :: Lens.Lens' RegisterStreamConsumerResponse Lude.Int
rscrsResponseStatus = Lens.lens (responseStatus :: RegisterStreamConsumerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterStreamConsumerResponse)
{-# DEPRECATED rscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
