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
    rscrrsConsumer,
    rscrrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterStreamConsumer' smart constructor.
data RegisterStreamConsumer = RegisterStreamConsumer'
  { -- | The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    streamARN :: Types.StreamARN,
    -- | For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
    consumerName :: Types.ConsumerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterStreamConsumer' value with any optional fields omitted.
mkRegisterStreamConsumer ::
  -- | 'streamARN'
  Types.StreamARN ->
  -- | 'consumerName'
  Types.ConsumerName ->
  RegisterStreamConsumer
mkRegisterStreamConsumer streamARN consumerName =
  RegisterStreamConsumer' {streamARN, consumerName}

-- | The ARN of the Kinesis data stream that you want to register the consumer with. For more info, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscStreamARN :: Lens.Lens' RegisterStreamConsumer Types.StreamARN
rscStreamARN = Lens.field @"streamARN"
{-# DEPRECATED rscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | For a given Kinesis data stream, each consumer must have a unique name. However, consumer names don't have to be unique across data streams.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscConsumerName :: Lens.Lens' RegisterStreamConsumer Types.ConsumerName
rscConsumerName = Lens.field @"consumerName"
{-# DEPRECATED rscConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

instance Core.FromJSON RegisterStreamConsumer where
  toJSON RegisterStreamConsumer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamARN" Core..= streamARN),
            Core.Just ("ConsumerName" Core..= consumerName)
          ]
      )

instance Core.AWSRequest RegisterStreamConsumer where
  type Rs RegisterStreamConsumer = RegisterStreamConsumerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Kinesis_20131202.RegisterStreamConsumer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterStreamConsumerResponse'
            Core.<$> (x Core..: "Consumer") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterStreamConsumerResponse' smart constructor.
data RegisterStreamConsumerResponse = RegisterStreamConsumerResponse'
  { -- | An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
    consumer :: Types.Consumer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RegisterStreamConsumerResponse' value with any optional fields omitted.
mkRegisterStreamConsumerResponse ::
  -- | 'consumer'
  Types.Consumer ->
  -- | 'responseStatus'
  Core.Int ->
  RegisterStreamConsumerResponse
mkRegisterStreamConsumerResponse consumer responseStatus =
  RegisterStreamConsumerResponse' {consumer, responseStatus}

-- | An object that represents the details of the consumer you registered. When you register a consumer, it gets an ARN that is generated by Kinesis Data Streams.
--
-- /Note:/ Consider using 'consumer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscrrsConsumer :: Lens.Lens' RegisterStreamConsumerResponse Types.Consumer
rscrrsConsumer = Lens.field @"consumer"
{-# DEPRECATED rscrrsConsumer "Use generic-lens or generic-optics with 'consumer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rscrrsResponseStatus :: Lens.Lens' RegisterStreamConsumerResponse Core.Int
rscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
