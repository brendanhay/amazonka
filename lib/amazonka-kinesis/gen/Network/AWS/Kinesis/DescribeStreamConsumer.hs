{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dConsumerName,
    dStreamARN,

    -- * Destructuring the response
    DescribeStreamConsumerResponse (..),
    mkDescribeStreamConsumerResponse,

    -- ** Response lenses
    dscrrsConsumerDescription,
    dscrrsResponseStatus,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStreamConsumer' smart constructor.
data DescribeStreamConsumer = DescribeStreamConsumer'
  { -- | The ARN returned by Kinesis Data Streams when you registered the consumer.
    consumerARN :: Core.Maybe Types.ConsumerARN,
    -- | The name that you gave to the consumer.
    consumerName :: Core.Maybe Types.ConsumerName,
    -- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    streamARN :: Core.Maybe Types.StreamARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStreamConsumer' value with any optional fields omitted.
mkDescribeStreamConsumer ::
  DescribeStreamConsumer
mkDescribeStreamConsumer =
  DescribeStreamConsumer'
    { consumerARN = Core.Nothing,
      consumerName = Core.Nothing,
      streamARN = Core.Nothing
    }

-- | The ARN returned by Kinesis Data Streams when you registered the consumer.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConsumerARN :: Lens.Lens' DescribeStreamConsumer (Core.Maybe Types.ConsumerARN)
dConsumerARN = Lens.field @"consumerARN"
{-# DEPRECATED dConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | The name that you gave to the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dConsumerName :: Lens.Lens' DescribeStreamConsumer (Core.Maybe Types.ConsumerName)
dConsumerName = Lens.field @"consumerName"
{-# DEPRECATED dConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

-- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamARN :: Lens.Lens' DescribeStreamConsumer (Core.Maybe Types.StreamARN)
dStreamARN = Lens.field @"streamARN"
{-# DEPRECATED dStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

instance Core.FromJSON DescribeStreamConsumer where
  toJSON DescribeStreamConsumer {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConsumerARN" Core..=) Core.<$> consumerARN,
            ("ConsumerName" Core..=) Core.<$> consumerName,
            ("StreamARN" Core..=) Core.<$> streamARN
          ]
      )

instance Core.AWSRequest DescribeStreamConsumer where
  type Rs DescribeStreamConsumer = DescribeStreamConsumerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Kinesis_20131202.DescribeStreamConsumer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStreamConsumerResponse'
            Core.<$> (x Core..: "ConsumerDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStreamConsumerResponse' smart constructor.
data DescribeStreamConsumerResponse = DescribeStreamConsumerResponse'
  { -- | An object that represents the details of the consumer.
    consumerDescription :: Types.ConsumerDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStreamConsumerResponse' value with any optional fields omitted.
mkDescribeStreamConsumerResponse ::
  -- | 'consumerDescription'
  Types.ConsumerDescription ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeStreamConsumerResponse
mkDescribeStreamConsumerResponse consumerDescription responseStatus =
  DescribeStreamConsumerResponse'
    { consumerDescription,
      responseStatus
    }

-- | An object that represents the details of the consumer.
--
-- /Note:/ Consider using 'consumerDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsConsumerDescription :: Lens.Lens' DescribeStreamConsumerResponse Types.ConsumerDescription
dscrrsConsumerDescription = Lens.field @"consumerDescription"
{-# DEPRECATED dscrrsConsumerDescription "Use generic-lens or generic-optics with 'consumerDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrrsResponseStatus :: Lens.Lens' DescribeStreamConsumerResponse Core.Int
dscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
