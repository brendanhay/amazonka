{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dscConsumerName,
    dscStreamARN,

    -- * Destructuring the response
    DeregisterStreamConsumerResponse (..),
    mkDeregisterStreamConsumerResponse,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterStreamConsumer' smart constructor.
data DeregisterStreamConsumer = DeregisterStreamConsumer'
  { -- | The ARN returned by Kinesis Data Streams when you registered the consumer. If you don't know the ARN of the consumer that you want to deregister, you can use the ListStreamConsumers operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its ARN.
    consumerARN :: Core.Maybe Types.ConsumerARN,
    -- | The name that you gave to the consumer.
    consumerName :: Core.Maybe Types.ConsumerName,
    -- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    streamARN :: Core.Maybe Types.StreamARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterStreamConsumer' value with any optional fields omitted.
mkDeregisterStreamConsumer ::
  DeregisterStreamConsumer
mkDeregisterStreamConsumer =
  DeregisterStreamConsumer'
    { consumerARN = Core.Nothing,
      consumerName = Core.Nothing,
      streamARN = Core.Nothing
    }

-- | The ARN returned by Kinesis Data Streams when you registered the consumer. If you don't know the ARN of the consumer that you want to deregister, you can use the ListStreamConsumers operation to get a list of the descriptions of all the consumers that are currently registered with a given data stream. The description of a consumer contains its ARN.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscConsumerARN :: Lens.Lens' DeregisterStreamConsumer (Core.Maybe Types.ConsumerARN)
dscConsumerARN = Lens.field @"consumerARN"
{-# DEPRECATED dscConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | The name that you gave to the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscConsumerName :: Lens.Lens' DeregisterStreamConsumer (Core.Maybe Types.ConsumerName)
dscConsumerName = Lens.field @"consumerName"
{-# DEPRECATED dscConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

-- | The ARN of the Kinesis data stream that the consumer is registered with. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscStreamARN :: Lens.Lens' DeregisterStreamConsumer (Core.Maybe Types.StreamARN)
dscStreamARN = Lens.field @"streamARN"
{-# DEPRECATED dscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

instance Core.FromJSON DeregisterStreamConsumer where
  toJSON DeregisterStreamConsumer {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConsumerARN" Core..=) Core.<$> consumerARN,
            ("ConsumerName" Core..=) Core.<$> consumerName,
            ("StreamARN" Core..=) Core.<$> streamARN
          ]
      )

instance Core.AWSRequest DeregisterStreamConsumer where
  type Rs DeregisterStreamConsumer = DeregisterStreamConsumerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Kinesis_20131202.DeregisterStreamConsumer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeregisterStreamConsumerResponse'

-- | /See:/ 'mkDeregisterStreamConsumerResponse' smart constructor.
data DeregisterStreamConsumerResponse = DeregisterStreamConsumerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterStreamConsumerResponse' value with any optional fields omitted.
mkDeregisterStreamConsumerResponse ::
  DeregisterStreamConsumerResponse
mkDeregisterStreamConsumerResponse =
  DeregisterStreamConsumerResponse'
