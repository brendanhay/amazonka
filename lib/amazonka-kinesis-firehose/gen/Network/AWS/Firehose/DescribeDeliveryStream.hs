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
    ddsDeliveryStreamName,
    ddsExclusiveStartDestinationId,
    ddsLimit,

    -- * Destructuring the response
    DescribeDeliveryStreamResponse (..),
    mkDescribeDeliveryStreamResponse,

    -- ** Response lenses
    ddsrrsDeliveryStreamDescription,
    ddsrrsResponseStatus,
  )
where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDeliveryStream' smart constructor.
data DescribeDeliveryStream = DescribeDeliveryStream'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Types.DeliveryStreamName,
    -- | The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
    exclusiveStartDestinationId :: Core.Maybe Types.ExclusiveStartDestinationId,
    -- | The limit on the number of destinations to return. You can have one destination per delivery stream.
    limit :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDeliveryStream' value with any optional fields omitted.
mkDescribeDeliveryStream ::
  -- | 'deliveryStreamName'
  Types.DeliveryStreamName ->
  DescribeDeliveryStream
mkDescribeDeliveryStream deliveryStreamName =
  DescribeDeliveryStream'
    { deliveryStreamName,
      exclusiveStartDestinationId = Core.Nothing,
      limit = Core.Nothing
    }

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDeliveryStreamName :: Lens.Lens' DescribeDeliveryStream Types.DeliveryStreamName
ddsDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# DEPRECATED ddsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | The ID of the destination to start returning the destination information. Kinesis Data Firehose supports one destination per delivery stream.
--
-- /Note:/ Consider using 'exclusiveStartDestinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsExclusiveStartDestinationId :: Lens.Lens' DescribeDeliveryStream (Core.Maybe Types.ExclusiveStartDestinationId)
ddsExclusiveStartDestinationId = Lens.field @"exclusiveStartDestinationId"
{-# DEPRECATED ddsExclusiveStartDestinationId "Use generic-lens or generic-optics with 'exclusiveStartDestinationId' instead." #-}

-- | The limit on the number of destinations to return. You can have one destination per delivery stream.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsLimit :: Lens.Lens' DescribeDeliveryStream (Core.Maybe Core.Natural)
ddsLimit = Lens.field @"limit"
{-# DEPRECATED ddsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Core.FromJSON DescribeDeliveryStream where
  toJSON DescribeDeliveryStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
            ("ExclusiveStartDestinationId" Core..=)
              Core.<$> exclusiveStartDestinationId,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.AWSRequest DescribeDeliveryStream where
  type Rs DescribeDeliveryStream = DescribeDeliveryStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Firehose_20150804.DescribeDeliveryStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryStreamResponse'
            Core.<$> (x Core..: "DeliveryStreamDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDeliveryStreamResponse' smart constructor.
data DescribeDeliveryStreamResponse = DescribeDeliveryStreamResponse'
  { -- | Information about the delivery stream.
    deliveryStreamDescription :: Types.DeliveryStreamDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDeliveryStreamResponse' value with any optional fields omitted.
mkDescribeDeliveryStreamResponse ::
  -- | 'deliveryStreamDescription'
  Types.DeliveryStreamDescription ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeDeliveryStreamResponse
mkDescribeDeliveryStreamResponse
  deliveryStreamDescription
  responseStatus =
    DescribeDeliveryStreamResponse'
      { deliveryStreamDescription,
        responseStatus
      }

-- | Information about the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsDeliveryStreamDescription :: Lens.Lens' DescribeDeliveryStreamResponse Types.DeliveryStreamDescription
ddsrrsDeliveryStreamDescription = Lens.field @"deliveryStreamDescription"
{-# DEPRECATED ddsrrsDeliveryStreamDescription "Use generic-lens or generic-optics with 'deliveryStreamDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsResponseStatus :: Lens.Lens' DescribeDeliveryStreamResponse Core.Int
ddsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
