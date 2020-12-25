{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.ListDeliveryStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your delivery streams in alphabetical order of their names.
--
-- The number of delivery streams might be too large to return using a single call to @ListDeliveryStreams@ . You can limit the number of delivery streams returned, using the @Limit@ parameter. To determine whether there are more delivery streams to list, check the value of @HasMoreDeliveryStreams@ in the output. If there are more delivery streams to list, you can request them by calling this operation again and setting the @ExclusiveStartDeliveryStreamName@ parameter to the name of the last delivery stream returned in the last call.
module Network.AWS.Firehose.ListDeliveryStreams
  ( -- * Creating a request
    ListDeliveryStreams (..),
    mkListDeliveryStreams,

    -- ** Request lenses
    ldsDeliveryStreamType,
    ldsExclusiveStartDeliveryStreamName,
    ldsLimit,

    -- * Destructuring the response
    ListDeliveryStreamsResponse (..),
    mkListDeliveryStreamsResponse,

    -- ** Response lenses
    ldsrrsDeliveryStreamNames,
    ldsrrsHasMoreDeliveryStreams,
    ldsrrsResponseStatus,
  )
where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeliveryStreams' smart constructor.
data ListDeliveryStreams = ListDeliveryStreams'
  { -- | The delivery stream type. This can be one of the following values:
    --
    --
    --     * @DirectPut@ : Provider applications access the delivery stream directly.
    --
    --
    --     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
    --
    --
    -- This parameter is optional. If this parameter is omitted, delivery streams of all types are returned.
    deliveryStreamType :: Core.Maybe Types.DeliveryStreamType,
    -- | The list of delivery streams returned by this call to @ListDeliveryStreams@ will start with the delivery stream whose name comes alphabetically immediately after the name you specify in @ExclusiveStartDeliveryStreamName@ .
    exclusiveStartDeliveryStreamName :: Core.Maybe Types.ExclusiveStartDeliveryStreamName,
    -- | The maximum number of delivery streams to list. The default value is 10.
    limit :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeliveryStreams' value with any optional fields omitted.
mkListDeliveryStreams ::
  ListDeliveryStreams
mkListDeliveryStreams =
  ListDeliveryStreams'
    { deliveryStreamType = Core.Nothing,
      exclusiveStartDeliveryStreamName = Core.Nothing,
      limit = Core.Nothing
    }

-- | The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
-- This parameter is optional. If this parameter is omitted, delivery streams of all types are returned.
--
-- /Note:/ Consider using 'deliveryStreamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsDeliveryStreamType :: Lens.Lens' ListDeliveryStreams (Core.Maybe Types.DeliveryStreamType)
ldsDeliveryStreamType = Lens.field @"deliveryStreamType"
{-# DEPRECATED ldsDeliveryStreamType "Use generic-lens or generic-optics with 'deliveryStreamType' instead." #-}

-- | The list of delivery streams returned by this call to @ListDeliveryStreams@ will start with the delivery stream whose name comes alphabetically immediately after the name you specify in @ExclusiveStartDeliveryStreamName@ .
--
-- /Note:/ Consider using 'exclusiveStartDeliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsExclusiveStartDeliveryStreamName :: Lens.Lens' ListDeliveryStreams (Core.Maybe Types.ExclusiveStartDeliveryStreamName)
ldsExclusiveStartDeliveryStreamName = Lens.field @"exclusiveStartDeliveryStreamName"
{-# DEPRECATED ldsExclusiveStartDeliveryStreamName "Use generic-lens or generic-optics with 'exclusiveStartDeliveryStreamName' instead." #-}

-- | The maximum number of delivery streams to list. The default value is 10.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsLimit :: Lens.Lens' ListDeliveryStreams (Core.Maybe Core.Natural)
ldsLimit = Lens.field @"limit"
{-# DEPRECATED ldsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Core.FromJSON ListDeliveryStreams where
  toJSON ListDeliveryStreams {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeliveryStreamType" Core..=) Core.<$> deliveryStreamType,
            ("ExclusiveStartDeliveryStreamName" Core..=)
              Core.<$> exclusiveStartDeliveryStreamName,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.AWSRequest ListDeliveryStreams where
  type Rs ListDeliveryStreams = ListDeliveryStreamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Firehose_20150804.ListDeliveryStreams")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeliveryStreamsResponse'
            Core.<$> (x Core..:? "DeliveryStreamNames" Core..!= Core.mempty)
            Core.<*> (x Core..: "HasMoreDeliveryStreams")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListDeliveryStreamsResponse' smart constructor.
data ListDeliveryStreamsResponse = ListDeliveryStreamsResponse'
  { -- | The names of the delivery streams.
    deliveryStreamNames :: [Types.DeliveryStreamName],
    -- | Indicates whether there are more delivery streams available to list.
    hasMoreDeliveryStreams :: Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeliveryStreamsResponse' value with any optional fields omitted.
mkListDeliveryStreamsResponse ::
  -- | 'hasMoreDeliveryStreams'
  Core.Bool ->
  -- | 'responseStatus'
  Core.Int ->
  ListDeliveryStreamsResponse
mkListDeliveryStreamsResponse hasMoreDeliveryStreams responseStatus =
  ListDeliveryStreamsResponse'
    { deliveryStreamNames = Core.mempty,
      hasMoreDeliveryStreams,
      responseStatus
    }

-- | The names of the delivery streams.
--
-- /Note:/ Consider using 'deliveryStreamNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrrsDeliveryStreamNames :: Lens.Lens' ListDeliveryStreamsResponse [Types.DeliveryStreamName]
ldsrrsDeliveryStreamNames = Lens.field @"deliveryStreamNames"
{-# DEPRECATED ldsrrsDeliveryStreamNames "Use generic-lens or generic-optics with 'deliveryStreamNames' instead." #-}

-- | Indicates whether there are more delivery streams available to list.
--
-- /Note:/ Consider using 'hasMoreDeliveryStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrrsHasMoreDeliveryStreams :: Lens.Lens' ListDeliveryStreamsResponse Core.Bool
ldsrrsHasMoreDeliveryStreams = Lens.field @"hasMoreDeliveryStreams"
{-# DEPRECATED ldsrrsHasMoreDeliveryStreams "Use generic-lens or generic-optics with 'hasMoreDeliveryStreams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsrrsResponseStatus :: Lens.Lens' ListDeliveryStreamsResponse Core.Int
ldsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
