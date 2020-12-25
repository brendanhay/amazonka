{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.UntagDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified delivery stream. Removed tags are deleted, and you can't recover them after this operation successfully completes.
--
-- If you specify a tag that doesn't exist, the operation ignores it.
-- This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.UntagDeliveryStream
  ( -- * Creating a request
    UntagDeliveryStream (..),
    mkUntagDeliveryStream,

    -- ** Request lenses
    udsDeliveryStreamName,
    udsTagKeys,

    -- * Destructuring the response
    UntagDeliveryStreamResponse (..),
    mkUntagDeliveryStreamResponse,

    -- ** Response lenses
    udsrrsResponseStatus,
  )
where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagDeliveryStream' smart constructor.
data UntagDeliveryStream = UntagDeliveryStream'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Types.DeliveryStreamName,
    -- | A list of tag keys. Each corresponding tag is removed from the delivery stream.
    tagKeys :: Core.NonEmpty Types.TagKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagDeliveryStream' value with any optional fields omitted.
mkUntagDeliveryStream ::
  -- | 'deliveryStreamName'
  Types.DeliveryStreamName ->
  -- | 'tagKeys'
  Core.NonEmpty Types.TagKey ->
  UntagDeliveryStream
mkUntagDeliveryStream deliveryStreamName tagKeys =
  UntagDeliveryStream' {deliveryStreamName, tagKeys}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsDeliveryStreamName :: Lens.Lens' UntagDeliveryStream Types.DeliveryStreamName
udsDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# DEPRECATED udsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | A list of tag keys. Each corresponding tag is removed from the delivery stream.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsTagKeys :: Lens.Lens' UntagDeliveryStream (Core.NonEmpty Types.TagKey)
udsTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED udsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON UntagDeliveryStream where
  toJSON UntagDeliveryStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest UntagDeliveryStream where
  type Rs UntagDeliveryStream = UntagDeliveryStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Firehose_20150804.UntagDeliveryStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagDeliveryStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUntagDeliveryStreamResponse' smart constructor.
newtype UntagDeliveryStreamResponse = UntagDeliveryStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UntagDeliveryStreamResponse' value with any optional fields omitted.
mkUntagDeliveryStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UntagDeliveryStreamResponse
mkUntagDeliveryStreamResponse responseStatus =
  UntagDeliveryStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrrsResponseStatus :: Lens.Lens' UntagDeliveryStreamResponse Core.Int
udsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
