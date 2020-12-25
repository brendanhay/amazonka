{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.TagDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified delivery stream. A tag is a key-value pair that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- Each delivery stream can have up to 50 tags.
-- This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.TagDeliveryStream
  ( -- * Creating a request
    TagDeliveryStream (..),
    mkTagDeliveryStream,

    -- ** Request lenses
    tdsDeliveryStreamName,
    tdsTags,

    -- * Destructuring the response
    TagDeliveryStreamResponse (..),
    mkTagDeliveryStreamResponse,

    -- ** Response lenses
    tdsrrsResponseStatus,
  )
where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTagDeliveryStream' smart constructor.
data TagDeliveryStream = TagDeliveryStream'
  { -- | The name of the delivery stream to which you want to add the tags.
    deliveryStreamName :: Types.DeliveryStreamName,
    -- | A set of key-value pairs to use to create the tags.
    tags :: Core.NonEmpty Types.Tag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagDeliveryStream' value with any optional fields omitted.
mkTagDeliveryStream ::
  -- | 'deliveryStreamName'
  Types.DeliveryStreamName ->
  -- | 'tags'
  Core.NonEmpty Types.Tag ->
  TagDeliveryStream
mkTagDeliveryStream deliveryStreamName tags =
  TagDeliveryStream' {deliveryStreamName, tags}

-- | The name of the delivery stream to which you want to add the tags.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsDeliveryStreamName :: Lens.Lens' TagDeliveryStream Types.DeliveryStreamName
tdsDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# DEPRECATED tdsDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | A set of key-value pairs to use to create the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsTags :: Lens.Lens' TagDeliveryStream (Core.NonEmpty Types.Tag)
tdsTags = Lens.field @"tags"
{-# DEPRECATED tdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON TagDeliveryStream where
  toJSON TagDeliveryStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest TagDeliveryStream where
  type Rs TagDeliveryStream = TagDeliveryStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Firehose_20150804.TagDeliveryStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagDeliveryStreamResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTagDeliveryStreamResponse' smart constructor.
newtype TagDeliveryStreamResponse = TagDeliveryStreamResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TagDeliveryStreamResponse' value with any optional fields omitted.
mkTagDeliveryStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TagDeliveryStreamResponse
mkTagDeliveryStreamResponse responseStatus =
  TagDeliveryStreamResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdsrrsResponseStatus :: Lens.Lens' TagDeliveryStreamResponse Core.Int
tdsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tdsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
