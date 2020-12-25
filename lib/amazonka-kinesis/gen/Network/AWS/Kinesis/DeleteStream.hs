{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis data stream and all its shards and data. You must shut down any applications that are operating on the stream before you delete the stream. If an application attempts to operate on a deleted stream, it receives the exception @ResourceNotFoundException@ .
--
-- If the stream is in the @ACTIVE@ state, you can delete it. After a @DeleteStream@ request, the specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.
-- __Note:__ Kinesis Data Streams might continue to accept data read and write operations, such as 'PutRecord' , 'PutRecords' , and 'GetRecords' , on a stream in the @DELETING@ state until the stream deletion is complete.
-- When you delete a stream, any shards in that stream are also deleted, and any tags are dissociated from the stream.
-- You can use the 'DescribeStream' operation to check the state of the stream, which is returned in @StreamStatus@ .
-- 'DeleteStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.DeleteStream
  ( -- * Creating a request
    DeleteStream (..),
    mkDeleteStream,

    -- ** Request lenses
    dsStreamName,
    dsEnforceConsumerDeletion,

    -- * Destructuring the response
    DeleteStreamResponse (..),
    mkDeleteStreamResponse,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for 'DeleteStream' .
--
-- /See:/ 'mkDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | The name of the stream to delete.
    streamName :: Types.StreamName,
    -- | If this parameter is unset (@null@ ) or if you set it to @false@ , and the stream has registered consumers, the call to @DeleteStream@ fails with a @ResourceInUseException@ .
    enforceConsumerDeletion :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStream' value with any optional fields omitted.
mkDeleteStream ::
  -- | 'streamName'
  Types.StreamName ->
  DeleteStream
mkDeleteStream streamName =
  DeleteStream' {streamName, enforceConsumerDeletion = Core.Nothing}

-- | The name of the stream to delete.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamName :: Lens.Lens' DeleteStream Types.StreamName
dsStreamName = Lens.field @"streamName"
{-# DEPRECATED dsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | If this parameter is unset (@null@ ) or if you set it to @false@ , and the stream has registered consumers, the call to @DeleteStream@ fails with a @ResourceInUseException@ .
--
-- /Note:/ Consider using 'enforceConsumerDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsEnforceConsumerDeletion :: Lens.Lens' DeleteStream (Core.Maybe Core.Bool)
dsEnforceConsumerDeletion = Lens.field @"enforceConsumerDeletion"
{-# DEPRECATED dsEnforceConsumerDeletion "Use generic-lens or generic-optics with 'enforceConsumerDeletion' instead." #-}

instance Core.FromJSON DeleteStream where
  toJSON DeleteStream {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            ("EnforceConsumerDeletion" Core..=)
              Core.<$> enforceConsumerDeletion
          ]
      )

instance Core.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Kinesis_20131202.DeleteStream")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteStreamResponse'

-- | /See:/ 'mkDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamResponse' value with any optional fields omitted.
mkDeleteStreamResponse ::
  DeleteStreamResponse
mkDeleteStreamResponse = DeleteStreamResponse'
