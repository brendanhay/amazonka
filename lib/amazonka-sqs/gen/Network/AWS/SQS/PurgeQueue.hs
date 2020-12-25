{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.PurgeQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the messages in a queue specified by the @QueueURL@ parameter.
--
-- /Important:/ When you use the @PurgeQueue@ action, you can't retrieve any messages deleted from a queue.
-- The message deletion process takes up to 60 seconds. We recommend waiting for 60 seconds regardless of your queue's size.
-- Messages sent to the queue /before/ you call @PurgeQueue@ might be received but are deleted within the next minute.
-- Messages sent to the queue /after/ you call @PurgeQueue@ might be deleted while the queue is being purged.
module Network.AWS.SQS.PurgeQueue
  ( -- * Creating a request
    PurgeQueue (..),
    mkPurgeQueue,

    -- ** Request lenses
    pqQueueUrl,

    -- * Destructuring the response
    PurgeQueueResponse (..),
    mkPurgeQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkPurgeQueue' smart constructor.
newtype PurgeQueue = PurgeQueue'
  { -- | The URL of the queue from which the @PurgeQueue@ action deletes messages.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PurgeQueue' value with any optional fields omitted.
mkPurgeQueue ::
  -- | 'queueUrl'
  Types.String ->
  PurgeQueue
mkPurgeQueue queueUrl = PurgeQueue' {queueUrl}

-- | The URL of the queue from which the @PurgeQueue@ action deletes messages.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqQueueUrl :: Lens.Lens' PurgeQueue Types.String
pqQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED pqQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

instance Core.AWSRequest PurgeQueue where
  type Rs PurgeQueue = PurgeQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PurgeQueue")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
            )
      }
  response = Response.receiveNull PurgeQueueResponse'

-- | /See:/ 'mkPurgeQueueResponse' smart constructor.
data PurgeQueueResponse = PurgeQueueResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurgeQueueResponse' value with any optional fields omitted.
mkPurgeQueueResponse ::
  PurgeQueueResponse
mkPurgeQueueResponse = PurgeQueueResponse'
