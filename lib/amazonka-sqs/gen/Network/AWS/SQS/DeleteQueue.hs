{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.DeleteQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queue specified by the @QueueUrl@ , regardless of the queue's contents.
--
-- /Important:/ Be careful with the @DeleteQueue@ action: When you delete a queue, any messages in the queue are no longer available.
-- When you delete a queue, the deletion process takes up to 60 seconds. Requests you send involving that queue during the 60 seconds might succeed. For example, a @'SendMessage' @ request might succeed, but after 60 seconds the queue and the message you sent no longer exist.
-- When you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.
module Network.AWS.SQS.DeleteQueue
  ( -- * Creating a request
    DeleteQueue (..),
    mkDeleteQueue,

    -- ** Request lenses
    dqQueueUrl,

    -- * Destructuring the response
    DeleteQueueResponse (..),
    mkDeleteQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkDeleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
  { -- | The URL of the Amazon SQS queue to delete.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueue' value with any optional fields omitted.
mkDeleteQueue ::
  -- | 'queueUrl'
  Types.String ->
  DeleteQueue
mkDeleteQueue queueUrl = DeleteQueue' {queueUrl}

-- | The URL of the Amazon SQS queue to delete.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqQueueUrl :: Lens.Lens' DeleteQueue Types.String
dqQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED dqQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

instance Core.AWSRequest DeleteQueue where
  type Rs DeleteQueue = DeleteQueueResponse
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
            ( Core.pure ("Action", "DeleteQueue")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
            )
      }
  response = Response.receiveNull DeleteQueueResponse'

-- | /See:/ 'mkDeleteQueueResponse' smart constructor.
data DeleteQueueResponse = DeleteQueueResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueueResponse' value with any optional fields omitted.
mkDeleteQueueResponse ::
  DeleteQueueResponse
mkDeleteQueueResponse = DeleteQueueResponse'
