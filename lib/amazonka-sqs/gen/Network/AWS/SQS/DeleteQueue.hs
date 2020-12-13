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
    dqQueueURL,

    -- * Destructuring the response
    DeleteQueueResponse (..),
    mkDeleteQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkDeleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
  { -- | The URL of the Amazon SQS queue to delete.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueue' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the Amazon SQS queue to delete.
--
-- Queue URLs and names are case-sensitive.
mkDeleteQueue ::
  -- | 'queueURL'
  Lude.Text ->
  DeleteQueue
mkDeleteQueue pQueueURL_ = DeleteQueue' {queueURL = pQueueURL_}

-- | The URL of the Amazon SQS queue to delete.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqQueueURL :: Lens.Lens' DeleteQueue Lude.Text
dqQueueURL = Lens.lens (queueURL :: DeleteQueue -> Lude.Text) (\s a -> s {queueURL = a} :: DeleteQueue)
{-# DEPRECATED dqQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

instance Lude.AWSRequest DeleteQueue where
  type Rs DeleteQueue = DeleteQueueResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull DeleteQueueResponse'

instance Lude.ToHeaders DeleteQueue where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteQueue where
  toQuery DeleteQueue' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteQueue" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL
      ]

-- | /See:/ 'mkDeleteQueueResponse' smart constructor.
data DeleteQueueResponse = DeleteQueueResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueueResponse' with the minimum fields required to make a request.
mkDeleteQueueResponse ::
  DeleteQueueResponse
mkDeleteQueueResponse = DeleteQueueResponse'
