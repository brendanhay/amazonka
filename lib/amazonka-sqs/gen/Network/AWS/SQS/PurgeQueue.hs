{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    pqQueueURL,

    -- * Destructuring the response
    PurgeQueueResponse (..),
    mkPurgeQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkPurgeQueue' smart constructor.
newtype PurgeQueue = PurgeQueue' {queueURL :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurgeQueue' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the queue from which the @PurgeQueue@ action deletes messages.
--
-- Queue URLs and names are case-sensitive.
mkPurgeQueue ::
  -- | 'queueURL'
  Lude.Text ->
  PurgeQueue
mkPurgeQueue pQueueURL_ = PurgeQueue' {queueURL = pQueueURL_}

-- | The URL of the queue from which the @PurgeQueue@ action deletes messages.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pqQueueURL :: Lens.Lens' PurgeQueue Lude.Text
pqQueueURL = Lens.lens (queueURL :: PurgeQueue -> Lude.Text) (\s a -> s {queueURL = a} :: PurgeQueue)
{-# DEPRECATED pqQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

instance Lude.AWSRequest PurgeQueue where
  type Rs PurgeQueue = PurgeQueueResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull PurgeQueueResponse'

instance Lude.ToHeaders PurgeQueue where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurgeQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery PurgeQueue where
  toQuery PurgeQueue' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PurgeQueue" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL
      ]

-- | /See:/ 'mkPurgeQueueResponse' smart constructor.
data PurgeQueueResponse = PurgeQueueResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurgeQueueResponse' with the minimum fields required to make a request.
mkPurgeQueueResponse ::
  PurgeQueueResponse
mkPurgeQueueResponse = PurgeQueueResponse'
