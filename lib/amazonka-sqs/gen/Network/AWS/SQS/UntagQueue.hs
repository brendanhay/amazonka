{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.UntagQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove cost allocation tags from the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.UntagQueue
  ( -- * Creating a request
    UntagQueue (..),
    mkUntagQueue,

    -- ** Request lenses
    uqTagKeys,
    uqQueueURL,

    -- * Destructuring the response
    UntagQueueResponse (..),
    mkUntagQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- | /See:/ 'mkUntagQueue' smart constructor.
data UntagQueue = UntagQueue'
  { -- | The list of tags to be removed from the specified queue.
    tagKeys :: [Lude.Text],
    -- | The URL of the queue.
    queueURL :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagQueue' with the minimum fields required to make a request.
--
-- * 'tagKeys' - The list of tags to be removed from the specified queue.
-- * 'queueURL' - The URL of the queue.
mkUntagQueue ::
  -- | 'queueURL'
  Lude.Text ->
  UntagQueue
mkUntagQueue pQueueURL_ =
  UntagQueue' {tagKeys = Lude.mempty, queueURL = pQueueURL_}

-- | The list of tags to be removed from the specified queue.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqTagKeys :: Lens.Lens' UntagQueue [Lude.Text]
uqTagKeys = Lens.lens (tagKeys :: UntagQueue -> [Lude.Text]) (\s a -> s {tagKeys = a} :: UntagQueue)
{-# DEPRECATED uqTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqQueueURL :: Lens.Lens' UntagQueue Lude.Text
uqQueueURL = Lens.lens (queueURL :: UntagQueue -> Lude.Text) (\s a -> s {queueURL = a} :: UntagQueue)
{-# DEPRECATED uqQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

instance Lude.AWSRequest UntagQueue where
  type Rs UntagQueue = UntagQueueResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull UntagQueueResponse'

instance Lude.ToHeaders UntagQueue where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UntagQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagQueue where
  toQuery UntagQueue' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UntagQueue" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        Lude.toQueryList "TagKey" tagKeys,
        "QueueUrl" Lude.=: queueURL
      ]

-- | /See:/ 'mkUntagQueueResponse' smart constructor.
data UntagQueueResponse = UntagQueueResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagQueueResponse' with the minimum fields required to make a request.
mkUntagQueueResponse ::
  UntagQueueResponse
mkUntagQueueResponse = UntagQueueResponse'
