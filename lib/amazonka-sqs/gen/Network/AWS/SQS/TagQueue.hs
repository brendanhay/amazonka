{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.TagQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- When you use queue tags, keep the following guidelines in mind:
--
--     * Adding more than 50 tags to a queue isn't recommended.
--
--
--     * Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.
--
--
--     * Tags are case-sensitive.
--
--
--     * A new tag with a key identical to that of an existing tag overwrites the existing tag.
--
--
-- For a full list of tag restrictions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Limits Related to Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.TagQueue
  ( -- * Creating a request
    TagQueue (..),
    mkTagQueue,

    -- ** Request lenses
    tqQueueURL,
    tqTags,

    -- * Destructuring the response
    TagQueueResponse (..),
    mkTagQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- | /See:/ 'mkTagQueue' smart constructor.
data TagQueue = TagQueue'
  { -- | The URL of the queue.
    queueURL :: Lude.Text,
    -- | The list of tags to be added to the specified queue.
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagQueue' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the queue.
-- * 'tags' - The list of tags to be added to the specified queue.
mkTagQueue ::
  -- | 'queueURL'
  Lude.Text ->
  TagQueue
mkTagQueue pQueueURL_ =
  TagQueue' {queueURL = pQueueURL_, tags = Lude.mempty}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tqQueueURL :: Lens.Lens' TagQueue Lude.Text
tqQueueURL = Lens.lens (queueURL :: TagQueue -> Lude.Text) (\s a -> s {queueURL = a} :: TagQueue)
{-# DEPRECATED tqQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | The list of tags to be added to the specified queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tqTags :: Lens.Lens' TagQueue (Lude.HashMap Lude.Text (Lude.Text))
tqTags = Lens.lens (tags :: TagQueue -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagQueue)
{-# DEPRECATED tqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagQueue where
  type Rs TagQueue = TagQueueResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull TagQueueResponse'

instance Lude.ToHeaders TagQueue where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TagQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery TagQueue where
  toQuery TagQueue' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("TagQueue" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL,
        Lude.toQueryMap "Tags" "Key" "Value" tags
      ]

-- | /See:/ 'mkTagQueueResponse' smart constructor.
data TagQueueResponse = TagQueueResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagQueueResponse' with the minimum fields required to make a request.
mkTagQueueResponse ::
  TagQueueResponse
mkTagQueueResponse = TagQueueResponse'
