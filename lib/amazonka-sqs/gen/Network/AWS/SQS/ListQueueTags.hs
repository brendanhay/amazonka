{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListQueueTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all cost allocation tags added to the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.ListQueueTags
  ( -- * Creating a request
    ListQueueTags (..),
    mkListQueueTags,

    -- ** Request lenses
    lqtQueueURL,

    -- * Destructuring the response
    ListQueueTagsResponse (..),
    mkListQueueTagsResponse,

    -- ** Response lenses
    lqtrsTags,
    lqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- | /See:/ 'mkListQueueTags' smart constructor.
newtype ListQueueTags = ListQueueTags'
  { -- | The URL of the queue.
    queueURL :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueueTags' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the queue.
mkListQueueTags ::
  -- | 'queueURL'
  Lude.Text ->
  ListQueueTags
mkListQueueTags pQueueURL_ = ListQueueTags' {queueURL = pQueueURL_}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtQueueURL :: Lens.Lens' ListQueueTags Lude.Text
lqtQueueURL = Lens.lens (queueURL :: ListQueueTags -> Lude.Text) (\s a -> s {queueURL = a} :: ListQueueTags)
{-# DEPRECATED lqtQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

instance Lude.AWSRequest ListQueueTags where
  type Rs ListQueueTags = ListQueueTagsResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "ListQueueTagsResult"
      ( \s h x ->
          ListQueueTagsResponse'
            Lude.<$> (Lude.may (Lude.parseXMLMap "Tag" "Key" "Value") x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListQueueTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListQueueTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListQueueTags where
  toQuery ListQueueTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListQueueTags" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL
      ]

-- | /See:/ 'mkListQueueTagsResponse' smart constructor.
data ListQueueTagsResponse = ListQueueTagsResponse'
  { -- | The list of all tags added to the specified queue.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListQueueTagsResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The list of all tags added to the specified queue.
-- * 'responseStatus' - The response status code.
mkListQueueTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListQueueTagsResponse
mkListQueueTagsResponse pResponseStatus_ =
  ListQueueTagsResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of all tags added to the specified queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrsTags :: Lens.Lens' ListQueueTagsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
lqtrsTags = Lens.lens (tags :: ListQueueTagsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListQueueTagsResponse)
{-# DEPRECATED lqtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrsResponseStatus :: Lens.Lens' ListQueueTagsResponse Lude.Int
lqtrsResponseStatus = Lens.lens (responseStatus :: ListQueueTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListQueueTagsResponse)
{-# DEPRECATED lqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
