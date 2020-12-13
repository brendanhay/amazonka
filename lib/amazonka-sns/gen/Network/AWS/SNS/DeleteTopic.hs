{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic and all its subscriptions. Deleting a topic might prevent some messages previously sent to the topic from being delivered to subscribers. This action is idempotent, so deleting a topic that does not exist does not result in an error.
module Network.AWS.SNS.DeleteTopic
  ( -- * Creating a request
    DeleteTopic (..),
    mkDeleteTopic,

    -- ** Request lenses
    dtTopicARN,

    -- * Destructuring the response
    DeleteTopicResponse (..),
    mkDeleteTopicResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | /See:/ 'mkDeleteTopic' smart constructor.
newtype DeleteTopic = DeleteTopic'
  { -- | The ARN of the topic you want to delete.
    topicARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTopic' with the minimum fields required to make a request.
--
-- * 'topicARN' - The ARN of the topic you want to delete.
mkDeleteTopic ::
  -- | 'topicARN'
  Lude.Text ->
  DeleteTopic
mkDeleteTopic pTopicARN_ = DeleteTopic' {topicARN = pTopicARN_}

-- | The ARN of the topic you want to delete.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTopicARN :: Lens.Lens' DeleteTopic Lude.Text
dtTopicARN = Lens.lens (topicARN :: DeleteTopic -> Lude.Text) (\s a -> s {topicARN = a} :: DeleteTopic)
{-# DEPRECATED dtTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

instance Lude.AWSRequest DeleteTopic where
  type Rs DeleteTopic = DeleteTopicResponse
  request = Req.postQuery snsService
  response = Res.receiveNull DeleteTopicResponse'

instance Lude.ToHeaders DeleteTopic where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTopic where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTopic where
  toQuery DeleteTopic' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTopic" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "TopicArn" Lude.=: topicARN
      ]

-- | /See:/ 'mkDeleteTopicResponse' smart constructor.
data DeleteTopicResponse = DeleteTopicResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTopicResponse' with the minimum fields required to make a request.
mkDeleteTopicResponse ::
  DeleteTopicResponse
mkDeleteTopicResponse = DeleteTopicResponse'
