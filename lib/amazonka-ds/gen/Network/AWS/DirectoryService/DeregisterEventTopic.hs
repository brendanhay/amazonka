{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeregisterEventTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified directory as a publisher to the specified SNS topic.
module Network.AWS.DirectoryService.DeregisterEventTopic
  ( -- * Creating a request
    DeregisterEventTopic (..),
    mkDeregisterEventTopic,

    -- ** Request lenses
    detDirectoryId,
    detTopicName,

    -- * Destructuring the response
    DeregisterEventTopicResponse (..),
    mkDeregisterEventTopicResponse,

    -- ** Response lenses
    derrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Removes the specified directory as a publisher to the specified SNS topic.
--
-- /See:/ 'mkDeregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { directoryId ::
      Lude.Text,
    topicName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterEventTopic' with the minimum fields required to make a request.
--
-- * 'directoryId' - The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
-- * 'topicName' - The name of the SNS topic from which to remove the directory as a publisher.
mkDeregisterEventTopic ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'topicName'
  Lude.Text ->
  DeregisterEventTopic
mkDeregisterEventTopic pDirectoryId_ pTopicName_ =
  DeregisterEventTopic'
    { directoryId = pDirectoryId_,
      topicName = pTopicName_
    }

-- | The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detDirectoryId :: Lens.Lens' DeregisterEventTopic Lude.Text
detDirectoryId = Lens.lens (directoryId :: DeregisterEventTopic -> Lude.Text) (\s a -> s {directoryId = a} :: DeregisterEventTopic)
{-# DEPRECATED detDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the SNS topic from which to remove the directory as a publisher.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTopicName :: Lens.Lens' DeregisterEventTopic Lude.Text
detTopicName = Lens.lens (topicName :: DeregisterEventTopic -> Lude.Text) (\s a -> s {topicName = a} :: DeregisterEventTopic)
{-# DEPRECATED detTopicName "Use generic-lens or generic-optics with 'topicName' instead." #-}

instance Lude.AWSRequest DeregisterEventTopic where
  type Rs DeregisterEventTopic = DeregisterEventTopicResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeregisterEventTopicResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterEventTopic where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DeregisterEventTopic" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterEventTopic where
  toJSON DeregisterEventTopic' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("TopicName" Lude..= topicName)
          ]
      )

instance Lude.ToPath DeregisterEventTopic where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterEventTopic where
  toQuery = Lude.const Lude.mempty

-- | The result of a DeregisterEventTopic request.
--
-- /See:/ 'mkDeregisterEventTopicResponse' smart constructor.
newtype DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterEventTopicResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterEventTopicResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterEventTopicResponse
mkDeregisterEventTopicResponse pResponseStatus_ =
  DeregisterEventTopicResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeregisterEventTopicResponse Lude.Int
derrsResponseStatus = Lens.lens (responseStatus :: DeregisterEventTopicResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterEventTopicResponse)
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
