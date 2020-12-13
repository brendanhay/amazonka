{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RegisterEventTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a directory with an SNS topic. This establishes the directory as a publisher to the specified SNS topic. You can then receive email or text (SMS) messages when the status of your directory changes. You get notified if your directory goes from an Active status to an Impaired or Inoperable status. You also receive a notification when the directory returns to an Active status.
module Network.AWS.DirectoryService.RegisterEventTopic
  ( -- * Creating a request
    RegisterEventTopic (..),
    mkRegisterEventTopic,

    -- ** Request lenses
    retDirectoryId,
    retTopicName,

    -- * Destructuring the response
    RegisterEventTopicResponse (..),
    mkRegisterEventTopicResponse,

    -- ** Response lenses
    retrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Registers a new event topic.
--
-- /See:/ 'mkRegisterEventTopic' smart constructor.
data RegisterEventTopic = RegisterEventTopic'
  { -- | The Directory ID that will publish status messages to the SNS topic.
    directoryId :: Lude.Text,
    -- | The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
    topicName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterEventTopic' with the minimum fields required to make a request.
--
-- * 'directoryId' - The Directory ID that will publish status messages to the SNS topic.
-- * 'topicName' - The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
mkRegisterEventTopic ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'topicName'
  Lude.Text ->
  RegisterEventTopic
mkRegisterEventTopic pDirectoryId_ pTopicName_ =
  RegisterEventTopic'
    { directoryId = pDirectoryId_,
      topicName = pTopicName_
    }

-- | The Directory ID that will publish status messages to the SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
retDirectoryId :: Lens.Lens' RegisterEventTopic Lude.Text
retDirectoryId = Lens.lens (directoryId :: RegisterEventTopic -> Lude.Text) (\s a -> s {directoryId = a} :: RegisterEventTopic)
{-# DEPRECATED retDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
retTopicName :: Lens.Lens' RegisterEventTopic Lude.Text
retTopicName = Lens.lens (topicName :: RegisterEventTopic -> Lude.Text) (\s a -> s {topicName = a} :: RegisterEventTopic)
{-# DEPRECATED retTopicName "Use generic-lens or generic-optics with 'topicName' instead." #-}

instance Lude.AWSRequest RegisterEventTopic where
  type Rs RegisterEventTopic = RegisterEventTopicResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RegisterEventTopicResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterEventTopic where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.RegisterEventTopic" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterEventTopic where
  toJSON RegisterEventTopic' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("TopicName" Lude..= topicName)
          ]
      )

instance Lude.ToPath RegisterEventTopic where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterEventTopic where
  toQuery = Lude.const Lude.mempty

-- | The result of a RegisterEventTopic request.
--
-- /See:/ 'mkRegisterEventTopicResponse' smart constructor.
newtype RegisterEventTopicResponse = RegisterEventTopicResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterEventTopicResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRegisterEventTopicResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterEventTopicResponse
mkRegisterEventTopicResponse pResponseStatus_ =
  RegisterEventTopicResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
retrsResponseStatus :: Lens.Lens' RegisterEventTopicResponse Lude.Int
retrsResponseStatus = Lens.lens (responseStatus :: RegisterEventTopicResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterEventTopicResponse)
{-# DEPRECATED retrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
