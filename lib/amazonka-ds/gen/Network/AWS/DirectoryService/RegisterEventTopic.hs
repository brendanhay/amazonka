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
    retrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Registers a new event topic.
--
-- /See:/ 'mkRegisterEventTopic' smart constructor.
data RegisterEventTopic = RegisterEventTopic'
  { -- | The Directory ID that will publish status messages to the SNS topic.
    directoryId :: Types.DirectoryId,
    -- | The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
    topicName :: Types.TopicName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterEventTopic' value with any optional fields omitted.
mkRegisterEventTopic ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'topicName'
  Types.TopicName ->
  RegisterEventTopic
mkRegisterEventTopic directoryId topicName =
  RegisterEventTopic' {directoryId, topicName}

-- | The Directory ID that will publish status messages to the SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
retDirectoryId :: Lens.Lens' RegisterEventTopic Types.DirectoryId
retDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED retDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The SNS topic name to which the directory will publish status messages. This SNS topic must be in the same region as the specified Directory ID.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
retTopicName :: Lens.Lens' RegisterEventTopic Types.TopicName
retTopicName = Lens.field @"topicName"
{-# DEPRECATED retTopicName "Use generic-lens or generic-optics with 'topicName' instead." #-}

instance Core.FromJSON RegisterEventTopic where
  toJSON RegisterEventTopic {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("TopicName" Core..= topicName)
          ]
      )

instance Core.AWSRequest RegisterEventTopic where
  type Rs RegisterEventTopic = RegisterEventTopicResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.RegisterEventTopic")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterEventTopicResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The result of a RegisterEventTopic request.
--
-- /See:/ 'mkRegisterEventTopicResponse' smart constructor.
newtype RegisterEventTopicResponse = RegisterEventTopicResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterEventTopicResponse' value with any optional fields omitted.
mkRegisterEventTopicResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterEventTopicResponse
mkRegisterEventTopicResponse responseStatus =
  RegisterEventTopicResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
retrrsResponseStatus :: Lens.Lens' RegisterEventTopicResponse Core.Int
retrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED retrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
