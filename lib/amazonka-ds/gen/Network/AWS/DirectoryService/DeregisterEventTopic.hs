{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    detrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes the specified directory as a publisher to the specified SNS topic.
--
-- /See:/ 'mkDeregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { -- | The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
    directoryId :: Types.DirectoryId,
    -- | The name of the SNS topic from which to remove the directory as a publisher.
    topicName :: Types.TopicName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterEventTopic' value with any optional fields omitted.
mkDeregisterEventTopic ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'topicName'
  Types.TopicName ->
  DeregisterEventTopic
mkDeregisterEventTopic directoryId topicName =
  DeregisterEventTopic' {directoryId, topicName}

-- | The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detDirectoryId :: Lens.Lens' DeregisterEventTopic Types.DirectoryId
detDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED detDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the SNS topic from which to remove the directory as a publisher.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTopicName :: Lens.Lens' DeregisterEventTopic Types.TopicName
detTopicName = Lens.field @"topicName"
{-# DEPRECATED detTopicName "Use generic-lens or generic-optics with 'topicName' instead." #-}

instance Core.FromJSON DeregisterEventTopic where
  toJSON DeregisterEventTopic {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("TopicName" Core..= topicName)
          ]
      )

instance Core.AWSRequest DeregisterEventTopic where
  type Rs DeregisterEventTopic = DeregisterEventTopicResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.DeregisterEventTopic")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterEventTopicResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The result of a DeregisterEventTopic request.
--
-- /See:/ 'mkDeregisterEventTopicResponse' smart constructor.
newtype DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterEventTopicResponse' value with any optional fields omitted.
mkDeregisterEventTopicResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeregisterEventTopicResponse
mkDeregisterEventTopicResponse responseStatus =
  DeregisterEventTopicResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DeregisterEventTopicResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED detrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
