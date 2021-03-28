{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeregisterEventTopic (..)
    , mkDeregisterEventTopic
    -- ** Request lenses
    , detDirectoryId
    , detTopicName

    -- * Destructuring the response
    , DeregisterEventTopicResponse (..)
    , mkDeregisterEventTopicResponse
    -- ** Response lenses
    , detrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes the specified directory as a publisher to the specified SNS topic.
--
-- /See:/ 'mkDeregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { directoryId :: Types.DirectoryId
    -- ^ The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
  , topicName :: Types.TopicName
    -- ^ The name of the SNS topic from which to remove the directory as a publisher.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterEventTopic' value with any optional fields omitted.
mkDeregisterEventTopic
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.TopicName -- ^ 'topicName'
    -> DeregisterEventTopic
mkDeregisterEventTopic directoryId topicName
  = DeregisterEventTopic'{directoryId, topicName}

-- | The Directory ID to remove as a publisher. This directory will no longer send messages to the specified SNS topic.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detDirectoryId :: Lens.Lens' DeregisterEventTopic Types.DirectoryId
detDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE detDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The name of the SNS topic from which to remove the directory as a publisher.
--
-- /Note:/ Consider using 'topicName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTopicName :: Lens.Lens' DeregisterEventTopic Types.TopicName
detTopicName = Lens.field @"topicName"
{-# INLINEABLE detTopicName #-}
{-# DEPRECATED topicName "Use generic-lens or generic-optics with 'topicName' instead"  #-}

instance Core.ToQuery DeregisterEventTopic where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterEventTopic where
        toHeaders DeregisterEventTopic{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DeregisterEventTopic")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterEventTopic where
        toJSON DeregisterEventTopic{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("TopicName" Core..= topicName)])

instance Core.AWSRequest DeregisterEventTopic where
        type Rs DeregisterEventTopic = DeregisterEventTopicResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeregisterEventTopicResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The result of a DeregisterEventTopic request.
--
-- /See:/ 'mkDeregisterEventTopicResponse' smart constructor.
newtype DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterEventTopicResponse' value with any optional fields omitted.
mkDeregisterEventTopicResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeregisterEventTopicResponse
mkDeregisterEventTopicResponse responseStatus
  = DeregisterEventTopicResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DeregisterEventTopicResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE detrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
