{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Channel.
module Network.AWS.MediaPackage.CreateChannel
    (
    -- * Creating a request
      CreateChannel (..)
    , mkCreateChannel
    -- ** Request lenses
    , ccId
    , ccDescription
    , ccTags

    -- * Destructuring the response
    , CreateChannelResponse (..)
    , mkCreateChannelResponse
    -- ** Response lenses
    , ccrrsArn
    , ccrrsDescription
    , ccrrsEgressAccessLogs
    , ccrrsHlsIngest
    , ccrrsId
    , ccrrsIngressAccessLogs
    , ccrrsTags
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A new Channel configuration.
--
-- /See:/ 'mkCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { id :: Core.Text
    -- ^ The ID of the Channel. The ID must be unique within the region and it
--
-- cannot be changed after a Channel is created.
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the Channel.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChannel' value with any optional fields omitted.
mkCreateChannel
    :: Core.Text -- ^ 'id'
    -> CreateChannel
mkCreateChannel id
  = CreateChannel'{id, description = Core.Nothing,
                   tags = Core.Nothing}

-- | The ID of the Channel. The ID must be unique within the region and it
--
-- cannot be changed after a Channel is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccId :: Lens.Lens' CreateChannel Core.Text
ccId = Lens.field @"id"
{-# INLINEABLE ccId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateChannel (Core.Maybe Core.Text)
ccDescription = Lens.field @"description"
{-# INLINEABLE ccDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateChannel (Core.Maybe (Core.HashMap Core.Text Core.Text))
ccTags = Lens.field @"tags"
{-# INLINEABLE ccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateChannel where
        toHeaders CreateChannel{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateChannel where
        toJSON CreateChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id),
                  ("description" Core..=) Core.<$> description,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateChannel where
        type Rs CreateChannel = CreateChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/channels",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateChannelResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                     x Core..:? "egressAccessLogs"
                     Core.<*> x Core..:? "hlsIngest"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "ingressAccessLogs"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the Channel.
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the Channel.
  , egressAccessLogs :: Core.Maybe Types.EgressAccessLogs
  , hlsIngest :: Core.Maybe Types.HlsIngest
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the Channel.
  , ingressAccessLogs :: Core.Maybe Types.IngressAccessLogs
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChannelResponse' value with any optional fields omitted.
mkCreateChannelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateChannelResponse
mkCreateChannelResponse responseStatus
  = CreateChannelResponse'{arn = Core.Nothing,
                           description = Core.Nothing, egressAccessLogs = Core.Nothing,
                           hlsIngest = Core.Nothing, id = Core.Nothing,
                           ingressAccessLogs = Core.Nothing, tags = Core.Nothing,
                           responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsArn :: Lens.Lens' CreateChannelResponse (Core.Maybe Core.Text)
ccrrsArn = Lens.field @"arn"
{-# INLINEABLE ccrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsDescription :: Lens.Lens' CreateChannelResponse (Core.Maybe Core.Text)
ccrrsDescription = Lens.field @"description"
{-# INLINEABLE ccrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsEgressAccessLogs :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.EgressAccessLogs)
ccrrsEgressAccessLogs = Lens.field @"egressAccessLogs"
{-# INLINEABLE ccrrsEgressAccessLogs #-}
{-# DEPRECATED egressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsHlsIngest :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.HlsIngest)
ccrrsHlsIngest = Lens.field @"hlsIngest"
{-# INLINEABLE ccrrsHlsIngest #-}
{-# DEPRECATED hlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead"  #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsId :: Lens.Lens' CreateChannelResponse (Core.Maybe Core.Text)
ccrrsId = Lens.field @"id"
{-# INLINEABLE ccrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsIngressAccessLogs :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.IngressAccessLogs)
ccrrsIngressAccessLogs = Lens.field @"ingressAccessLogs"
{-# INLINEABLE ccrrsIngressAccessLogs #-}
{-# DEPRECATED ingressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsTags :: Lens.Lens' CreateChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
ccrrsTags = Lens.field @"tags"
{-# INLINEABLE ccrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateChannelResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
