{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel. A channel collects data from an MQTT topic and archives the raw, unprocessed messages before publishing the data to a pipeline.
module Network.AWS.IoTAnalytics.CreateChannel
    (
    -- * Creating a request
      CreateChannel (..)
    , mkCreateChannel
    -- ** Request lenses
    , ccChannelName
    , ccChannelStorage
    , ccRetentionPeriod
    , ccTags

    -- * Destructuring the response
    , CreateChannelResponse (..)
    , mkCreateChannelResponse
    -- ** Response lenses
    , ccrrsChannelArn
    , ccrrsChannelName
    , ccrrsRetentionPeriod
    , ccrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { channelName :: Types.ChannelName
    -- ^ The name of the channel.
  , channelStorage :: Core.Maybe Types.ChannelStorage
    -- ^ Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ Metadata which can be used to manage the channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChannel' value with any optional fields omitted.
mkCreateChannel
    :: Types.ChannelName -- ^ 'channelName'
    -> CreateChannel
mkCreateChannel channelName
  = CreateChannel'{channelName, channelStorage = Core.Nothing,
                   retentionPeriod = Core.Nothing, tags = Core.Nothing}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelName :: Lens.Lens' CreateChannel Types.ChannelName
ccChannelName = Lens.field @"channelName"
{-# INLINEABLE ccChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelStorage :: Lens.Lens' CreateChannel (Core.Maybe Types.ChannelStorage)
ccChannelStorage = Lens.field @"channelStorage"
{-# INLINEABLE ccChannelStorage #-}
{-# DEPRECATED channelStorage "Use generic-lens or generic-optics with 'channelStorage' instead"  #-}

-- | How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRetentionPeriod :: Lens.Lens' CreateChannel (Core.Maybe Types.RetentionPeriod)
ccRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE ccRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | Metadata which can be used to manage the channel.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateChannel (Core.Maybe (Core.NonEmpty Types.Tag))
ccTags = Lens.field @"tags"
{-# INLINEABLE ccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateChannel where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateChannel where
        toJSON CreateChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("channelName" Core..= channelName),
                  ("channelStorage" Core..=) Core.<$> channelStorage,
                  ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
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
                   (x Core..:? "channelArn") Core.<*> x Core..:? "channelName"
                     Core.<*> x Core..:? "retentionPeriod"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { channelArn :: Core.Maybe Types.ChannelArn
    -- ^ The ARN of the channel.
  , channelName :: Core.Maybe Types.ChannelName
    -- ^ The name of the channel.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, message data is kept for the channel.
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
  = CreateChannelResponse'{channelArn = Core.Nothing,
                           channelName = Core.Nothing, retentionPeriod = Core.Nothing,
                           responseStatus}

-- | The ARN of the channel.
--
-- /Note:/ Consider using 'channelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsChannelArn :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.ChannelArn)
ccrrsChannelArn = Lens.field @"channelArn"
{-# INLINEABLE ccrrsChannelArn #-}
{-# DEPRECATED channelArn "Use generic-lens or generic-optics with 'channelArn' instead"  #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsChannelName :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.ChannelName)
ccrrsChannelName = Lens.field @"channelName"
{-# INLINEABLE ccrrsChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

-- | How long, in days, message data is kept for the channel.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsRetentionPeriod :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.RetentionPeriod)
ccrrsRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE ccrrsRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateChannelResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
