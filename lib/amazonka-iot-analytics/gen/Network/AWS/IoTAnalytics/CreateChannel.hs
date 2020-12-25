{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateChannel (..),
    mkCreateChannel,

    -- ** Request lenses
    ccChannelName,
    ccChannelStorage,
    ccRetentionPeriod,
    ccTags,

    -- * Destructuring the response
    CreateChannelResponse (..),
    mkCreateChannelResponse,

    -- ** Response lenses
    ccrrsChannelArn,
    ccrrsChannelName,
    ccrrsRetentionPeriod,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | The name of the channel.
    channelName :: Types.ChannelName,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
    channelStorage :: Core.Maybe Types.ChannelStorage,
    -- | How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Core.Maybe Types.RetentionPeriod,
    -- | Metadata which can be used to manage the channel.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChannel' value with any optional fields omitted.
mkCreateChannel ::
  -- | 'channelName'
  Types.ChannelName ->
  CreateChannel
mkCreateChannel channelName =
  CreateChannel'
    { channelName,
      channelStorage = Core.Nothing,
      retentionPeriod = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelName :: Lens.Lens' CreateChannel Types.ChannelName
ccChannelName = Lens.field @"channelName"
{-# DEPRECATED ccChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccChannelStorage :: Lens.Lens' CreateChannel (Core.Maybe Types.ChannelStorage)
ccChannelStorage = Lens.field @"channelStorage"
{-# DEPRECATED ccChannelStorage "Use generic-lens or generic-optics with 'channelStorage' instead." #-}

-- | How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRetentionPeriod :: Lens.Lens' CreateChannel (Core.Maybe Types.RetentionPeriod)
ccRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED ccRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | Metadata which can be used to manage the channel.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateChannel (Core.Maybe (Core.NonEmpty Types.Tag))
ccTags = Lens.field @"tags"
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateChannel where
  toJSON CreateChannel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("channelName" Core..= channelName),
            ("channelStorage" Core..=) Core.<$> channelStorage,
            ("retentionPeriod" Core..=) Core.<$> retentionPeriod,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/channels",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Core.<$> (x Core..:? "channelArn")
            Core.<*> (x Core..:? "channelName")
            Core.<*> (x Core..:? "retentionPeriod")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The ARN of the channel.
    channelArn :: Core.Maybe Types.ChannelArn,
    -- | The name of the channel.
    channelName :: Core.Maybe Types.ChannelName,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Core.Maybe Types.RetentionPeriod,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateChannelResponse' value with any optional fields omitted.
mkCreateChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateChannelResponse
mkCreateChannelResponse responseStatus =
  CreateChannelResponse'
    { channelArn = Core.Nothing,
      channelName = Core.Nothing,
      retentionPeriod = Core.Nothing,
      responseStatus
    }

-- | The ARN of the channel.
--
-- /Note:/ Consider using 'channelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsChannelArn :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.ChannelArn)
ccrrsChannelArn = Lens.field @"channelArn"
{-# DEPRECATED ccrrsChannelArn "Use generic-lens or generic-optics with 'channelArn' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsChannelName :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.ChannelName)
ccrrsChannelName = Lens.field @"channelName"
{-# DEPRECATED ccrrsChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | How long, in days, message data is kept for the channel.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsRetentionPeriod :: Lens.Lens' CreateChannelResponse (Core.Maybe Types.RetentionPeriod)
ccrrsRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED ccrrsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateChannelResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
