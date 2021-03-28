{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.UpdateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a channel.
module Network.AWS.IoTAnalytics.UpdateChannel
    (
    -- * Creating a request
      UpdateChannel (..)
    , mkUpdateChannel
    -- ** Request lenses
    , ucChannelName
    , ucChannelStorage
    , ucRetentionPeriod

    -- * Destructuring the response
    , UpdateChannelResponse (..)
    , mkUpdateChannelResponse
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { channelName :: Types.ChannelName
    -- ^ The name of the channel to be updated.
  , channelStorage :: Core.Maybe Types.ChannelStorage
    -- ^ Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannel' value with any optional fields omitted.
mkUpdateChannel
    :: Types.ChannelName -- ^ 'channelName'
    -> UpdateChannel
mkUpdateChannel channelName
  = UpdateChannel'{channelName, channelStorage = Core.Nothing,
                   retentionPeriod = Core.Nothing}

-- | The name of the channel to be updated.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucChannelName :: Lens.Lens' UpdateChannel Types.ChannelName
ucChannelName = Lens.field @"channelName"
{-# INLINEABLE ucChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucChannelStorage :: Lens.Lens' UpdateChannel (Core.Maybe Types.ChannelStorage)
ucChannelStorage = Lens.field @"channelStorage"
{-# INLINEABLE ucChannelStorage #-}
{-# DEPRECATED channelStorage "Use generic-lens or generic-optics with 'channelStorage' instead"  #-}

-- | How long, in days, message data is kept for the channel. The retention period cannot be updated if the channel's S3 storage is customer-managed.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRetentionPeriod :: Lens.Lens' UpdateChannel (Core.Maybe Types.RetentionPeriod)
ucRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE ucRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

instance Core.ToQuery UpdateChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateChannel where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateChannel where
        toJSON UpdateChannel{..}
          = Core.object
              (Core.catMaybes
                 [("channelStorage" Core..=) Core.<$> channelStorage,
                  ("retentionPeriod" Core..=) Core.<$> retentionPeriod])

instance Core.AWSRequest UpdateChannel where
        type Rs UpdateChannel = UpdateChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/channels/" Core.<> Core.toText channelName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateChannelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateChannelResponse' value with any optional fields omitted.
mkUpdateChannelResponse
    :: UpdateChannelResponse
mkUpdateChannelResponse = UpdateChannelResponse'
