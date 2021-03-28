{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DescribeSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the signaling channel. You must specify either the name or the Amazon Resource Name (ARN) of the channel that you want to describe.
module Network.AWS.KinesisVideo.DescribeSignalingChannel
    (
    -- * Creating a request
      DescribeSignalingChannel (..)
    , mkDescribeSignalingChannel
    -- ** Request lenses
    , dChannelARN
    , dChannelName

    -- * Destructuring the response
    , DescribeSignalingChannelResponse (..)
    , mkDescribeSignalingChannelResponse
    -- ** Response lenses
    , drsChannelInfo
    , drsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSignalingChannel' smart constructor.
data DescribeSignalingChannel = DescribeSignalingChannel'
  { channelARN :: Core.Maybe Types.ResourceARN
    -- ^ The ARN of the signaling channel that you want to describe.
  , channelName :: Core.Maybe Types.ChannelName
    -- ^ The name of the signaling channel that you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSignalingChannel' value with any optional fields omitted.
mkDescribeSignalingChannel
    :: DescribeSignalingChannel
mkDescribeSignalingChannel
  = DescribeSignalingChannel'{channelARN = Core.Nothing,
                              channelName = Core.Nothing}

-- | The ARN of the signaling channel that you want to describe.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelARN :: Lens.Lens' DescribeSignalingChannel (Core.Maybe Types.ResourceARN)
dChannelARN = Lens.field @"channelARN"
{-# INLINEABLE dChannelARN #-}
{-# DEPRECATED channelARN "Use generic-lens or generic-optics with 'channelARN' instead"  #-}

-- | The name of the signaling channel that you want to describe.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelName :: Lens.Lens' DescribeSignalingChannel (Core.Maybe Types.ChannelName)
dChannelName = Lens.field @"channelName"
{-# INLINEABLE dChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

instance Core.ToQuery DescribeSignalingChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSignalingChannel where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DescribeSignalingChannel where
        toJSON DescribeSignalingChannel{..}
          = Core.object
              (Core.catMaybes
                 [("ChannelARN" Core..=) Core.<$> channelARN,
                  ("ChannelName" Core..=) Core.<$> channelName])

instance Core.AWSRequest DescribeSignalingChannel where
        type Rs DescribeSignalingChannel = DescribeSignalingChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/describeSignalingChannel",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSignalingChannelResponse' Core.<$>
                   (x Core..:? "ChannelInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSignalingChannelResponse' smart constructor.
data DescribeSignalingChannelResponse = DescribeSignalingChannelResponse'
  { channelInfo :: Core.Maybe Types.ChannelInfo
    -- ^ A structure that encapsulates the specified signaling channel's metadata and properties.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSignalingChannelResponse' value with any optional fields omitted.
mkDescribeSignalingChannelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSignalingChannelResponse
mkDescribeSignalingChannelResponse responseStatus
  = DescribeSignalingChannelResponse'{channelInfo = Core.Nothing,
                                      responseStatus}

-- | A structure that encapsulates the specified signaling channel's metadata and properties.
--
-- /Note:/ Consider using 'channelInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsChannelInfo :: Lens.Lens' DescribeSignalingChannelResponse (Core.Maybe Types.ChannelInfo)
drsChannelInfo = Lens.field @"channelInfo"
{-# INLINEABLE drsChannelInfo #-}
{-# DEPRECATED channelInfo "Use generic-lens or generic-optics with 'channelInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeSignalingChannelResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
