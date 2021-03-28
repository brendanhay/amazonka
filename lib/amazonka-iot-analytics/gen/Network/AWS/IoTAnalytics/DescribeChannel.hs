{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a channel.
module Network.AWS.IoTAnalytics.DescribeChannel
    (
    -- * Creating a request
      DescribeChannel (..)
    , mkDescribeChannel
    -- ** Request lenses
    , dcfChannelName
    , dcfIncludeStatistics

    -- * Destructuring the response
    , DescribeChannelResponse (..)
    , mkDescribeChannelResponse
    -- ** Response lenses
    , dcrrsChannel
    , dcrrsStatistics
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { channelName :: Types.ChannelName
    -- ^ The name of the channel whose information is retrieved.
  , includeStatistics :: Core.Maybe Core.Bool
    -- ^ If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChannel' value with any optional fields omitted.
mkDescribeChannel
    :: Types.ChannelName -- ^ 'channelName'
    -> DescribeChannel
mkDescribeChannel channelName
  = DescribeChannel'{channelName, includeStatistics = Core.Nothing}

-- | The name of the channel whose information is retrieved.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfChannelName :: Lens.Lens' DescribeChannel Types.ChannelName
dcfChannelName = Lens.field @"channelName"
{-# INLINEABLE dcfChannelName #-}
{-# DEPRECATED channelName "Use generic-lens or generic-optics with 'channelName' instead"  #-}

-- | If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
--
-- /Note:/ Consider using 'includeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfIncludeStatistics :: Lens.Lens' DescribeChannel (Core.Maybe Core.Bool)
dcfIncludeStatistics = Lens.field @"includeStatistics"
{-# INLINEABLE dcfIncludeStatistics #-}
{-# DEPRECATED includeStatistics "Use generic-lens or generic-optics with 'includeStatistics' instead"  #-}

instance Core.ToQuery DescribeChannel where
        toQuery DescribeChannel{..}
          = Core.maybe Core.mempty (Core.toQueryPair "includeStatistics")
              includeStatistics

instance Core.ToHeaders DescribeChannel where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeChannel where
        type Rs DescribeChannel = DescribeChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/channels/" Core.<> Core.toText channelName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeChannelResponse' Core.<$>
                   (x Core..:? "channel") Core.<*> x Core..:? "statistics" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { channel :: Core.Maybe Types.Channel
    -- ^ An object that contains information about the channel.
  , statistics :: Core.Maybe Types.ChannelStatistics
    -- ^ Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeChannelResponse' value with any optional fields omitted.
mkDescribeChannelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeChannelResponse
mkDescribeChannelResponse responseStatus
  = DescribeChannelResponse'{channel = Core.Nothing,
                             statistics = Core.Nothing, responseStatus}

-- | An object that contains information about the channel.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsChannel :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.Channel)
dcrrsChannel = Lens.field @"channel"
{-# INLINEABLE dcrrsChannel #-}
{-# DEPRECATED channel "Use generic-lens or generic-optics with 'channel' instead"  #-}

-- | Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsStatistics :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.ChannelStatistics)
dcrrsStatistics = Lens.field @"statistics"
{-# INLINEABLE dcrrsStatistics #-}
{-# DEPRECATED statistics "Use generic-lens or generic-optics with 'statistics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeChannelResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
