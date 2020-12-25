{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeChannel (..),
    mkDescribeChannel,

    -- ** Request lenses
    dcfChannelName,
    dcfIncludeStatistics,

    -- * Destructuring the response
    DescribeChannelResponse (..),
    mkDescribeChannelResponse,

    -- ** Response lenses
    dcrrsChannel,
    dcrrsStatistics,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | The name of the channel whose information is retrieved.
    channelName :: Types.ChannelName,
    -- | If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
    includeStatistics :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeChannel' value with any optional fields omitted.
mkDescribeChannel ::
  -- | 'channelName'
  Types.ChannelName ->
  DescribeChannel
mkDescribeChannel channelName =
  DescribeChannel' {channelName, includeStatistics = Core.Nothing}

-- | The name of the channel whose information is retrieved.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfChannelName :: Lens.Lens' DescribeChannel Types.ChannelName
dcfChannelName = Lens.field @"channelName"
{-# DEPRECATED dcfChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | If true, additional statistical information about the channel is included in the response. This feature cannot be used with a channel whose S3 storage is customer-managed.
--
-- /Note:/ Consider using 'includeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfIncludeStatistics :: Lens.Lens' DescribeChannel (Core.Maybe Core.Bool)
dcfIncludeStatistics = Lens.field @"includeStatistics"
{-# DEPRECATED dcfIncludeStatistics "Use generic-lens or generic-optics with 'includeStatistics' instead." #-}

instance Core.AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/channels/" Core.<> (Core.toText channelName)),
        Core._rqQuery =
          Core.toQueryValue "includeStatistics" Core.<$> includeStatistics,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Core.<$> (x Core..:? "channel")
            Core.<*> (x Core..:? "statistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | An object that contains information about the channel.
    channel :: Core.Maybe Types.Channel,
    -- | Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
    statistics :: Core.Maybe Types.ChannelStatistics,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeChannelResponse' value with any optional fields omitted.
mkDescribeChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeChannelResponse
mkDescribeChannelResponse responseStatus =
  DescribeChannelResponse'
    { channel = Core.Nothing,
      statistics = Core.Nothing,
      responseStatus
    }

-- | An object that contains information about the channel.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsChannel :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.Channel)
dcrrsChannel = Lens.field @"channel"
{-# DEPRECATED dcrrsChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | Statistics about the channel. Included if the @includeStatistics@ parameter is set to @true@ in the request.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsStatistics :: Lens.Lens' DescribeChannelResponse (Core.Maybe Types.ChannelStatistics)
dcrrsStatistics = Lens.field @"statistics"
{-# DEPRECATED dcrrsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeChannelResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
