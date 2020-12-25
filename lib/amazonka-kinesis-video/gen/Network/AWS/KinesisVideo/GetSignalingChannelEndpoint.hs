{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.GetSignalingChannelEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides an endpoint for the specified signaling channel to send and receive messages. This API uses the @SingleMasterChannelEndpointConfiguration@ input parameter, which consists of the @Protocols@ and @Role@ properties.
--
-- @Protocols@ is used to determine the communication mechanism. For example, if you specify @WSS@ as the protocol, this API produces a secure websocket endpoint. If you specify @HTTPS@ as the protocol, this API generates an HTTPS endpoint.
-- @Role@ determines the messaging permissions. A @MASTER@ role results in this API generating an endpoint that a client can use to communicate with any of the viewers on the channel. A @VIEWER@ role results in this API generating an endpoint that a client can use to communicate only with a @MASTER@ .
module Network.AWS.KinesisVideo.GetSignalingChannelEndpoint
  ( -- * Creating a request
    GetSignalingChannelEndpoint (..),
    mkGetSignalingChannelEndpoint,

    -- ** Request lenses
    gsceChannelARN,
    gsceSingleMasterChannelEndpointConfiguration,

    -- * Destructuring the response
    GetSignalingChannelEndpointResponse (..),
    mkGetSignalingChannelEndpointResponse,

    -- ** Response lenses
    gscerrsResourceEndpointList,
    gscerrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSignalingChannelEndpoint' smart constructor.
data GetSignalingChannelEndpoint = GetSignalingChannelEndpoint'
  { -- | The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
    channelARN :: Types.ChannelARN,
    -- | A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
    singleMasterChannelEndpointConfiguration :: Core.Maybe Types.SingleMasterChannelEndpointConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSignalingChannelEndpoint' value with any optional fields omitted.
mkGetSignalingChannelEndpoint ::
  -- | 'channelARN'
  Types.ChannelARN ->
  GetSignalingChannelEndpoint
mkGetSignalingChannelEndpoint channelARN =
  GetSignalingChannelEndpoint'
    { channelARN,
      singleMasterChannelEndpointConfiguration = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsceChannelARN :: Lens.Lens' GetSignalingChannelEndpoint Types.ChannelARN
gsceChannelARN = Lens.field @"channelARN"
{-# DEPRECATED gsceChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
--
-- /Note:/ Consider using 'singleMasterChannelEndpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsceSingleMasterChannelEndpointConfiguration :: Lens.Lens' GetSignalingChannelEndpoint (Core.Maybe Types.SingleMasterChannelEndpointConfiguration)
gsceSingleMasterChannelEndpointConfiguration = Lens.field @"singleMasterChannelEndpointConfiguration"
{-# DEPRECATED gsceSingleMasterChannelEndpointConfiguration "Use generic-lens or generic-optics with 'singleMasterChannelEndpointConfiguration' instead." #-}

instance Core.FromJSON GetSignalingChannelEndpoint where
  toJSON GetSignalingChannelEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ChannelARN" Core..= channelARN),
            ("SingleMasterChannelEndpointConfiguration" Core..=)
              Core.<$> singleMasterChannelEndpointConfiguration
          ]
      )

instance Core.AWSRequest GetSignalingChannelEndpoint where
  type
    Rs GetSignalingChannelEndpoint =
      GetSignalingChannelEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/getSignalingChannelEndpoint",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSignalingChannelEndpointResponse'
            Core.<$> (x Core..:? "ResourceEndpointList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSignalingChannelEndpointResponse' smart constructor.
data GetSignalingChannelEndpointResponse = GetSignalingChannelEndpointResponse'
  { -- | A list of endpoints for the specified signaling channel.
    resourceEndpointList :: Core.Maybe [Types.ResourceEndpointListItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSignalingChannelEndpointResponse' value with any optional fields omitted.
mkGetSignalingChannelEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSignalingChannelEndpointResponse
mkGetSignalingChannelEndpointResponse responseStatus =
  GetSignalingChannelEndpointResponse'
    { resourceEndpointList =
        Core.Nothing,
      responseStatus
    }

-- | A list of endpoints for the specified signaling channel.
--
-- /Note:/ Consider using 'resourceEndpointList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscerrsResourceEndpointList :: Lens.Lens' GetSignalingChannelEndpointResponse (Core.Maybe [Types.ResourceEndpointListItem])
gscerrsResourceEndpointList = Lens.field @"resourceEndpointList"
{-# DEPRECATED gscerrsResourceEndpointList "Use generic-lens or generic-optics with 'resourceEndpointList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscerrsResponseStatus :: Lens.Lens' GetSignalingChannelEndpointResponse Core.Int
gscerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gscerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
