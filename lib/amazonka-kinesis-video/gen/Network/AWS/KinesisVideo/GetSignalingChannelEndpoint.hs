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
    gscersResourceEndpointList,
    gscersResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSignalingChannelEndpoint' smart constructor.
data GetSignalingChannelEndpoint = GetSignalingChannelEndpoint'
  { -- | The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
    channelARN :: Lude.Text,
    -- | A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
    singleMasterChannelEndpointConfiguration :: Lude.Maybe SingleMasterChannelEndpointConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSignalingChannelEndpoint' with the minimum fields required to make a request.
--
-- * 'channelARN' - The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
-- * 'singleMasterChannelEndpointConfiguration' - A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
mkGetSignalingChannelEndpoint ::
  -- | 'channelARN'
  Lude.Text ->
  GetSignalingChannelEndpoint
mkGetSignalingChannelEndpoint pChannelARN_ =
  GetSignalingChannelEndpoint'
    { channelARN = pChannelARN_,
      singleMasterChannelEndpointConfiguration = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the signalling channel for which you want to get an endpoint.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsceChannelARN :: Lens.Lens' GetSignalingChannelEndpoint Lude.Text
gsceChannelARN = Lens.lens (channelARN :: GetSignalingChannelEndpoint -> Lude.Text) (\s a -> s {channelARN = a} :: GetSignalingChannelEndpoint)
{-# DEPRECATED gsceChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | A structure containing the endpoint configuration for the @SINGLE_MASTER@ channel type.
--
-- /Note:/ Consider using 'singleMasterChannelEndpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsceSingleMasterChannelEndpointConfiguration :: Lens.Lens' GetSignalingChannelEndpoint (Lude.Maybe SingleMasterChannelEndpointConfiguration)
gsceSingleMasterChannelEndpointConfiguration = Lens.lens (singleMasterChannelEndpointConfiguration :: GetSignalingChannelEndpoint -> Lude.Maybe SingleMasterChannelEndpointConfiguration) (\s a -> s {singleMasterChannelEndpointConfiguration = a} :: GetSignalingChannelEndpoint)
{-# DEPRECATED gsceSingleMasterChannelEndpointConfiguration "Use generic-lens or generic-optics with 'singleMasterChannelEndpointConfiguration' instead." #-}

instance Lude.AWSRequest GetSignalingChannelEndpoint where
  type
    Rs GetSignalingChannelEndpoint =
      GetSignalingChannelEndpointResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSignalingChannelEndpointResponse'
            Lude.<$> (x Lude..?> "ResourceEndpointList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSignalingChannelEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetSignalingChannelEndpoint where
  toJSON GetSignalingChannelEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ChannelARN" Lude..= channelARN),
            ("SingleMasterChannelEndpointConfiguration" Lude..=)
              Lude.<$> singleMasterChannelEndpointConfiguration
          ]
      )

instance Lude.ToPath GetSignalingChannelEndpoint where
  toPath = Lude.const "/getSignalingChannelEndpoint"

instance Lude.ToQuery GetSignalingChannelEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSignalingChannelEndpointResponse' smart constructor.
data GetSignalingChannelEndpointResponse = GetSignalingChannelEndpointResponse'
  { -- | A list of endpoints for the specified signaling channel.
    resourceEndpointList :: Lude.Maybe [ResourceEndpointListItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSignalingChannelEndpointResponse' with the minimum fields required to make a request.
--
-- * 'resourceEndpointList' - A list of endpoints for the specified signaling channel.
-- * 'responseStatus' - The response status code.
mkGetSignalingChannelEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSignalingChannelEndpointResponse
mkGetSignalingChannelEndpointResponse pResponseStatus_ =
  GetSignalingChannelEndpointResponse'
    { resourceEndpointList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of endpoints for the specified signaling channel.
--
-- /Note:/ Consider using 'resourceEndpointList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscersResourceEndpointList :: Lens.Lens' GetSignalingChannelEndpointResponse (Lude.Maybe [ResourceEndpointListItem])
gscersResourceEndpointList = Lens.lens (resourceEndpointList :: GetSignalingChannelEndpointResponse -> Lude.Maybe [ResourceEndpointListItem]) (\s a -> s {resourceEndpointList = a} :: GetSignalingChannelEndpointResponse)
{-# DEPRECATED gscersResourceEndpointList "Use generic-lens or generic-optics with 'resourceEndpointList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscersResponseStatus :: Lens.Lens' GetSignalingChannelEndpointResponse Lude.Int
gscersResponseStatus = Lens.lens (responseStatus :: GetSignalingChannelEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSignalingChannelEndpointResponse)
{-# DEPRECATED gscersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
