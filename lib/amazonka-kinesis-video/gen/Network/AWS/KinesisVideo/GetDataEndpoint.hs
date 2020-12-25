{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.GetDataEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an endpoint for a specified stream for either reading or writing. Use this endpoint in your application to read from the specified stream (using the @GetMedia@ or @GetMediaForFragmentList@ operations) or write to it (using the @PutMedia@ operation).
--
-- In the request, specify the stream either by @StreamName@ or @StreamARN@ .
module Network.AWS.KinesisVideo.GetDataEndpoint
  ( -- * Creating a request
    GetDataEndpoint (..),
    mkGetDataEndpoint,

    -- ** Request lenses
    gdeAPIName,
    gdeStreamARN,
    gdeStreamName,

    -- * Destructuring the response
    GetDataEndpointResponse (..),
    mkGetDataEndpointResponse,

    -- ** Response lenses
    gderrsDataEndpoint,
    gderrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDataEndpoint' smart constructor.
data GetDataEndpoint = GetDataEndpoint'
  { -- | The name of the API action for which to get an endpoint.
    aPIName :: Types.APIName,
    -- | The Amazon Resource Name (ARN) of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamName@ in the request.
    streamARN :: Core.Maybe Types.StreamARN,
    -- | The name of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamARN@ in the request.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataEndpoint' value with any optional fields omitted.
mkGetDataEndpoint ::
  -- | 'aPIName'
  Types.APIName ->
  GetDataEndpoint
mkGetDataEndpoint aPIName =
  GetDataEndpoint'
    { aPIName,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | The name of the API action for which to get an endpoint.
--
-- /Note:/ Consider using 'aPIName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeAPIName :: Lens.Lens' GetDataEndpoint Types.APIName
gdeAPIName = Lens.field @"aPIName"
{-# DEPRECATED gdeAPIName "Use generic-lens or generic-optics with 'aPIName' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamName@ in the request.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeStreamARN :: Lens.Lens' GetDataEndpoint (Core.Maybe Types.StreamARN)
gdeStreamARN = Lens.field @"streamARN"
{-# DEPRECATED gdeStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamARN@ in the request.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeStreamName :: Lens.Lens' GetDataEndpoint (Core.Maybe Types.StreamName)
gdeStreamName = Lens.field @"streamName"
{-# DEPRECATED gdeStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON GetDataEndpoint where
  toJSON GetDataEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("APIName" Core..= aPIName),
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest GetDataEndpoint where
  type Rs GetDataEndpoint = GetDataEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/getDataEndpoint",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataEndpointResponse'
            Core.<$> (x Core..:? "DataEndpoint") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDataEndpointResponse' smart constructor.
data GetDataEndpointResponse = GetDataEndpointResponse'
  { -- | The endpoint value. To read data from the stream or to write data to it, specify this endpoint in your application.
    dataEndpoint :: Core.Maybe Types.DataEndpoint,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataEndpointResponse' value with any optional fields omitted.
mkGetDataEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDataEndpointResponse
mkGetDataEndpointResponse responseStatus =
  GetDataEndpointResponse'
    { dataEndpoint = Core.Nothing,
      responseStatus
    }

-- | The endpoint value. To read data from the stream or to write data to it, specify this endpoint in your application.
--
-- /Note:/ Consider using 'dataEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderrsDataEndpoint :: Lens.Lens' GetDataEndpointResponse (Core.Maybe Types.DataEndpoint)
gderrsDataEndpoint = Lens.field @"dataEndpoint"
{-# DEPRECATED gderrsDataEndpoint "Use generic-lens or generic-optics with 'dataEndpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderrsResponseStatus :: Lens.Lens' GetDataEndpointResponse Core.Int
gderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
