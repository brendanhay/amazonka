{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gdeStreamARN,
    gdeStreamName,
    gdeAPIName,

    -- * Destructuring the response
    GetDataEndpointResponse (..),
    mkGetDataEndpointResponse,

    -- ** Response lenses
    gdersDataEndpoint,
    gdersResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDataEndpoint' smart constructor.
data GetDataEndpoint = GetDataEndpoint'
  { streamARN ::
      Lude.Maybe Lude.Text,
    streamName :: Lude.Maybe Lude.Text,
    apiName :: APIName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataEndpoint' with the minimum fields required to make a request.
--
-- * 'apiName' - The name of the API action for which to get an endpoint.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamName@ in the request.
-- * 'streamName' - The name of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamARN@ in the request.
mkGetDataEndpoint ::
  -- | 'apiName'
  APIName ->
  GetDataEndpoint
mkGetDataEndpoint pAPIName_ =
  GetDataEndpoint'
    { streamARN = Lude.Nothing,
      streamName = Lude.Nothing,
      apiName = pAPIName_
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamName@ in the request.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeStreamARN :: Lens.Lens' GetDataEndpoint (Lude.Maybe Lude.Text)
gdeStreamARN = Lens.lens (streamARN :: GetDataEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: GetDataEndpoint)
{-# DEPRECATED gdeStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to get the endpoint for. You must specify either this parameter or a @StreamARN@ in the request.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeStreamName :: Lens.Lens' GetDataEndpoint (Lude.Maybe Lude.Text)
gdeStreamName = Lens.lens (streamName :: GetDataEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: GetDataEndpoint)
{-# DEPRECATED gdeStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The name of the API action for which to get an endpoint.
--
-- /Note:/ Consider using 'apiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeAPIName :: Lens.Lens' GetDataEndpoint APIName
gdeAPIName = Lens.lens (apiName :: GetDataEndpoint -> APIName) (\s a -> s {apiName = a} :: GetDataEndpoint)
{-# DEPRECATED gdeAPIName "Use generic-lens or generic-optics with 'apiName' instead." #-}

instance Lude.AWSRequest GetDataEndpoint where
  type Rs GetDataEndpoint = GetDataEndpointResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDataEndpointResponse'
            Lude.<$> (x Lude..?> "DataEndpoint") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDataEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetDataEndpoint where
  toJSON GetDataEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName,
            Lude.Just ("APIName" Lude..= apiName)
          ]
      )

instance Lude.ToPath GetDataEndpoint where
  toPath = Lude.const "/getDataEndpoint"

instance Lude.ToQuery GetDataEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDataEndpointResponse' smart constructor.
data GetDataEndpointResponse = GetDataEndpointResponse'
  { dataEndpoint ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDataEndpointResponse' with the minimum fields required to make a request.
--
-- * 'dataEndpoint' - The endpoint value. To read data from the stream or to write data to it, specify this endpoint in your application.
-- * 'responseStatus' - The response status code.
mkGetDataEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDataEndpointResponse
mkGetDataEndpointResponse pResponseStatus_ =
  GetDataEndpointResponse'
    { dataEndpoint = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The endpoint value. To read data from the stream or to write data to it, specify this endpoint in your application.
--
-- /Note:/ Consider using 'dataEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdersDataEndpoint :: Lens.Lens' GetDataEndpointResponse (Lude.Maybe Lude.Text)
gdersDataEndpoint = Lens.lens (dataEndpoint :: GetDataEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {dataEndpoint = a} :: GetDataEndpointResponse)
{-# DEPRECATED gdersDataEndpoint "Use generic-lens or generic-optics with 'dataEndpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdersResponseStatus :: Lens.Lens' GetDataEndpointResponse Lude.Int
gdersResponseStatus = Lens.lens (responseStatus :: GetDataEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDataEndpointResponse)
{-# DEPRECATED gdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
