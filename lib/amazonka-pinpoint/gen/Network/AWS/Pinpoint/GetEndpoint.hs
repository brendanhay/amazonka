{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the settings and attributes of a specific endpoint for an application.
module Network.AWS.Pinpoint.GetEndpoint
  ( -- * Creating a request
    GetEndpoint (..),
    mkGetEndpoint,

    -- ** Request lenses
    geApplicationId,
    geEndpointId,

    -- * Destructuring the response
    GetEndpointResponse (..),
    mkGetEndpointResponse,

    -- ** Response lenses
    gersResponseStatus,
    gersEndpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEndpoint' smart constructor.
data GetEndpoint = GetEndpoint'
  { applicationId :: Lude.Text,
    endpointId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEndpoint' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'endpointId' - The unique identifier for the endpoint.
mkGetEndpoint ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'endpointId'
  Lude.Text ->
  GetEndpoint
mkGetEndpoint pApplicationId_ pEndpointId_ =
  GetEndpoint'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geApplicationId :: Lens.Lens' GetEndpoint Lude.Text
geApplicationId = Lens.lens (applicationId :: GetEndpoint -> Lude.Text) (\s a -> s {applicationId = a} :: GetEndpoint)
{-# DEPRECATED geApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the endpoint.
--
-- /Note:/ Consider using 'endpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geEndpointId :: Lens.Lens' GetEndpoint Lude.Text
geEndpointId = Lens.lens (endpointId :: GetEndpoint -> Lude.Text) (\s a -> s {endpointId = a} :: GetEndpoint)
{-# DEPRECATED geEndpointId "Use generic-lens or generic-optics with 'endpointId' instead." #-}

instance Lude.AWSRequest GetEndpoint where
  type Rs GetEndpoint = GetEndpointResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEndpointResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetEndpoint where
  toPath GetEndpoint' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/endpoints/",
        Lude.toBS endpointId
      ]

instance Lude.ToQuery GetEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetEndpointResponse' smart constructor.
data GetEndpointResponse = GetEndpointResponse'
  { responseStatus ::
      Lude.Int,
    endpointResponse :: EndpointResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'endpointResponse'
  EndpointResponse ->
  GetEndpointResponse
mkGetEndpointResponse pResponseStatus_ pEndpointResponse_ =
  GetEndpointResponse'
    { responseStatus = pResponseStatus_,
      endpointResponse = pEndpointResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersResponseStatus :: Lens.Lens' GetEndpointResponse Lude.Int
gersResponseStatus = Lens.lens (responseStatus :: GetEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEndpointResponse)
{-# DEPRECATED gersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gersEndpointResponse :: Lens.Lens' GetEndpointResponse EndpointResponse
gersEndpointResponse = Lens.lens (endpointResponse :: GetEndpointResponse -> EndpointResponse) (\s a -> s {endpointResponse = a} :: GetEndpointResponse)
{-# DEPRECATED gersEndpointResponse "Use generic-lens or generic-optics with 'endpointResponse' instead." #-}
