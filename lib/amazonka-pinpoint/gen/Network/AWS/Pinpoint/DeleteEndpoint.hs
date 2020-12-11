{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint from an application.
module Network.AWS.Pinpoint.DeleteEndpoint
  ( -- * Creating a request
    DeleteEndpoint (..),
    mkDeleteEndpoint,

    -- ** Request lenses
    deApplicationId,
    deEndpointId,

    -- * Destructuring the response
    DeleteEndpointResponse (..),
    mkDeleteEndpointResponse,

    -- ** Response lenses
    dersResponseStatus,
    dersEndpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
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

-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'endpointId' - The unique identifier for the endpoint.
mkDeleteEndpoint ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'endpointId'
  Lude.Text ->
  DeleteEndpoint
mkDeleteEndpoint pApplicationId_ pEndpointId_ =
  DeleteEndpoint'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deApplicationId :: Lens.Lens' DeleteEndpoint Lude.Text
deApplicationId = Lens.lens (applicationId :: DeleteEndpoint -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteEndpoint)
{-# DEPRECATED deApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the endpoint.
--
-- /Note:/ Consider using 'endpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointId :: Lens.Lens' DeleteEndpoint Lude.Text
deEndpointId = Lens.lens (endpointId :: DeleteEndpoint -> Lude.Text) (\s a -> s {endpointId = a} :: DeleteEndpoint)
{-# DEPRECATED deEndpointId "Use generic-lens or generic-optics with 'endpointId' instead." #-}

instance Lude.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteEndpointResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteEndpoint where
  toPath DeleteEndpoint' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/endpoints/",
        Lude.toBS endpointId
      ]

instance Lude.ToQuery DeleteEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
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

-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'endpointResponse'
  EndpointResponse ->
  DeleteEndpointResponse
mkDeleteEndpointResponse pResponseStatus_ pEndpointResponse_ =
  DeleteEndpointResponse'
    { responseStatus = pResponseStatus_,
      endpointResponse = pEndpointResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DeleteEndpointResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DeleteEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEndpointResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpointResponse :: Lens.Lens' DeleteEndpointResponse EndpointResponse
dersEndpointResponse = Lens.lens (endpointResponse :: DeleteEndpointResponse -> EndpointResponse) (\s a -> s {endpointResponse = a} :: DeleteEndpointResponse)
{-# DEPRECATED dersEndpointResponse "Use generic-lens or generic-optics with 'endpointResponse' instead." #-}
