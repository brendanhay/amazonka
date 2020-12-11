{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new endpoint for an application or updates the settings and attributes of an existing endpoint for an application. You can also use this operation to define custom attributes for an endpoint. If an update includes one or more values for a custom attribute, Amazon Pinpoint replaces (overwrites) any existing values with the new values.
module Network.AWS.Pinpoint.UpdateEndpoint
  ( -- * Creating a request
    UpdateEndpoint (..),
    mkUpdateEndpoint,

    -- ** Request lenses
    ueApplicationId,
    ueEndpointId,
    ueEndpointRequest,

    -- * Destructuring the response
    UpdateEndpointResponse (..),
    mkUpdateEndpointResponse,

    -- ** Response lenses
    uersResponseStatus,
    uersMessageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { applicationId :: Lude.Text,
    endpointId :: Lude.Text,
    endpointRequest :: EndpointRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpoint' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'endpointId' - The unique identifier for the endpoint.
-- * 'endpointRequest' - Undocumented field.
mkUpdateEndpoint ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'endpointId'
  Lude.Text ->
  -- | 'endpointRequest'
  EndpointRequest ->
  UpdateEndpoint
mkUpdateEndpoint pApplicationId_ pEndpointId_ pEndpointRequest_ =
  UpdateEndpoint'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_,
      endpointRequest = pEndpointRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueApplicationId :: Lens.Lens' UpdateEndpoint Lude.Text
ueApplicationId = Lens.lens (applicationId :: UpdateEndpoint -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateEndpoint)
{-# DEPRECATED ueApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the endpoint.
--
-- /Note:/ Consider using 'endpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointId :: Lens.Lens' UpdateEndpoint Lude.Text
ueEndpointId = Lens.lens (endpointId :: UpdateEndpoint -> Lude.Text) (\s a -> s {endpointId = a} :: UpdateEndpoint)
{-# DEPRECATED ueEndpointId "Use generic-lens or generic-optics with 'endpointId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointRequest :: Lens.Lens' UpdateEndpoint EndpointRequest
ueEndpointRequest = Lens.lens (endpointRequest :: UpdateEndpoint -> EndpointRequest) (\s a -> s {endpointRequest = a} :: UpdateEndpoint)
{-# DEPRECATED ueEndpointRequest "Use generic-lens or generic-optics with 'endpointRequest' instead." #-}

instance Lude.AWSRequest UpdateEndpoint where
  type Rs UpdateEndpoint = UpdateEndpointResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEndpointResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EndpointRequest" Lude..= endpointRequest)]
      )

instance Lude.ToPath UpdateEndpoint where
  toPath UpdateEndpoint' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/endpoints/",
        Lude.toBS endpointId
      ]

instance Lude.ToQuery UpdateEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { responseStatus ::
      Lude.Int,
    messageBody :: MessageBody
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'messageBody' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateEndpointResponse
mkUpdateEndpointResponse pResponseStatus_ pMessageBody_ =
  UpdateEndpointResponse'
    { responseStatus = pResponseStatus_,
      messageBody = pMessageBody_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersResponseStatus :: Lens.Lens' UpdateEndpointResponse Lude.Int
uersResponseStatus = Lens.lens (responseStatus :: UpdateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersMessageBody :: Lens.Lens' UpdateEndpointResponse MessageBody
uersMessageBody = Lens.lens (messageBody :: UpdateEndpointResponse -> MessageBody) (\s a -> s {messageBody = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersMessageBody "Use generic-lens or generic-optics with 'messageBody' instead." #-}
