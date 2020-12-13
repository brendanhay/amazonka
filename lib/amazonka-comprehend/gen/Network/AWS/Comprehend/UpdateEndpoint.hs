{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.UpdateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the specified endpoint.
module Network.AWS.Comprehend.UpdateEndpoint
  ( -- * Creating a request
    UpdateEndpoint (..),
    mkUpdateEndpoint,

    -- ** Request lenses
    ueDesiredInferenceUnits,
    ueEndpointARN,

    -- * Destructuring the response
    UpdateEndpointResponse (..),
    mkUpdateEndpointResponse,

    -- ** Response lenses
    uersResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
    desiredInferenceUnits :: Lude.Natural,
    -- | The Amazon Resource Number (ARN) of the endpoint being updated.
    endpointARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpoint' with the minimum fields required to make a request.
--
-- * 'desiredInferenceUnits' - The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
-- * 'endpointARN' - The Amazon Resource Number (ARN) of the endpoint being updated.
mkUpdateEndpoint ::
  -- | 'desiredInferenceUnits'
  Lude.Natural ->
  -- | 'endpointARN'
  Lude.Text ->
  UpdateEndpoint
mkUpdateEndpoint pDesiredInferenceUnits_ pEndpointARN_ =
  UpdateEndpoint'
    { desiredInferenceUnits = pDesiredInferenceUnits_,
      endpointARN = pEndpointARN_
    }

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDesiredInferenceUnits :: Lens.Lens' UpdateEndpoint Lude.Natural
ueDesiredInferenceUnits = Lens.lens (desiredInferenceUnits :: UpdateEndpoint -> Lude.Natural) (\s a -> s {desiredInferenceUnits = a} :: UpdateEndpoint)
{-# DEPRECATED ueDesiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead." #-}

-- | The Amazon Resource Number (ARN) of the endpoint being updated.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointARN :: Lens.Lens' UpdateEndpoint Lude.Text
ueEndpointARN = Lens.lens (endpointARN :: UpdateEndpoint -> Lude.Text) (\s a -> s {endpointARN = a} :: UpdateEndpoint)
{-# DEPRECATED ueEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest UpdateEndpoint where
  type Rs UpdateEndpoint = UpdateEndpointResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateEndpointResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.UpdateEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DesiredInferenceUnits" Lude..= desiredInferenceUnits),
            Lude.Just ("EndpointArn" Lude..= endpointARN)
          ]
      )

instance Lude.ToPath UpdateEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
newtype UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEndpointResponse
mkUpdateEndpointResponse pResponseStatus_ =
  UpdateEndpointResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersResponseStatus :: Lens.Lens' UpdateEndpointResponse Lude.Int
uersResponseStatus = Lens.lens (responseStatus :: UpdateEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEndpointResponse)
{-# DEPRECATED uersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
