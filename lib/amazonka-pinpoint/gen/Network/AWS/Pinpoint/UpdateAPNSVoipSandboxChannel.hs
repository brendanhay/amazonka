{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP sandbox channel for an application or updates the status and settings of the APNs VoIP sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateAPNSVoipSandboxChannel
  ( -- * Creating a request
    UpdateAPNSVoipSandboxChannel (..),
    mkUpdateAPNSVoipSandboxChannel,

    -- ** Request lenses
    uavscAPNSVoipSandboxChannelRequest,
    uavscApplicationId,

    -- * Destructuring the response
    UpdateAPNSVoipSandboxChannelResponse (..),
    mkUpdateAPNSVoipSandboxChannelResponse,

    -- ** Response lenses
    uavscrsAPNSVoipSandboxChannelResponse,
    uavscrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAPNSVoipSandboxChannel' smart constructor.
data UpdateAPNSVoipSandboxChannel = UpdateAPNSVoipSandboxChannel'
  { apnsVoipSandboxChannelRequest :: APNSVoipSandboxChannelRequest,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSVoipSandboxChannel' with the minimum fields required to make a request.
--
-- * 'apnsVoipSandboxChannelRequest' -
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkUpdateAPNSVoipSandboxChannel ::
  -- | 'apnsVoipSandboxChannelRequest'
  APNSVoipSandboxChannelRequest ->
  -- | 'applicationId'
  Lude.Text ->
  UpdateAPNSVoipSandboxChannel
mkUpdateAPNSVoipSandboxChannel
  pAPNSVoipSandboxChannelRequest_
  pApplicationId_ =
    UpdateAPNSVoipSandboxChannel'
      { apnsVoipSandboxChannelRequest =
          pAPNSVoipSandboxChannelRequest_,
        applicationId = pApplicationId_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipSandboxChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscAPNSVoipSandboxChannelRequest :: Lens.Lens' UpdateAPNSVoipSandboxChannel APNSVoipSandboxChannelRequest
uavscAPNSVoipSandboxChannelRequest = Lens.lens (apnsVoipSandboxChannelRequest :: UpdateAPNSVoipSandboxChannel -> APNSVoipSandboxChannelRequest) (\s a -> s {apnsVoipSandboxChannelRequest = a} :: UpdateAPNSVoipSandboxChannel)
{-# DEPRECATED uavscAPNSVoipSandboxChannelRequest "Use generic-lens or generic-optics with 'apnsVoipSandboxChannelRequest' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscApplicationId :: Lens.Lens' UpdateAPNSVoipSandboxChannel Lude.Text
uavscApplicationId = Lens.lens (applicationId :: UpdateAPNSVoipSandboxChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateAPNSVoipSandboxChannel)
{-# DEPRECATED uavscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest UpdateAPNSVoipSandboxChannel where
  type
    Rs UpdateAPNSVoipSandboxChannel =
      UpdateAPNSVoipSandboxChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPNSVoipSandboxChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAPNSVoipSandboxChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAPNSVoipSandboxChannel where
  toJSON UpdateAPNSVoipSandboxChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "APNSVoipSandboxChannelRequest"
                  Lude..= apnsVoipSandboxChannelRequest
              )
          ]
      )

instance Lude.ToPath UpdateAPNSVoipSandboxChannel where
  toPath UpdateAPNSVoipSandboxChannel' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Lude.ToQuery UpdateAPNSVoipSandboxChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAPNSVoipSandboxChannelResponse' smart constructor.
data UpdateAPNSVoipSandboxChannelResponse = UpdateAPNSVoipSandboxChannelResponse'
  { apnsVoipSandboxChannelResponse :: APNSVoipSandboxChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsVoipSandboxChannelResponse' -
-- * 'responseStatus' - The response status code.
mkUpdateAPNSVoipSandboxChannelResponse ::
  -- | 'apnsVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAPNSVoipSandboxChannelResponse
mkUpdateAPNSVoipSandboxChannelResponse
  pAPNSVoipSandboxChannelResponse_
  pResponseStatus_ =
    UpdateAPNSVoipSandboxChannelResponse'
      { apnsVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscrsAPNSVoipSandboxChannelResponse :: Lens.Lens' UpdateAPNSVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
uavscrsAPNSVoipSandboxChannelResponse = Lens.lens (apnsVoipSandboxChannelResponse :: UpdateAPNSVoipSandboxChannelResponse -> APNSVoipSandboxChannelResponse) (\s a -> s {apnsVoipSandboxChannelResponse = a} :: UpdateAPNSVoipSandboxChannelResponse)
{-# DEPRECATED uavscrsAPNSVoipSandboxChannelResponse "Use generic-lens or generic-optics with 'apnsVoipSandboxChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavscrsResponseStatus :: Lens.Lens' UpdateAPNSVoipSandboxChannelResponse Lude.Int
uavscrsResponseStatus = Lens.lens (responseStatus :: UpdateAPNSVoipSandboxChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPNSVoipSandboxChannelResponse)
{-# DEPRECATED uavscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
