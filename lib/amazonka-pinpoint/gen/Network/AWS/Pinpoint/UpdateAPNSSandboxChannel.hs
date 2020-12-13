{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateAPNSSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs sandbox channel for an application or updates the status and settings of the APNs sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateAPNSSandboxChannel
  ( -- * Creating a request
    UpdateAPNSSandboxChannel (..),
    mkUpdateAPNSSandboxChannel,

    -- ** Request lenses
    uascApplicationId,
    uascAPNSSandboxChannelRequest,

    -- * Destructuring the response
    UpdateAPNSSandboxChannelResponse (..),
    mkUpdateAPNSSandboxChannelResponse,

    -- ** Response lenses
    uascrsAPNSSandboxChannelResponse,
    uascrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAPNSSandboxChannel' smart constructor.
data UpdateAPNSSandboxChannel = UpdateAPNSSandboxChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    apnsSandboxChannelRequest :: APNSSandboxChannelRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSSandboxChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'apnsSandboxChannelRequest' -
mkUpdateAPNSSandboxChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'apnsSandboxChannelRequest'
  APNSSandboxChannelRequest ->
  UpdateAPNSSandboxChannel
mkUpdateAPNSSandboxChannel
  pApplicationId_
  pAPNSSandboxChannelRequest_ =
    UpdateAPNSSandboxChannel'
      { applicationId = pApplicationId_,
        apnsSandboxChannelRequest = pAPNSSandboxChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascApplicationId :: Lens.Lens' UpdateAPNSSandboxChannel Lude.Text
uascApplicationId = Lens.lens (applicationId :: UpdateAPNSSandboxChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateAPNSSandboxChannel)
{-# DEPRECATED uascApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsSandboxChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascAPNSSandboxChannelRequest :: Lens.Lens' UpdateAPNSSandboxChannel APNSSandboxChannelRequest
uascAPNSSandboxChannelRequest = Lens.lens (apnsSandboxChannelRequest :: UpdateAPNSSandboxChannel -> APNSSandboxChannelRequest) (\s a -> s {apnsSandboxChannelRequest = a} :: UpdateAPNSSandboxChannel)
{-# DEPRECATED uascAPNSSandboxChannelRequest "Use generic-lens or generic-optics with 'apnsSandboxChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateAPNSSandboxChannel where
  type Rs UpdateAPNSSandboxChannel = UpdateAPNSSandboxChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPNSSandboxChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAPNSSandboxChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAPNSSandboxChannel where
  toJSON UpdateAPNSSandboxChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("APNSSandboxChannelRequest" Lude..= apnsSandboxChannelRequest)
          ]
      )

instance Lude.ToPath UpdateAPNSSandboxChannel where
  toPath UpdateAPNSSandboxChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns_sandbox"]

instance Lude.ToQuery UpdateAPNSSandboxChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAPNSSandboxChannelResponse' smart constructor.
data UpdateAPNSSandboxChannelResponse = UpdateAPNSSandboxChannelResponse'
  { apnsSandboxChannelResponse :: APNSSandboxChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsSandboxChannelResponse' -
-- * 'responseStatus' - The response status code.
mkUpdateAPNSSandboxChannelResponse ::
  -- | 'apnsSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAPNSSandboxChannelResponse
mkUpdateAPNSSandboxChannelResponse
  pAPNSSandboxChannelResponse_
  pResponseStatus_ =
    UpdateAPNSSandboxChannelResponse'
      { apnsSandboxChannelResponse =
          pAPNSSandboxChannelResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascrsAPNSSandboxChannelResponse :: Lens.Lens' UpdateAPNSSandboxChannelResponse APNSSandboxChannelResponse
uascrsAPNSSandboxChannelResponse = Lens.lens (apnsSandboxChannelResponse :: UpdateAPNSSandboxChannelResponse -> APNSSandboxChannelResponse) (\s a -> s {apnsSandboxChannelResponse = a} :: UpdateAPNSSandboxChannelResponse)
{-# DEPRECATED uascrsAPNSSandboxChannelResponse "Use generic-lens or generic-optics with 'apnsSandboxChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uascrsResponseStatus :: Lens.Lens' UpdateAPNSSandboxChannelResponse Lude.Int
uascrsResponseStatus = Lens.lens (responseStatus :: UpdateAPNSSandboxChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPNSSandboxChannelResponse)
{-# DEPRECATED uascrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
