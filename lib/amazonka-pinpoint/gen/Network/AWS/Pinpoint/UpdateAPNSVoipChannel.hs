{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateAPNSVoipChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs VoIP channel for an application or updates the status and settings of the APNs VoIP channel for an application.
module Network.AWS.Pinpoint.UpdateAPNSVoipChannel
  ( -- * Creating a request
    UpdateAPNSVoipChannel (..),
    mkUpdateAPNSVoipChannel,

    -- ** Request lenses
    uavcApplicationId,
    uavcAPNSVoipChannelRequest,

    -- * Destructuring the response
    UpdateAPNSVoipChannelResponse (..),
    mkUpdateAPNSVoipChannelResponse,

    -- ** Response lenses
    uavcrsResponseStatus,
    uavcrsAPNSVoipChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAPNSVoipChannel' smart constructor.
data UpdateAPNSVoipChannel = UpdateAPNSVoipChannel'
  { applicationId ::
      Lude.Text,
    apnsVoipChannelRequest ::
      APNSVoipChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSVoipChannel' with the minimum fields required to make a request.
--
-- * 'apnsVoipChannelRequest' - Undocumented field.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkUpdateAPNSVoipChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'apnsVoipChannelRequest'
  APNSVoipChannelRequest ->
  UpdateAPNSVoipChannel
mkUpdateAPNSVoipChannel pApplicationId_ pAPNSVoipChannelRequest_ =
  UpdateAPNSVoipChannel'
    { applicationId = pApplicationId_,
      apnsVoipChannelRequest = pAPNSVoipChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcApplicationId :: Lens.Lens' UpdateAPNSVoipChannel Lude.Text
uavcApplicationId = Lens.lens (applicationId :: UpdateAPNSVoipChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateAPNSVoipChannel)
{-# DEPRECATED uavcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcAPNSVoipChannelRequest :: Lens.Lens' UpdateAPNSVoipChannel APNSVoipChannelRequest
uavcAPNSVoipChannelRequest = Lens.lens (apnsVoipChannelRequest :: UpdateAPNSVoipChannel -> APNSVoipChannelRequest) (\s a -> s {apnsVoipChannelRequest = a} :: UpdateAPNSVoipChannel)
{-# DEPRECATED uavcAPNSVoipChannelRequest "Use generic-lens or generic-optics with 'apnsVoipChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateAPNSVoipChannel where
  type Rs UpdateAPNSVoipChannel = UpdateAPNSVoipChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAPNSVoipChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateAPNSVoipChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAPNSVoipChannel where
  toJSON UpdateAPNSVoipChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("APNSVoipChannelRequest" Lude..= apnsVoipChannelRequest)
          ]
      )

instance Lude.ToPath UpdateAPNSVoipChannel where
  toPath UpdateAPNSVoipChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns_voip"]

instance Lude.ToQuery UpdateAPNSVoipChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAPNSVoipChannelResponse' smart constructor.
data UpdateAPNSVoipChannelResponse = UpdateAPNSVoipChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsVoipChannelResponse ::
      APNSVoipChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAPNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsVoipChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateAPNSVoipChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsVoipChannelResponse'
  APNSVoipChannelResponse ->
  UpdateAPNSVoipChannelResponse
mkUpdateAPNSVoipChannelResponse
  pResponseStatus_
  pAPNSVoipChannelResponse_ =
    UpdateAPNSVoipChannelResponse'
      { responseStatus = pResponseStatus_,
        apnsVoipChannelResponse = pAPNSVoipChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcrsResponseStatus :: Lens.Lens' UpdateAPNSVoipChannelResponse Lude.Int
uavcrsResponseStatus = Lens.lens (responseStatus :: UpdateAPNSVoipChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAPNSVoipChannelResponse)
{-# DEPRECATED uavcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uavcrsAPNSVoipChannelResponse :: Lens.Lens' UpdateAPNSVoipChannelResponse APNSVoipChannelResponse
uavcrsAPNSVoipChannelResponse = Lens.lens (apnsVoipChannelResponse :: UpdateAPNSVoipChannelResponse -> APNSVoipChannelResponse) (\s a -> s {apnsVoipChannelResponse = a} :: UpdateAPNSVoipChannelResponse)
{-# DEPRECATED uavcrsAPNSVoipChannelResponse "Use generic-lens or generic-optics with 'apnsVoipChannelResponse' instead." #-}
