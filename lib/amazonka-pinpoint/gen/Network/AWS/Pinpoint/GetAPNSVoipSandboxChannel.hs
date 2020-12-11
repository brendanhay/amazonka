{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP sandbox channel for an application.
module Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel
  ( -- * Creating a request
    GetAPNSVoipSandboxChannel (..),
    mkGetAPNSVoipSandboxChannel,

    -- ** Request lenses
    gavscApplicationId,

    -- * Destructuring the response
    GetAPNSVoipSandboxChannelResponse (..),
    mkGetAPNSVoipSandboxChannelResponse,

    -- ** Response lenses
    gavscrsResponseStatus,
    gavscrsAPNSVoipSandboxChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAPNSVoipSandboxChannel' smart constructor.
newtype GetAPNSVoipSandboxChannel = GetAPNSVoipSandboxChannel'
  { applicationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPNSVoipSandboxChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetAPNSVoipSandboxChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetAPNSVoipSandboxChannel
mkGetAPNSVoipSandboxChannel pApplicationId_ =
  GetAPNSVoipSandboxChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavscApplicationId :: Lens.Lens' GetAPNSVoipSandboxChannel Lude.Text
gavscApplicationId = Lens.lens (applicationId :: GetAPNSVoipSandboxChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetAPNSVoipSandboxChannel)
{-# DEPRECATED gavscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetAPNSVoipSandboxChannel where
  type
    Rs GetAPNSVoipSandboxChannel =
      GetAPNSVoipSandboxChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAPNSVoipSandboxChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetAPNSVoipSandboxChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetAPNSVoipSandboxChannel where
  toPath GetAPNSVoipSandboxChannel' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/channels/apns_voip_sandbox"
      ]

instance Lude.ToQuery GetAPNSVoipSandboxChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAPNSVoipSandboxChannelResponse' smart constructor.
data GetAPNSVoipSandboxChannelResponse = GetAPNSVoipSandboxChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsVoipSandboxChannelResponse ::
      APNSVoipSandboxChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsVoipSandboxChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetAPNSVoipSandboxChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  GetAPNSVoipSandboxChannelResponse
mkGetAPNSVoipSandboxChannelResponse
  pResponseStatus_
  pAPNSVoipSandboxChannelResponse_ =
    GetAPNSVoipSandboxChannelResponse'
      { responseStatus =
          pResponseStatus_,
        apnsVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavscrsResponseStatus :: Lens.Lens' GetAPNSVoipSandboxChannelResponse Lude.Int
gavscrsResponseStatus = Lens.lens (responseStatus :: GetAPNSVoipSandboxChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAPNSVoipSandboxChannelResponse)
{-# DEPRECATED gavscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavscrsAPNSVoipSandboxChannelResponse :: Lens.Lens' GetAPNSVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
gavscrsAPNSVoipSandboxChannelResponse = Lens.lens (apnsVoipSandboxChannelResponse :: GetAPNSVoipSandboxChannelResponse -> APNSVoipSandboxChannelResponse) (\s a -> s {apnsVoipSandboxChannelResponse = a} :: GetAPNSVoipSandboxChannelResponse)
{-# DEPRECATED gavscrsAPNSVoipSandboxChannelResponse "Use generic-lens or generic-optics with 'apnsVoipSandboxChannelResponse' instead." #-}
