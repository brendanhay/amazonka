{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetAPNSVoipChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP channel for an application.
module Network.AWS.Pinpoint.GetAPNSVoipChannel
  ( -- * Creating a request
    GetAPNSVoipChannel (..),
    mkGetAPNSVoipChannel,

    -- ** Request lenses
    gavcApplicationId,

    -- * Destructuring the response
    GetAPNSVoipChannelResponse (..),
    mkGetAPNSVoipChannelResponse,

    -- ** Response lenses
    gavcrsAPNSVoipChannelResponse,
    gavcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAPNSVoipChannel' smart constructor.
newtype GetAPNSVoipChannel = GetAPNSVoipChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPNSVoipChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetAPNSVoipChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetAPNSVoipChannel
mkGetAPNSVoipChannel pApplicationId_ =
  GetAPNSVoipChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcApplicationId :: Lens.Lens' GetAPNSVoipChannel Lude.Text
gavcApplicationId = Lens.lens (applicationId :: GetAPNSVoipChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetAPNSVoipChannel)
{-# DEPRECATED gavcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetAPNSVoipChannel where
  type Rs GetAPNSVoipChannel = GetAPNSVoipChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAPNSVoipChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAPNSVoipChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetAPNSVoipChannel where
  toPath GetAPNSVoipChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns_voip"]

instance Lude.ToQuery GetAPNSVoipChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAPNSVoipChannelResponse' smart constructor.
data GetAPNSVoipChannelResponse = GetAPNSVoipChannelResponse'
  { apnsVoipChannelResponse :: APNSVoipChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsVoipChannelResponse' -
-- * 'responseStatus' - The response status code.
mkGetAPNSVoipChannelResponse ::
  -- | 'apnsVoipChannelResponse'
  APNSVoipChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetAPNSVoipChannelResponse
mkGetAPNSVoipChannelResponse
  pAPNSVoipChannelResponse_
  pResponseStatus_ =
    GetAPNSVoipChannelResponse'
      { apnsVoipChannelResponse =
          pAPNSVoipChannelResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsVoipChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrsAPNSVoipChannelResponse :: Lens.Lens' GetAPNSVoipChannelResponse APNSVoipChannelResponse
gavcrsAPNSVoipChannelResponse = Lens.lens (apnsVoipChannelResponse :: GetAPNSVoipChannelResponse -> APNSVoipChannelResponse) (\s a -> s {apnsVoipChannelResponse = a} :: GetAPNSVoipChannelResponse)
{-# DEPRECATED gavcrsAPNSVoipChannelResponse "Use generic-lens or generic-optics with 'apnsVoipChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrsResponseStatus :: Lens.Lens' GetAPNSVoipChannelResponse Lude.Int
gavcrsResponseStatus = Lens.lens (responseStatus :: GetAPNSVoipChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAPNSVoipChannelResponse)
{-# DEPRECATED gavcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
