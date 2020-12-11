{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetAPNSSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs sandbox channel for an application.
module Network.AWS.Pinpoint.GetAPNSSandboxChannel
  ( -- * Creating a request
    GetAPNSSandboxChannel (..),
    mkGetAPNSSandboxChannel,

    -- ** Request lenses
    gascApplicationId,

    -- * Destructuring the response
    GetAPNSSandboxChannelResponse (..),
    mkGetAPNSSandboxChannelResponse,

    -- ** Response lenses
    gascrsResponseStatus,
    gascrsAPNSSandboxChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAPNSSandboxChannel' smart constructor.
newtype GetAPNSSandboxChannel = GetAPNSSandboxChannel'
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

-- | Creates a value of 'GetAPNSSandboxChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetAPNSSandboxChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetAPNSSandboxChannel
mkGetAPNSSandboxChannel pApplicationId_ =
  GetAPNSSandboxChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gascApplicationId :: Lens.Lens' GetAPNSSandboxChannel Lude.Text
gascApplicationId = Lens.lens (applicationId :: GetAPNSSandboxChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetAPNSSandboxChannel)
{-# DEPRECATED gascApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetAPNSSandboxChannel where
  type Rs GetAPNSSandboxChannel = GetAPNSSandboxChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAPNSSandboxChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetAPNSSandboxChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetAPNSSandboxChannel where
  toPath GetAPNSSandboxChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns_sandbox"]

instance Lude.ToQuery GetAPNSSandboxChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAPNSSandboxChannelResponse' smart constructor.
data GetAPNSSandboxChannelResponse = GetAPNSSandboxChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsSandboxChannelResponse ::
      APNSSandboxChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsSandboxChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetAPNSSandboxChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  GetAPNSSandboxChannelResponse
mkGetAPNSSandboxChannelResponse
  pResponseStatus_
  pAPNSSandboxChannelResponse_ =
    GetAPNSSandboxChannelResponse'
      { responseStatus = pResponseStatus_,
        apnsSandboxChannelResponse = pAPNSSandboxChannelResponse_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gascrsResponseStatus :: Lens.Lens' GetAPNSSandboxChannelResponse Lude.Int
gascrsResponseStatus = Lens.lens (responseStatus :: GetAPNSSandboxChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAPNSSandboxChannelResponse)
{-# DEPRECATED gascrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsSandboxChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gascrsAPNSSandboxChannelResponse :: Lens.Lens' GetAPNSSandboxChannelResponse APNSSandboxChannelResponse
gascrsAPNSSandboxChannelResponse = Lens.lens (apnsSandboxChannelResponse :: GetAPNSSandboxChannelResponse -> APNSSandboxChannelResponse) (\s a -> s {apnsSandboxChannelResponse = a} :: GetAPNSSandboxChannelResponse)
{-# DEPRECATED gascrsAPNSSandboxChannelResponse "Use generic-lens or generic-optics with 'apnsSandboxChannelResponse' instead." #-}
