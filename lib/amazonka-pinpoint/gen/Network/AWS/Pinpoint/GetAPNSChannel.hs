{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetAPNSChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs channel for an application.
module Network.AWS.Pinpoint.GetAPNSChannel
  ( -- * Creating a request
    GetAPNSChannel (..),
    mkGetAPNSChannel,

    -- ** Request lenses
    gacApplicationId,

    -- * Destructuring the response
    GetAPNSChannelResponse (..),
    mkGetAPNSChannelResponse,

    -- ** Response lenses
    gacrsResponseStatus,
    gacrsAPNSChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAPNSChannel' smart constructor.
newtype GetAPNSChannel = GetAPNSChannel'
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

-- | Creates a value of 'GetAPNSChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetAPNSChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetAPNSChannel
mkGetAPNSChannel pApplicationId_ =
  GetAPNSChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacApplicationId :: Lens.Lens' GetAPNSChannel Lude.Text
gacApplicationId = Lens.lens (applicationId :: GetAPNSChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetAPNSChannel)
{-# DEPRECATED gacApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetAPNSChannel where
  type Rs GetAPNSChannel = GetAPNSChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAPNSChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetAPNSChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetAPNSChannel where
  toPath GetAPNSChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/apns"]

instance Lude.ToQuery GetAPNSChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAPNSChannelResponse' smart constructor.
data GetAPNSChannelResponse = GetAPNSChannelResponse'
  { responseStatus ::
      Lude.Int,
    apnsChannelResponse :: APNSChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPNSChannelResponse' with the minimum fields required to make a request.
--
-- * 'apnsChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetAPNSChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'apnsChannelResponse'
  APNSChannelResponse ->
  GetAPNSChannelResponse
mkGetAPNSChannelResponse pResponseStatus_ pAPNSChannelResponse_ =
  GetAPNSChannelResponse'
    { responseStatus = pResponseStatus_,
      apnsChannelResponse = pAPNSChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrsResponseStatus :: Lens.Lens' GetAPNSChannelResponse Lude.Int
gacrsResponseStatus = Lens.lens (responseStatus :: GetAPNSChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAPNSChannelResponse)
{-# DEPRECATED gacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apnsChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrsAPNSChannelResponse :: Lens.Lens' GetAPNSChannelResponse APNSChannelResponse
gacrsAPNSChannelResponse = Lens.lens (apnsChannelResponse :: GetAPNSChannelResponse -> APNSChannelResponse) (\s a -> s {apnsChannelResponse = a} :: GetAPNSChannelResponse)
{-# DEPRECATED gacrsAPNSChannelResponse "Use generic-lens or generic-optics with 'apnsChannelResponse' instead." #-}
