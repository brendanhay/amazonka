{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSmsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the SMS channel for an application.
module Network.AWS.Pinpoint.GetSmsChannel
  ( -- * Creating a request
    GetSmsChannel (..),
    mkGetSmsChannel,

    -- ** Request lenses
    gscApplicationId,

    -- * Destructuring the response
    GetSmsChannelResponse (..),
    mkGetSmsChannelResponse,

    -- ** Response lenses
    gscrsSMSChannelResponse,
    gscrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSmsChannel' smart constructor.
newtype GetSmsChannel = GetSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSmsChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetSmsChannel ::
  -- | 'applicationId'
  Lude.Text ->
  GetSmsChannel
mkGetSmsChannel pApplicationId_ =
  GetSmsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscApplicationId :: Lens.Lens' GetSmsChannel Lude.Text
gscApplicationId = Lens.lens (applicationId :: GetSmsChannel -> Lude.Text) (\s a -> s {applicationId = a} :: GetSmsChannel)
{-# DEPRECATED gscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetSmsChannel where
  type Rs GetSmsChannel = GetSmsChannelResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSmsChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSmsChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSmsChannel where
  toPath GetSmsChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/sms"]

instance Lude.ToQuery GetSmsChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSmsChannelResponse' smart constructor.
data GetSmsChannelResponse = GetSmsChannelResponse'
  { sMSChannelResponse :: SMSChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSmsChannelResponse' with the minimum fields required to make a request.
--
-- * 'sMSChannelResponse' -
-- * 'responseStatus' - The response status code.
mkGetSmsChannelResponse ::
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSmsChannelResponse
mkGetSmsChannelResponse pSMSChannelResponse_ pResponseStatus_ =
  GetSmsChannelResponse'
    { sMSChannelResponse = pSMSChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsSMSChannelResponse :: Lens.Lens' GetSmsChannelResponse SMSChannelResponse
gscrsSMSChannelResponse = Lens.lens (sMSChannelResponse :: GetSmsChannelResponse -> SMSChannelResponse) (\s a -> s {sMSChannelResponse = a} :: GetSmsChannelResponse)
{-# DEPRECATED gscrsSMSChannelResponse "Use generic-lens or generic-optics with 'sMSChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrsResponseStatus :: Lens.Lens' GetSmsChannelResponse Lude.Int
gscrsResponseStatus = Lens.lens (responseStatus :: GetSmsChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSmsChannelResponse)
{-# DEPRECATED gscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
