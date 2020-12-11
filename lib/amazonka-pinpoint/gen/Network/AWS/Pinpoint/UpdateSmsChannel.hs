{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateSmsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the SMS channel for an application or updates the status and settings of the SMS channel for an application.
module Network.AWS.Pinpoint.UpdateSmsChannel
  ( -- * Creating a request
    UpdateSmsChannel (..),
    mkUpdateSmsChannel,

    -- ** Request lenses
    uscApplicationId,
    uscSMSChannelRequest,

    -- * Destructuring the response
    UpdateSmsChannelResponse (..),
    mkUpdateSmsChannelResponse,

    -- ** Response lenses
    uscrsResponseStatus,
    uscrsSMSChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSmsChannel' smart constructor.
data UpdateSmsChannel = UpdateSmsChannel'
  { applicationId ::
      Lude.Text,
    sMSChannelRequest :: SMSChannelRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSmsChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'sMSChannelRequest' - Undocumented field.
mkUpdateSmsChannel ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'sMSChannelRequest'
  SMSChannelRequest ->
  UpdateSmsChannel
mkUpdateSmsChannel pApplicationId_ pSMSChannelRequest_ =
  UpdateSmsChannel'
    { applicationId = pApplicationId_,
      sMSChannelRequest = pSMSChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscApplicationId :: Lens.Lens' UpdateSmsChannel Lude.Text
uscApplicationId = Lens.lens (applicationId :: UpdateSmsChannel -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateSmsChannel)
{-# DEPRECATED uscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscSMSChannelRequest :: Lens.Lens' UpdateSmsChannel SMSChannelRequest
uscSMSChannelRequest = Lens.lens (sMSChannelRequest :: UpdateSmsChannel -> SMSChannelRequest) (\s a -> s {sMSChannelRequest = a} :: UpdateSmsChannel)
{-# DEPRECATED uscSMSChannelRequest "Use generic-lens or generic-optics with 'sMSChannelRequest' instead." #-}

instance Lude.AWSRequest UpdateSmsChannel where
  type Rs UpdateSmsChannel = UpdateSmsChannelResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSmsChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateSmsChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSmsChannel where
  toJSON UpdateSmsChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SMSChannelRequest" Lude..= sMSChannelRequest)]
      )

instance Lude.ToPath UpdateSmsChannel where
  toPath UpdateSmsChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/sms"]

instance Lude.ToQuery UpdateSmsChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSmsChannelResponse' smart constructor.
data UpdateSmsChannelResponse = UpdateSmsChannelResponse'
  { responseStatus ::
      Lude.Int,
    sMSChannelResponse :: SMSChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSmsChannelResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'sMSChannelResponse' - Undocumented field.
mkUpdateSmsChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  UpdateSmsChannelResponse
mkUpdateSmsChannelResponse pResponseStatus_ pSMSChannelResponse_ =
  UpdateSmsChannelResponse'
    { responseStatus = pResponseStatus_,
      sMSChannelResponse = pSMSChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsResponseStatus :: Lens.Lens' UpdateSmsChannelResponse Lude.Int
uscrsResponseStatus = Lens.lens (responseStatus :: UpdateSmsChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSmsChannelResponse)
{-# DEPRECATED uscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscrsSMSChannelResponse :: Lens.Lens' UpdateSmsChannelResponse SMSChannelResponse
uscrsSMSChannelResponse = Lens.lens (sMSChannelResponse :: UpdateSmsChannelResponse -> SMSChannelResponse) (\s a -> s {sMSChannelResponse = a} :: UpdateSmsChannelResponse)
{-# DEPRECATED uscrsSMSChannelResponse "Use generic-lens or generic-optics with 'sMSChannelResponse' instead." #-}
