{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteSmsChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the SMS channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteSmsChannel
  ( -- * Creating a request
    DeleteSmsChannel (..),
    mkDeleteSmsChannel,

    -- ** Request lenses
    dscApplicationId,

    -- * Destructuring the response
    DeleteSmsChannelResponse (..),
    mkDeleteSmsChannelResponse,

    -- ** Response lenses
    dscrsSMSChannelResponse,
    dscrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSmsChannel' smart constructor.
newtype DeleteSmsChannel = DeleteSmsChannel'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSmsChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteSmsChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteSmsChannel
mkDeleteSmsChannel pApplicationId_ =
  DeleteSmsChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscApplicationId :: Lens.Lens' DeleteSmsChannel Lude.Text
dscApplicationId = Lens.lens (applicationId :: DeleteSmsChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteSmsChannel)
{-# DEPRECATED dscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteSmsChannel where
  type Rs DeleteSmsChannel = DeleteSmsChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSmsChannelResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSmsChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSmsChannel where
  toPath DeleteSmsChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/sms"]

instance Lude.ToQuery DeleteSmsChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSmsChannelResponse' smart constructor.
data DeleteSmsChannelResponse = DeleteSmsChannelResponse'
  { sMSChannelResponse :: SMSChannelResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSmsChannelResponse' with the minimum fields required to make a request.
--
-- * 'sMSChannelResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteSmsChannelResponse ::
  -- | 'sMSChannelResponse'
  SMSChannelResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSmsChannelResponse
mkDeleteSmsChannelResponse pSMSChannelResponse_ pResponseStatus_ =
  DeleteSmsChannelResponse'
    { sMSChannelResponse =
        pSMSChannelResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sMSChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsSMSChannelResponse :: Lens.Lens' DeleteSmsChannelResponse SMSChannelResponse
dscrsSMSChannelResponse = Lens.lens (sMSChannelResponse :: DeleteSmsChannelResponse -> SMSChannelResponse) (\s a -> s {sMSChannelResponse = a} :: DeleteSmsChannelResponse)
{-# DEPRECATED dscrsSMSChannelResponse "Use generic-lens or generic-optics with 'sMSChannelResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrsResponseStatus :: Lens.Lens' DeleteSmsChannelResponse Lude.Int
dscrsResponseStatus = Lens.lens (responseStatus :: DeleteSmsChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSmsChannelResponse)
{-# DEPRECATED dscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
