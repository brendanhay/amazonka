{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteADMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the ADM channel for an application and deletes any existing settings for the channel.
module Network.AWS.Pinpoint.DeleteADMChannel
  ( -- * Creating a request
    DeleteADMChannel (..),
    mkDeleteADMChannel,

    -- ** Request lenses
    dadmcApplicationId,

    -- * Destructuring the response
    DeleteADMChannelResponse (..),
    mkDeleteADMChannelResponse,

    -- ** Response lenses
    dadmcrsResponseStatus,
    dadmcrsADMChannelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteADMChannel' smart constructor.
newtype DeleteADMChannel = DeleteADMChannel'
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

-- | Creates a value of 'DeleteADMChannel' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkDeleteADMChannel ::
  -- | 'applicationId'
  Lude.Text ->
  DeleteADMChannel
mkDeleteADMChannel pApplicationId_ =
  DeleteADMChannel' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadmcApplicationId :: Lens.Lens' DeleteADMChannel Lude.Text
dadmcApplicationId = Lens.lens (applicationId :: DeleteADMChannel -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteADMChannel)
{-# DEPRECATED dadmcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest DeleteADMChannel where
  type Rs DeleteADMChannel = DeleteADMChannelResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteADMChannelResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders DeleteADMChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteADMChannel where
  toPath DeleteADMChannel' {..} =
    Lude.mconcat
      ["/v1/apps/", Lude.toBS applicationId, "/channels/adm"]

instance Lude.ToQuery DeleteADMChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteADMChannelResponse' smart constructor.
data DeleteADMChannelResponse = DeleteADMChannelResponse'
  { responseStatus ::
      Lude.Int,
    aDMChannelResponse :: ADMChannelResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteADMChannelResponse' with the minimum fields required to make a request.
--
-- * 'aDMChannelResponse' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDeleteADMChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'aDMChannelResponse'
  ADMChannelResponse ->
  DeleteADMChannelResponse
mkDeleteADMChannelResponse pResponseStatus_ pADMChannelResponse_ =
  DeleteADMChannelResponse'
    { responseStatus = pResponseStatus_,
      aDMChannelResponse = pADMChannelResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadmcrsResponseStatus :: Lens.Lens' DeleteADMChannelResponse Lude.Int
dadmcrsResponseStatus = Lens.lens (responseStatus :: DeleteADMChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteADMChannelResponse)
{-# DEPRECATED dadmcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aDMChannelResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dadmcrsADMChannelResponse :: Lens.Lens' DeleteADMChannelResponse ADMChannelResponse
dadmcrsADMChannelResponse = Lens.lens (aDMChannelResponse :: DeleteADMChannelResponse -> ADMChannelResponse) (\s a -> s {aDMChannelResponse = a} :: DeleteADMChannelResponse)
{-# DEPRECATED dadmcrsADMChannelResponse "Use generic-lens or generic-optics with 'aDMChannelResponse' instead." #-}
