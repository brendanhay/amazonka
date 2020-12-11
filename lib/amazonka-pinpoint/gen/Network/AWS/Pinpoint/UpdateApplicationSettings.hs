{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateApplicationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for an application.
module Network.AWS.Pinpoint.UpdateApplicationSettings
  ( -- * Creating a request
    UpdateApplicationSettings (..),
    mkUpdateApplicationSettings,

    -- ** Request lenses
    uasApplicationId,
    uasWriteApplicationSettingsRequest,

    -- * Destructuring the response
    UpdateApplicationSettingsResponse (..),
    mkUpdateApplicationSettingsResponse,

    -- ** Response lenses
    uasrsResponseStatus,
    uasrsApplicationSettingsResource,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApplicationSettings' smart constructor.
data UpdateApplicationSettings = UpdateApplicationSettings'
  { applicationId ::
      Lude.Text,
    writeApplicationSettingsRequest ::
      WriteApplicationSettingsRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplicationSettings' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'writeApplicationSettingsRequest' - Undocumented field.
mkUpdateApplicationSettings ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'writeApplicationSettingsRequest'
  WriteApplicationSettingsRequest ->
  UpdateApplicationSettings
mkUpdateApplicationSettings
  pApplicationId_
  pWriteApplicationSettingsRequest_ =
    UpdateApplicationSettings'
      { applicationId = pApplicationId_,
        writeApplicationSettingsRequest =
          pWriteApplicationSettingsRequest_
      }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasApplicationId :: Lens.Lens' UpdateApplicationSettings Lude.Text
uasApplicationId = Lens.lens (applicationId :: UpdateApplicationSettings -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateApplicationSettings)
{-# DEPRECATED uasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeApplicationSettingsRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasWriteApplicationSettingsRequest :: Lens.Lens' UpdateApplicationSettings WriteApplicationSettingsRequest
uasWriteApplicationSettingsRequest = Lens.lens (writeApplicationSettingsRequest :: UpdateApplicationSettings -> WriteApplicationSettingsRequest) (\s a -> s {writeApplicationSettingsRequest = a} :: UpdateApplicationSettings)
{-# DEPRECATED uasWriteApplicationSettingsRequest "Use generic-lens or generic-optics with 'writeApplicationSettingsRequest' instead." #-}

instance Lude.AWSRequest UpdateApplicationSettings where
  type
    Rs UpdateApplicationSettings =
      UpdateApplicationSettingsResponse
  request = Req.putJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateApplicationSettingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders UpdateApplicationSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApplicationSettings where
  toJSON UpdateApplicationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "WriteApplicationSettingsRequest"
                  Lude..= writeApplicationSettingsRequest
              )
          ]
      )

instance Lude.ToPath UpdateApplicationSettings where
  toPath UpdateApplicationSettings' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/settings"]

instance Lude.ToQuery UpdateApplicationSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApplicationSettingsResponse' smart constructor.
data UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse'
  { responseStatus ::
      Lude.Int,
    applicationSettingsResource ::
      ApplicationSettingsResource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- * 'applicationSettingsResource' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateApplicationSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'applicationSettingsResource'
  ApplicationSettingsResource ->
  UpdateApplicationSettingsResponse
mkUpdateApplicationSettingsResponse
  pResponseStatus_
  pApplicationSettingsResource_ =
    UpdateApplicationSettingsResponse'
      { responseStatus =
          pResponseStatus_,
        applicationSettingsResource = pApplicationSettingsResource_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsResponseStatus :: Lens.Lens' UpdateApplicationSettingsResponse Lude.Int
uasrsResponseStatus = Lens.lens (responseStatus :: UpdateApplicationSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApplicationSettingsResponse)
{-# DEPRECATED uasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationSettingsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsApplicationSettingsResource :: Lens.Lens' UpdateApplicationSettingsResponse ApplicationSettingsResource
uasrsApplicationSettingsResource = Lens.lens (applicationSettingsResource :: UpdateApplicationSettingsResponse -> ApplicationSettingsResource) (\s a -> s {applicationSettingsResource = a} :: UpdateApplicationSettingsResponse)
{-# DEPRECATED uasrsApplicationSettingsResource "Use generic-lens or generic-optics with 'applicationSettingsResource' instead." #-}
