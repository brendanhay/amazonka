{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetApplicationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the settings for an application.
module Network.AWS.Pinpoint.GetApplicationSettings
  ( -- * Creating a request
    GetApplicationSettings (..),
    mkGetApplicationSettings,

    -- ** Request lenses
    gasApplicationId,

    -- * Destructuring the response
    GetApplicationSettingsResponse (..),
    mkGetApplicationSettingsResponse,

    -- ** Response lenses
    gassrsApplicationSettingsResource,
    gassrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetApplicationSettings' smart constructor.
newtype GetApplicationSettings = GetApplicationSettings'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationSettings' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
mkGetApplicationSettings ::
  -- | 'applicationId'
  Lude.Text ->
  GetApplicationSettings
mkGetApplicationSettings pApplicationId_ =
  GetApplicationSettings' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gasApplicationId :: Lens.Lens' GetApplicationSettings Lude.Text
gasApplicationId = Lens.lens (applicationId :: GetApplicationSettings -> Lude.Text) (\s a -> s {applicationId = a} :: GetApplicationSettings)
{-# DEPRECATED gasApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetApplicationSettings where
  type Rs GetApplicationSettings = GetApplicationSettingsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApplicationSettingsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApplicationSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetApplicationSettings where
  toPath GetApplicationSettings' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/settings"]

instance Lude.ToQuery GetApplicationSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetApplicationSettingsResponse' smart constructor.
data GetApplicationSettingsResponse = GetApplicationSettingsResponse'
  { applicationSettingsResource :: ApplicationSettingsResource,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- * 'applicationSettingsResource' -
-- * 'responseStatus' - The response status code.
mkGetApplicationSettingsResponse ::
  -- | 'applicationSettingsResource'
  ApplicationSettingsResource ->
  -- | 'responseStatus'
  Lude.Int ->
  GetApplicationSettingsResponse
mkGetApplicationSettingsResponse
  pApplicationSettingsResource_
  pResponseStatus_ =
    GetApplicationSettingsResponse'
      { applicationSettingsResource =
          pApplicationSettingsResource_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'applicationSettingsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gassrsApplicationSettingsResource :: Lens.Lens' GetApplicationSettingsResponse ApplicationSettingsResource
gassrsApplicationSettingsResource = Lens.lens (applicationSettingsResource :: GetApplicationSettingsResponse -> ApplicationSettingsResource) (\s a -> s {applicationSettingsResource = a} :: GetApplicationSettingsResponse)
{-# DEPRECATED gassrsApplicationSettingsResource "Use generic-lens or generic-optics with 'applicationSettingsResource' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gassrsResponseStatus :: Lens.Lens' GetApplicationSettingsResponse Lude.Int
gassrsResponseStatus = Lens.lens (responseStatus :: GetApplicationSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApplicationSettingsResponse)
{-# DEPRECATED gassrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
