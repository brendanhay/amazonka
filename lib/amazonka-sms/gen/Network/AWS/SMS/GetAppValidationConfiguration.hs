{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a configuration for validating an application.
module Network.AWS.SMS.GetAppValidationConfiguration
  ( -- * Creating a request
    GetAppValidationConfiguration (..),
    mkGetAppValidationConfiguration,

    -- ** Request lenses
    gavcAppId,

    -- * Destructuring the response
    GetAppValidationConfigurationResponse (..),
    mkGetAppValidationConfigurationResponse,

    -- ** Response lenses
    gavcrsServerGroupValidationConfigurations,
    gavcrsAppValidationConfigurations,
    gavcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetAppValidationConfiguration' smart constructor.
newtype GetAppValidationConfiguration = GetAppValidationConfiguration'
  { appId ::
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

-- | Creates a value of 'GetAppValidationConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkGetAppValidationConfiguration ::
  -- | 'appId'
  Lude.Text ->
  GetAppValidationConfiguration
mkGetAppValidationConfiguration pAppId_ =
  GetAppValidationConfiguration' {appId = pAppId_}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcAppId :: Lens.Lens' GetAppValidationConfiguration Lude.Text
gavcAppId = Lens.lens (appId :: GetAppValidationConfiguration -> Lude.Text) (\s a -> s {appId = a} :: GetAppValidationConfiguration)
{-# DEPRECATED gavcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest GetAppValidationConfiguration where
  type
    Rs GetAppValidationConfiguration =
      GetAppValidationConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppValidationConfigurationResponse'
            Lude.<$> ( x Lude..?> "serverGroupValidationConfigurations"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "appValidationConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAppValidationConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetAppValidationConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAppValidationConfiguration where
  toJSON GetAppValidationConfiguration' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("appId" Lude..= appId)])

instance Lude.ToPath GetAppValidationConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAppValidationConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppValidationConfigurationResponse' smart constructor.
data GetAppValidationConfigurationResponse = GetAppValidationConfigurationResponse'
  { serverGroupValidationConfigurations ::
      Lude.Maybe
        [ServerGroupValidationConfiguration],
    appValidationConfigurations ::
      Lude.Maybe
        [AppValidationConfiguration],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppValidationConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'appValidationConfigurations' - The configuration for application validation.
-- * 'responseStatus' - The response status code.
-- * 'serverGroupValidationConfigurations' - The configuration for instance validation.
mkGetAppValidationConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppValidationConfigurationResponse
mkGetAppValidationConfigurationResponse pResponseStatus_ =
  GetAppValidationConfigurationResponse'
    { serverGroupValidationConfigurations =
        Lude.Nothing,
      appValidationConfigurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The configuration for instance validation.
--
-- /Note:/ Consider using 'serverGroupValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrsServerGroupValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Lude.Maybe [ServerGroupValidationConfiguration])
gavcrsServerGroupValidationConfigurations = Lens.lens (serverGroupValidationConfigurations :: GetAppValidationConfigurationResponse -> Lude.Maybe [ServerGroupValidationConfiguration]) (\s a -> s {serverGroupValidationConfigurations = a} :: GetAppValidationConfigurationResponse)
{-# DEPRECATED gavcrsServerGroupValidationConfigurations "Use generic-lens or generic-optics with 'serverGroupValidationConfigurations' instead." #-}

-- | The configuration for application validation.
--
-- /Note:/ Consider using 'appValidationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrsAppValidationConfigurations :: Lens.Lens' GetAppValidationConfigurationResponse (Lude.Maybe [AppValidationConfiguration])
gavcrsAppValidationConfigurations = Lens.lens (appValidationConfigurations :: GetAppValidationConfigurationResponse -> Lude.Maybe [AppValidationConfiguration]) (\s a -> s {appValidationConfigurations = a} :: GetAppValidationConfigurationResponse)
{-# DEPRECATED gavcrsAppValidationConfigurations "Use generic-lens or generic-optics with 'appValidationConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gavcrsResponseStatus :: Lens.Lens' GetAppValidationConfigurationResponse Lude.Int
gavcrsResponseStatus = Lens.lens (responseStatus :: GetAppValidationConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppValidationConfigurationResponse)
{-# DEPRECATED gavcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
