{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application launch configuration associated with the specified application.
module Network.AWS.SMS.GetAppLaunchConfiguration
  ( -- * Creating a request
    GetAppLaunchConfiguration (..),
    mkGetAppLaunchConfiguration,

    -- ** Request lenses
    galcAppId,

    -- * Destructuring the response
    GetAppLaunchConfigurationResponse (..),
    mkGetAppLaunchConfigurationResponse,

    -- ** Response lenses
    galcrsServerGroupLaunchConfigurations,
    galcrsAutoLaunch,
    galcrsRoleName,
    galcrsAppId,
    galcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetAppLaunchConfiguration' smart constructor.
newtype GetAppLaunchConfiguration = GetAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkGetAppLaunchConfiguration ::
  GetAppLaunchConfiguration
mkGetAppLaunchConfiguration =
  GetAppLaunchConfiguration' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcAppId :: Lens.Lens' GetAppLaunchConfiguration (Lude.Maybe Lude.Text)
galcAppId = Lens.lens (appId :: GetAppLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: GetAppLaunchConfiguration)
{-# DEPRECATED galcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest GetAppLaunchConfiguration where
  type
    Rs GetAppLaunchConfiguration =
      GetAppLaunchConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppLaunchConfigurationResponse'
            Lude.<$> (x Lude..?> "serverGroupLaunchConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "autoLaunch")
            Lude.<*> (x Lude..?> "roleName")
            Lude.<*> (x Lude..?> "appId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAppLaunchConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetAppLaunchConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAppLaunchConfiguration where
  toJSON GetAppLaunchConfiguration' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath GetAppLaunchConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAppLaunchConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppLaunchConfigurationResponse' smart constructor.
data GetAppLaunchConfigurationResponse = GetAppLaunchConfigurationResponse'
  { -- | The launch configurations for server groups in this application.
    serverGroupLaunchConfigurations :: Lude.Maybe [ServerGroupLaunchConfiguration],
    -- | Indicates whether the application is configured to launch automatically after replication is complete.
    autoLaunch :: Lude.Maybe Lude.Bool,
    -- | The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
    roleName :: Lude.Maybe Lude.Text,
    -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'serverGroupLaunchConfigurations' - The launch configurations for server groups in this application.
-- * 'autoLaunch' - Indicates whether the application is configured to launch automatically after replication is complete.
-- * 'roleName' - The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
-- * 'appId' - The ID of the application.
-- * 'responseStatus' - The response status code.
mkGetAppLaunchConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppLaunchConfigurationResponse
mkGetAppLaunchConfigurationResponse pResponseStatus_ =
  GetAppLaunchConfigurationResponse'
    { serverGroupLaunchConfigurations =
        Lude.Nothing,
      autoLaunch = Lude.Nothing,
      roleName = Lude.Nothing,
      appId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The launch configurations for server groups in this application.
--
-- /Note:/ Consider using 'serverGroupLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrsServerGroupLaunchConfigurations :: Lens.Lens' GetAppLaunchConfigurationResponse (Lude.Maybe [ServerGroupLaunchConfiguration])
galcrsServerGroupLaunchConfigurations = Lens.lens (serverGroupLaunchConfigurations :: GetAppLaunchConfigurationResponse -> Lude.Maybe [ServerGroupLaunchConfiguration]) (\s a -> s {serverGroupLaunchConfigurations = a} :: GetAppLaunchConfigurationResponse)
{-# DEPRECATED galcrsServerGroupLaunchConfigurations "Use generic-lens or generic-optics with 'serverGroupLaunchConfigurations' instead." #-}

-- | Indicates whether the application is configured to launch automatically after replication is complete.
--
-- /Note:/ Consider using 'autoLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrsAutoLaunch :: Lens.Lens' GetAppLaunchConfigurationResponse (Lude.Maybe Lude.Bool)
galcrsAutoLaunch = Lens.lens (autoLaunch :: GetAppLaunchConfigurationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {autoLaunch = a} :: GetAppLaunchConfigurationResponse)
{-# DEPRECATED galcrsAutoLaunch "Use generic-lens or generic-optics with 'autoLaunch' instead." #-}

-- | The name of the service role in the customer's account that AWS CloudFormation uses to launch the application.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrsRoleName :: Lens.Lens' GetAppLaunchConfigurationResponse (Lude.Maybe Lude.Text)
galcrsRoleName = Lens.lens (roleName :: GetAppLaunchConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: GetAppLaunchConfigurationResponse)
{-# DEPRECATED galcrsRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrsAppId :: Lens.Lens' GetAppLaunchConfigurationResponse (Lude.Maybe Lude.Text)
galcrsAppId = Lens.lens (appId :: GetAppLaunchConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: GetAppLaunchConfigurationResponse)
{-# DEPRECATED galcrsAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galcrsResponseStatus :: Lens.Lens' GetAppLaunchConfigurationResponse Lude.Int
galcrsResponseStatus = Lens.lens (responseStatus :: GetAppLaunchConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppLaunchConfigurationResponse)
{-# DEPRECATED galcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
