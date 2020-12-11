{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.PutAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the launch configuration for the specified application.
module Network.AWS.SMS.PutAppLaunchConfiguration
  ( -- * Creating a request
    PutAppLaunchConfiguration (..),
    mkPutAppLaunchConfiguration,

    -- ** Request lenses
    palcServerGroupLaunchConfigurations,
    palcAutoLaunch,
    palcRoleName,
    palcAppId,

    -- * Destructuring the response
    PutAppLaunchConfigurationResponse (..),
    mkPutAppLaunchConfigurationResponse,

    -- ** Response lenses
    palcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkPutAppLaunchConfiguration' smart constructor.
data PutAppLaunchConfiguration = PutAppLaunchConfiguration'
  { serverGroupLaunchConfigurations ::
      Lude.Maybe
        [ServerGroupLaunchConfiguration],
    autoLaunch :: Lude.Maybe Lude.Bool,
    roleName :: Lude.Maybe Lude.Text,
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
-- * 'autoLaunch' - Indicates whether the application is configured to launch automatically after replication is complete.
-- * 'roleName' - The name of service role in the customer's account that AWS CloudFormation uses to launch the application.
-- * 'serverGroupLaunchConfigurations' - Information about the launch configurations for server groups in the application.
mkPutAppLaunchConfiguration ::
  PutAppLaunchConfiguration
mkPutAppLaunchConfiguration =
  PutAppLaunchConfiguration'
    { serverGroupLaunchConfigurations =
        Lude.Nothing,
      autoLaunch = Lude.Nothing,
      roleName = Lude.Nothing,
      appId = Lude.Nothing
    }

-- | Information about the launch configurations for server groups in the application.
--
-- /Note:/ Consider using 'serverGroupLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcServerGroupLaunchConfigurations :: Lens.Lens' PutAppLaunchConfiguration (Lude.Maybe [ServerGroupLaunchConfiguration])
palcServerGroupLaunchConfigurations = Lens.lens (serverGroupLaunchConfigurations :: PutAppLaunchConfiguration -> Lude.Maybe [ServerGroupLaunchConfiguration]) (\s a -> s {serverGroupLaunchConfigurations = a} :: PutAppLaunchConfiguration)
{-# DEPRECATED palcServerGroupLaunchConfigurations "Use generic-lens or generic-optics with 'serverGroupLaunchConfigurations' instead." #-}

-- | Indicates whether the application is configured to launch automatically after replication is complete.
--
-- /Note:/ Consider using 'autoLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcAutoLaunch :: Lens.Lens' PutAppLaunchConfiguration (Lude.Maybe Lude.Bool)
palcAutoLaunch = Lens.lens (autoLaunch :: PutAppLaunchConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {autoLaunch = a} :: PutAppLaunchConfiguration)
{-# DEPRECATED palcAutoLaunch "Use generic-lens or generic-optics with 'autoLaunch' instead." #-}

-- | The name of service role in the customer's account that AWS CloudFormation uses to launch the application.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcRoleName :: Lens.Lens' PutAppLaunchConfiguration (Lude.Maybe Lude.Text)
palcRoleName = Lens.lens (roleName :: PutAppLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: PutAppLaunchConfiguration)
{-# DEPRECATED palcRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcAppId :: Lens.Lens' PutAppLaunchConfiguration (Lude.Maybe Lude.Text)
palcAppId = Lens.lens (appId :: PutAppLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: PutAppLaunchConfiguration)
{-# DEPRECATED palcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest PutAppLaunchConfiguration where
  type
    Rs PutAppLaunchConfiguration =
      PutAppLaunchConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutAppLaunchConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAppLaunchConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.PutAppLaunchConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAppLaunchConfiguration where
  toJSON PutAppLaunchConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverGroupLaunchConfigurations" Lude..=)
              Lude.<$> serverGroupLaunchConfigurations,
            ("autoLaunch" Lude..=) Lude.<$> autoLaunch,
            ("roleName" Lude..=) Lude.<$> roleName,
            ("appId" Lude..=) Lude.<$> appId
          ]
      )

instance Lude.ToPath PutAppLaunchConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAppLaunchConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAppLaunchConfigurationResponse' smart constructor.
newtype PutAppLaunchConfigurationResponse = PutAppLaunchConfigurationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutAppLaunchConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAppLaunchConfigurationResponse
mkPutAppLaunchConfigurationResponse pResponseStatus_ =
  PutAppLaunchConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palcrsResponseStatus :: Lens.Lens' PutAppLaunchConfigurationResponse Lude.Int
palcrsResponseStatus = Lens.lens (responseStatus :: PutAppLaunchConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAppLaunchConfigurationResponse)
{-# DEPRECATED palcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
