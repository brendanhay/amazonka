{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteAppLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the launch configuration for the specified application.
module Network.AWS.SMS.DeleteAppLaunchConfiguration
  ( -- * Creating a request
    DeleteAppLaunchConfiguration (..),
    mkDeleteAppLaunchConfiguration,

    -- ** Request lenses
    dalcAppId,

    -- * Destructuring the response
    DeleteAppLaunchConfigurationResponse (..),
    mkDeleteAppLaunchConfigurationResponse,

    -- ** Response lenses
    dalcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkDeleteAppLaunchConfiguration' smart constructor.
newtype DeleteAppLaunchConfiguration = DeleteAppLaunchConfiguration'
  { -- | The ID of the application.
    appId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkDeleteAppLaunchConfiguration ::
  DeleteAppLaunchConfiguration
mkDeleteAppLaunchConfiguration =
  DeleteAppLaunchConfiguration' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalcAppId :: Lens.Lens' DeleteAppLaunchConfiguration (Lude.Maybe Lude.Text)
dalcAppId = Lens.lens (appId :: DeleteAppLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: DeleteAppLaunchConfiguration)
{-# DEPRECATED dalcAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest DeleteAppLaunchConfiguration where
  type
    Rs DeleteAppLaunchConfiguration =
      DeleteAppLaunchConfigurationResponse
  request = Req.postJSON smsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAppLaunchConfigurationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAppLaunchConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppLaunchConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAppLaunchConfiguration where
  toJSON DeleteAppLaunchConfiguration' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath DeleteAppLaunchConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAppLaunchConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAppLaunchConfigurationResponse' smart constructor.
newtype DeleteAppLaunchConfigurationResponse = DeleteAppLaunchConfigurationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAppLaunchConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAppLaunchConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAppLaunchConfigurationResponse
mkDeleteAppLaunchConfigurationResponse pResponseStatus_ =
  DeleteAppLaunchConfigurationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dalcrsResponseStatus :: Lens.Lens' DeleteAppLaunchConfigurationResponse Lude.Int
dalcrsResponseStatus = Lens.lens (responseStatus :: DeleteAppLaunchConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAppLaunchConfigurationResponse)
{-# DEPRECATED dalcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
