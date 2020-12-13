{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified launch configuration.
--
-- The launch configuration must not be attached to an Auto Scaling group. When this call completes, the launch configuration is no longer available for use.
module Network.AWS.AutoScaling.DeleteLaunchConfiguration
  ( -- * Creating a request
    DeleteLaunchConfiguration (..),
    mkDeleteLaunchConfiguration,

    -- ** Request lenses
    dlcLaunchConfigurationName,

    -- * Destructuring the response
    DeleteLaunchConfigurationResponse (..),
    mkDeleteLaunchConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLaunchConfiguration' smart constructor.
newtype DeleteLaunchConfiguration = DeleteLaunchConfiguration'
  { -- | The name of the launch configuration.
    launchConfigurationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'launchConfigurationName' - The name of the launch configuration.
mkDeleteLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Lude.Text ->
  DeleteLaunchConfiguration
mkDeleteLaunchConfiguration pLaunchConfigurationName_ =
  DeleteLaunchConfiguration'
    { launchConfigurationName =
        pLaunchConfigurationName_
    }

-- | The name of the launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcLaunchConfigurationName :: Lens.Lens' DeleteLaunchConfiguration Lude.Text
dlcLaunchConfigurationName = Lens.lens (launchConfigurationName :: DeleteLaunchConfiguration -> Lude.Text) (\s a -> s {launchConfigurationName = a} :: DeleteLaunchConfiguration)
{-# DEPRECATED dlcLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

instance Lude.AWSRequest DeleteLaunchConfiguration where
  type
    Rs DeleteLaunchConfiguration =
      DeleteLaunchConfigurationResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DeleteLaunchConfigurationResponse'

instance Lude.ToHeaders DeleteLaunchConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLaunchConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLaunchConfiguration where
  toQuery DeleteLaunchConfiguration' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLaunchConfiguration" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "LaunchConfigurationName" Lude.=: launchConfigurationName
      ]

-- | /See:/ 'mkDeleteLaunchConfigurationResponse' smart constructor.
data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLaunchConfigurationResponse' with the minimum fields required to make a request.
mkDeleteLaunchConfigurationResponse ::
  DeleteLaunchConfigurationResponse
mkDeleteLaunchConfigurationResponse =
  DeleteLaunchConfigurationResponse'
