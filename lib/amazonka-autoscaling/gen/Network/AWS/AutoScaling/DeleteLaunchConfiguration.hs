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

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLaunchConfiguration' smart constructor.
newtype DeleteLaunchConfiguration = DeleteLaunchConfiguration'
  { -- | The name of the launch configuration.
    launchConfigurationName :: Types.LaunchConfigurationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchConfiguration' value with any optional fields omitted.
mkDeleteLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Types.LaunchConfigurationName ->
  DeleteLaunchConfiguration
mkDeleteLaunchConfiguration launchConfigurationName =
  DeleteLaunchConfiguration' {launchConfigurationName}

-- | The name of the launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcLaunchConfigurationName :: Lens.Lens' DeleteLaunchConfiguration Types.LaunchConfigurationName
dlcLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# DEPRECATED dlcLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

instance Core.AWSRequest DeleteLaunchConfiguration where
  type
    Rs DeleteLaunchConfiguration =
      DeleteLaunchConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteLaunchConfiguration")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> ( Core.toQueryValue
                            "LaunchConfigurationName"
                            launchConfigurationName
                        )
            )
      }
  response = Response.receiveNull DeleteLaunchConfigurationResponse'

-- | /See:/ 'mkDeleteLaunchConfigurationResponse' smart constructor.
data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLaunchConfigurationResponse' value with any optional fields omitted.
mkDeleteLaunchConfigurationResponse ::
  DeleteLaunchConfigurationResponse
mkDeleteLaunchConfigurationResponse =
  DeleteLaunchConfigurationResponse'
