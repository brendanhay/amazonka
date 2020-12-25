{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the draft configuration associated with the running environment.
--
-- Updating a running environment with any configuration changes creates a draft configuration set. You can get the draft configuration using 'DescribeConfigurationSettings' while the update is in progress or if the update fails. The @DeploymentStatus@ for the draft configuration indicates whether the deployment is in process or has failed. The draft configuration remains in existence until it is deleted with this action.
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
  ( -- * Creating a request
    DeleteEnvironmentConfiguration (..),
    mkDeleteEnvironmentConfiguration,

    -- ** Request lenses
    decApplicationName,
    decEnvironmentName,

    -- * Destructuring the response
    DeleteEnvironmentConfigurationResponse (..),
    mkDeleteEnvironmentConfigurationResponse,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete a draft environment configuration.
--
-- /See:/ 'mkDeleteEnvironmentConfiguration' smart constructor.
data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration'
  { -- | The name of the application the environment is associated with.
    applicationName :: Types.ApplicationName,
    -- | The name of the environment to delete the draft configuration from.
    environmentName :: Types.EnvironmentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentConfiguration' value with any optional fields omitted.
mkDeleteEnvironmentConfiguration ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'environmentName'
  Types.EnvironmentName ->
  DeleteEnvironmentConfiguration
mkDeleteEnvironmentConfiguration applicationName environmentName =
  DeleteEnvironmentConfiguration' {applicationName, environmentName}

-- | The name of the application the environment is associated with.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decApplicationName :: Lens.Lens' DeleteEnvironmentConfiguration Types.ApplicationName
decApplicationName = Lens.field @"applicationName"
{-# DEPRECATED decApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the environment to delete the draft configuration from.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decEnvironmentName :: Lens.Lens' DeleteEnvironmentConfiguration Types.EnvironmentName
decEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED decEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Core.AWSRequest DeleteEnvironmentConfiguration where
  type
    Rs DeleteEnvironmentConfiguration =
      DeleteEnvironmentConfigurationResponse
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
            ( Core.pure ("Action", "DeleteEnvironmentConfiguration")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" applicationName)
                Core.<> (Core.toQueryValue "EnvironmentName" environmentName)
            )
      }
  response =
    Response.receiveNull DeleteEnvironmentConfigurationResponse'

-- | /See:/ 'mkDeleteEnvironmentConfigurationResponse' smart constructor.
data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEnvironmentConfigurationResponse' value with any optional fields omitted.
mkDeleteEnvironmentConfigurationResponse ::
  DeleteEnvironmentConfigurationResponse
mkDeleteEnvironmentConfigurationResponse =
  DeleteEnvironmentConfigurationResponse'
