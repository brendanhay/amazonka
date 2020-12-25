{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified app.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateApp
  ( -- * Creating a request
    UpdateApp (..),
    mkUpdateApp,

    -- ** Request lenses
    uaAppId,
    uaAppSource,
    uaAttributes,
    uaDataSources,
    uaDescription,
    uaDomains,
    uaEnableSsl,
    uaEnvironment,
    uaName,
    uaSslConfiguration,
    uaType,

    -- * Destructuring the response
    UpdateAppResponse (..),
    mkUpdateAppResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | The app ID.
    appId :: Types.String,
    -- | A @Source@ object that specifies the app repository.
    appSource :: Core.Maybe Types.Source,
    -- | One or more user-defined key/value pairs to be added to the stack attributes.
    attributes :: Core.Maybe (Core.HashMap Types.AppAttributesKeys Types.String),
    -- | The app's data sources.
    dataSources :: Core.Maybe [Types.DataSource],
    -- | A description of the app.
    description :: Core.Maybe Types.String,
    -- | The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
    domains :: Core.Maybe [Types.String],
    -- | Whether SSL is enabled for the app.
    enableSsl :: Core.Maybe Core.Bool,
    -- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
    --
    -- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20 KB)."
    environment :: Core.Maybe [Types.EnvironmentVariable],
    -- | The app name.
    name :: Core.Maybe Types.String,
    -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Core.Maybe Types.SslConfiguration,
    -- | The app type.
    type' :: Core.Maybe Types.AppType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApp' value with any optional fields omitted.
mkUpdateApp ::
  -- | 'appId'
  Types.String ->
  UpdateApp
mkUpdateApp appId =
  UpdateApp'
    { appId,
      appSource = Core.Nothing,
      attributes = Core.Nothing,
      dataSources = Core.Nothing,
      description = Core.Nothing,
      domains = Core.Nothing,
      enableSsl = Core.Nothing,
      environment = Core.Nothing,
      name = Core.Nothing,
      sslConfiguration = Core.Nothing,
      type' = Core.Nothing
    }

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppId :: Lens.Lens' UpdateApp Types.String
uaAppId = Lens.field @"appId"
{-# DEPRECATED uaAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | A @Source@ object that specifies the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppSource :: Lens.Lens' UpdateApp (Core.Maybe Types.Source)
uaAppSource = Lens.field @"appSource"
{-# DEPRECATED uaAppSource "Use generic-lens or generic-optics with 'appSource' instead." #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAttributes :: Lens.Lens' UpdateApp (Core.Maybe (Core.HashMap Types.AppAttributesKeys Types.String))
uaAttributes = Lens.field @"attributes"
{-# DEPRECATED uaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The app's data sources.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDataSources :: Lens.Lens' UpdateApp (Core.Maybe [Types.DataSource])
uaDataSources = Lens.field @"dataSources"
{-# DEPRECATED uaDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApp (Core.Maybe Types.String)
uaDescription = Lens.field @"description"
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDomains :: Lens.Lens' UpdateApp (Core.Maybe [Types.String])
uaDomains = Lens.field @"domains"
{-# DEPRECATED uaDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | Whether SSL is enabled for the app.
--
-- /Note:/ Consider using 'enableSsl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEnableSsl :: Lens.Lens' UpdateApp (Core.Maybe Core.Bool)
uaEnableSsl = Lens.field @"enableSsl"
{-# DEPRECATED uaEnableSsl "Use generic-lens or generic-optics with 'enableSsl' instead." #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20 KB)."
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEnvironment :: Lens.Lens' UpdateApp (Core.Maybe [Types.EnvironmentVariable])
uaEnvironment = Lens.field @"environment"
{-# DEPRECATED uaEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApp (Core.Maybe Types.String)
uaName = Lens.field @"name"
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaSslConfiguration :: Lens.Lens' UpdateApp (Core.Maybe Types.SslConfiguration)
uaSslConfiguration = Lens.field @"sslConfiguration"
{-# DEPRECATED uaSslConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead." #-}

-- | The app type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaType :: Lens.Lens' UpdateApp (Core.Maybe Types.AppType)
uaType = Lens.field @"type'"
{-# DEPRECATED uaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON UpdateApp where
  toJSON UpdateApp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AppId" Core..= appId),
            ("AppSource" Core..=) Core.<$> appSource,
            ("Attributes" Core..=) Core.<$> attributes,
            ("DataSources" Core..=) Core.<$> dataSources,
            ("Description" Core..=) Core.<$> description,
            ("Domains" Core..=) Core.<$> domains,
            ("EnableSsl" Core..=) Core.<$> enableSsl,
            ("Environment" Core..=) Core.<$> environment,
            ("Name" Core..=) Core.<$> name,
            ("SslConfiguration" Core..=) Core.<$> sslConfiguration,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.AWSRequest UpdateApp where
  type Rs UpdateApp = UpdateAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateAppResponse'

-- | /See:/ 'mkUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAppResponse' value with any optional fields omitted.
mkUpdateAppResponse ::
  UpdateAppResponse
mkUpdateAppResponse = UpdateAppResponse'
