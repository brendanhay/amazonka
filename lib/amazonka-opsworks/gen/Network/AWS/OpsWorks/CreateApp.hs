{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an app for a specified stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateApp
  ( -- * Creating a request
    CreateApp (..),
    mkCreateApp,

    -- ** Request lenses
    caStackId,
    caName,
    caType,
    caAppSource,
    caAttributes,
    caDataSources,
    caDescription,
    caDomains,
    caEnableSsl,
    caEnvironment,
    caShortname,
    caSslConfiguration,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carrsAppId,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The stack ID.
    stackId :: Types.String,
    -- | The app name.
    name :: Types.String,
    -- | The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
    type' :: Types.AppType,
    -- | A @Source@ object that specifies the app repository.
    appSource :: Core.Maybe Types.Source,
    -- | One or more user-defined key/value pairs to be added to the stack attributes.
    attributes :: Core.Maybe (Core.HashMap Types.AppAttributesKeys Types.String),
    -- | The app's data source.
    dataSources :: Core.Maybe [Types.DataSource],
    -- | A description of the app.
    description :: Core.Maybe Types.String,
    -- | The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
    domains :: Core.Maybe [Types.String],
    -- | Whether to enable SSL for the app.
    enableSsl :: Core.Maybe Core.Bool,
    -- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
    --
    -- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
    environment :: Core.Maybe [Types.EnvironmentVariable],
    -- | The app's short name.
    shortname :: Core.Maybe Types.String,
    -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Core.Maybe Types.SslConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApp' value with any optional fields omitted.
mkCreateApp ::
  -- | 'stackId'
  Types.String ->
  -- | 'name'
  Types.String ->
  -- | 'type\''
  Types.AppType ->
  CreateApp
mkCreateApp stackId name type' =
  CreateApp'
    { stackId,
      name,
      type',
      appSource = Core.Nothing,
      attributes = Core.Nothing,
      dataSources = Core.Nothing,
      description = Core.Nothing,
      domains = Core.Nothing,
      enableSsl = Core.Nothing,
      environment = Core.Nothing,
      shortname = Core.Nothing,
      sslConfiguration = Core.Nothing
    }

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStackId :: Lens.Lens' CreateApp Types.String
caStackId = Lens.field @"stackId"
{-# DEPRECATED caStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApp Types.String
caName = Lens.field @"name"
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caType :: Lens.Lens' CreateApp Types.AppType
caType = Lens.field @"type'"
{-# DEPRECATED caType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A @Source@ object that specifies the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppSource :: Lens.Lens' CreateApp (Core.Maybe Types.Source)
caAppSource = Lens.field @"appSource"
{-# DEPRECATED caAppSource "Use generic-lens or generic-optics with 'appSource' instead." #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAttributes :: Lens.Lens' CreateApp (Core.Maybe (Core.HashMap Types.AppAttributesKeys Types.String))
caAttributes = Lens.field @"attributes"
{-# DEPRECATED caAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The app's data source.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDataSources :: Lens.Lens' CreateApp (Core.Maybe [Types.DataSource])
caDataSources = Lens.field @"dataSources"
{-# DEPRECATED caDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApp (Core.Maybe Types.String)
caDescription = Lens.field @"description"
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomains :: Lens.Lens' CreateApp (Core.Maybe [Types.String])
caDomains = Lens.field @"domains"
{-# DEPRECATED caDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | Whether to enable SSL for the app.
--
-- /Note:/ Consider using 'enableSsl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEnableSsl :: Lens.Lens' CreateApp (Core.Maybe Core.Bool)
caEnableSsl = Lens.field @"enableSsl"
{-# DEPRECATED caEnableSsl "Use generic-lens or generic-optics with 'enableSsl' instead." #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEnvironment :: Lens.Lens' CreateApp (Core.Maybe [Types.EnvironmentVariable])
caEnvironment = Lens.field @"environment"
{-# DEPRECATED caEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The app's short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caShortname :: Lens.Lens' CreateApp (Core.Maybe Types.String)
caShortname = Lens.field @"shortname"
{-# DEPRECATED caShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSslConfiguration :: Lens.Lens' CreateApp (Core.Maybe Types.SslConfiguration)
caSslConfiguration = Lens.field @"sslConfiguration"
{-# DEPRECATED caSslConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead." #-}

instance Core.FromJSON CreateApp where
  toJSON CreateApp {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StackId" Core..= stackId),
            Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            ("AppSource" Core..=) Core.<$> appSource,
            ("Attributes" Core..=) Core.<$> attributes,
            ("DataSources" Core..=) Core.<$> dataSources,
            ("Description" Core..=) Core.<$> description,
            ("Domains" Core..=) Core.<$> domains,
            ("EnableSsl" Core..=) Core.<$> enableSsl,
            ("Environment" Core..=) Core.<$> environment,
            ("Shortname" Core..=) Core.<$> shortname,
            ("SslConfiguration" Core..=) Core.<$> sslConfiguration
          ]
      )

instance Core.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateApp")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Core.<$> (x Core..:? "AppId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @CreateApp@ request.
--
-- /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The app ID.
    appId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppResponse' value with any optional fields omitted.
mkCreateAppResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAppResponse
mkCreateAppResponse responseStatus =
  CreateAppResponse' {appId = Core.Nothing, responseStatus}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAppId :: Lens.Lens' CreateAppResponse (Core.Maybe Types.String)
carrsAppId = Lens.field @"appId"
{-# DEPRECATED carrsAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAppResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
