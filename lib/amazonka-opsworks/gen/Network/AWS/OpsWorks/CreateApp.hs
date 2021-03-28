{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateApp (..)
    , mkCreateApp
    -- ** Request lenses
    , caStackId
    , caName
    , caType
    , caAppSource
    , caAttributes
    , caDataSources
    , caDescription
    , caDomains
    , caEnableSsl
    , caEnvironment
    , caShortname
    , caSslConfiguration

    -- * Destructuring the response
    , CreateAppResponse (..)
    , mkCreateAppResponse
    -- ** Response lenses
    , carrsAppId
    , carrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { stackId :: Core.Text
    -- ^ The stack ID.
  , name :: Core.Text
    -- ^ The app name.
  , type' :: Types.AppType
    -- ^ The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
  , appSource :: Core.Maybe Types.Source
    -- ^ A @Source@ object that specifies the app repository.
  , attributes :: Core.Maybe (Core.HashMap Types.AppAttributesKeys Core.Text)
    -- ^ One or more user-defined key/value pairs to be added to the stack attributes.
  , dataSources :: Core.Maybe [Types.DataSource]
    -- ^ The app's data source.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the app.
  , domains :: Core.Maybe [Core.Text]
    -- ^ The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@ 
  , enableSsl :: Core.Maybe Core.Bool
    -- ^ Whether to enable SSL for the app.
  , environment :: Core.Maybe [Types.EnvironmentVariable]
    -- ^ An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
  , shortname :: Core.Maybe Core.Text
    -- ^ The app's short name.
  , sslConfiguration :: Core.Maybe Types.SslConfiguration
    -- ^ An @SslConfiguration@ object with the SSL configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApp' value with any optional fields omitted.
mkCreateApp
    :: Core.Text -- ^ 'stackId'
    -> Core.Text -- ^ 'name'
    -> Types.AppType -- ^ 'type\''
    -> CreateApp
mkCreateApp stackId name type'
  = CreateApp'{stackId, name, type', appSource = Core.Nothing,
               attributes = Core.Nothing, dataSources = Core.Nothing,
               description = Core.Nothing, domains = Core.Nothing,
               enableSsl = Core.Nothing, environment = Core.Nothing,
               shortname = Core.Nothing, sslConfiguration = Core.Nothing}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStackId :: Lens.Lens' CreateApp Core.Text
caStackId = Lens.field @"stackId"
{-# INLINEABLE caStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApp Core.Text
caName = Lens.field @"name"
{-# INLINEABLE caName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caType :: Lens.Lens' CreateApp Types.AppType
caType = Lens.field @"type'"
{-# INLINEABLE caType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A @Source@ object that specifies the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppSource :: Lens.Lens' CreateApp (Core.Maybe Types.Source)
caAppSource = Lens.field @"appSource"
{-# INLINEABLE caAppSource #-}
{-# DEPRECATED appSource "Use generic-lens or generic-optics with 'appSource' instead"  #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAttributes :: Lens.Lens' CreateApp (Core.Maybe (Core.HashMap Types.AppAttributesKeys Core.Text))
caAttributes = Lens.field @"attributes"
{-# INLINEABLE caAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The app's data source.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDataSources :: Lens.Lens' CreateApp (Core.Maybe [Types.DataSource])
caDataSources = Lens.field @"dataSources"
{-# INLINEABLE caDataSources #-}
{-# DEPRECATED dataSources "Use generic-lens or generic-optics with 'dataSources' instead"  #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApp (Core.Maybe Core.Text)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@ 
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomains :: Lens.Lens' CreateApp (Core.Maybe [Core.Text])
caDomains = Lens.field @"domains"
{-# INLINEABLE caDomains #-}
{-# DEPRECATED domains "Use generic-lens or generic-optics with 'domains' instead"  #-}

-- | Whether to enable SSL for the app.
--
-- /Note:/ Consider using 'enableSsl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEnableSsl :: Lens.Lens' CreateApp (Core.Maybe Core.Bool)
caEnableSsl = Lens.field @"enableSsl"
{-# INLINEABLE caEnableSsl #-}
{-# DEPRECATED enableSsl "Use generic-lens or generic-optics with 'enableSsl' instead"  #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEnvironment :: Lens.Lens' CreateApp (Core.Maybe [Types.EnvironmentVariable])
caEnvironment = Lens.field @"environment"
{-# INLINEABLE caEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | The app's short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caShortname :: Lens.Lens' CreateApp (Core.Maybe Core.Text)
caShortname = Lens.field @"shortname"
{-# INLINEABLE caShortname #-}
{-# DEPRECATED shortname "Use generic-lens or generic-optics with 'shortname' instead"  #-}

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSslConfiguration :: Lens.Lens' CreateApp (Core.Maybe Types.SslConfiguration)
caSslConfiguration = Lens.field @"sslConfiguration"
{-# INLINEABLE caSslConfiguration #-}
{-# DEPRECATED sslConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead"  #-}

instance Core.ToQuery CreateApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApp where
        toHeaders CreateApp{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateApp") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApp where
        toJSON CreateApp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackId" Core..= stackId),
                  Core.Just ("Name" Core..= name), Core.Just ("Type" Core..= type'),
                  ("AppSource" Core..=) Core.<$> appSource,
                  ("Attributes" Core..=) Core.<$> attributes,
                  ("DataSources" Core..=) Core.<$> dataSources,
                  ("Description" Core..=) Core.<$> description,
                  ("Domains" Core..=) Core.<$> domains,
                  ("EnableSsl" Core..=) Core.<$> enableSsl,
                  ("Environment" Core..=) Core.<$> environment,
                  ("Shortname" Core..=) Core.<$> shortname,
                  ("SslConfiguration" Core..=) Core.<$> sslConfiguration])

instance Core.AWSRequest CreateApp where
        type Rs CreateApp = CreateAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAppResponse' Core.<$>
                   (x Core..:? "AppId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @CreateApp@ request.
--
-- /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { appId :: Core.Maybe Core.Text
    -- ^ The app ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppResponse' value with any optional fields omitted.
mkCreateAppResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAppResponse
mkCreateAppResponse responseStatus
  = CreateAppResponse'{appId = Core.Nothing, responseStatus}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAppId :: Lens.Lens' CreateAppResponse (Core.Maybe Core.Text)
carrsAppId = Lens.field @"appId"
{-# INLINEABLE carrsAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAppResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
