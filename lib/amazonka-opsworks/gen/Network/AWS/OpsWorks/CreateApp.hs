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
    caSSLConfiguration,
    caEnvironment,
    caEnableSSL,
    caShortname,
    caDataSources,
    caAppSource,
    caAttributes,
    caName,
    caType,
    caStackId,
    caDomains,
    caDescription,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carsAppId,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Lude.Maybe SSLConfiguration,
    -- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
    --
    -- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
    environment :: Lude.Maybe [EnvironmentVariable],
    -- | Whether to enable SSL for the app.
    enableSSL :: Lude.Maybe Lude.Bool,
    -- | The app's short name.
    shortname :: Lude.Maybe Lude.Text,
    -- | The app's data source.
    dataSources :: Lude.Maybe [DataSource],
    -- | A @Source@ object that specifies the app repository.
    appSource :: Lude.Maybe Source,
    -- | One or more user-defined key/value pairs to be added to the stack attributes.
    attributes :: Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text)),
    -- | The app name.
    name :: Lude.Text,
    -- | The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
    type' :: AppType,
    -- | The stack ID.
    stackId :: Lude.Text,
    -- | The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
    domains :: Lude.Maybe [Lude.Text],
    -- | A description of the app.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- * 'sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
-- * 'environment' - An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
-- * 'enableSSL' - Whether to enable SSL for the app.
-- * 'shortname' - The app's short name.
-- * 'dataSources' - The app's data source.
-- * 'appSource' - A @Source@ object that specifies the app repository.
-- * 'attributes' - One or more user-defined key/value pairs to be added to the stack attributes.
-- * 'name' - The app name.
-- * 'type'' - The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
-- * 'stackId' - The stack ID.
-- * 'domains' - The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
-- * 'description' - A description of the app.
mkCreateApp ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  AppType ->
  -- | 'stackId'
  Lude.Text ->
  CreateApp
mkCreateApp pName_ pType_ pStackId_ =
  CreateApp'
    { sslConfiguration = Lude.Nothing,
      environment = Lude.Nothing,
      enableSSL = Lude.Nothing,
      shortname = Lude.Nothing,
      dataSources = Lude.Nothing,
      appSource = Lude.Nothing,
      attributes = Lude.Nothing,
      name = pName_,
      type' = pType_,
      stackId = pStackId_,
      domains = Lude.Nothing,
      description = Lude.Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSSLConfiguration :: Lens.Lens' CreateApp (Lude.Maybe SSLConfiguration)
caSSLConfiguration = Lens.lens (sslConfiguration :: CreateApp -> Lude.Maybe SSLConfiguration) (\s a -> s {sslConfiguration = a} :: CreateApp)
{-# DEPRECATED caSSLConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead." #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20KB)."
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEnvironment :: Lens.Lens' CreateApp (Lude.Maybe [EnvironmentVariable])
caEnvironment = Lens.lens (environment :: CreateApp -> Lude.Maybe [EnvironmentVariable]) (\s a -> s {environment = a} :: CreateApp)
{-# DEPRECATED caEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Whether to enable SSL for the app.
--
-- /Note:/ Consider using 'enableSSL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEnableSSL :: Lens.Lens' CreateApp (Lude.Maybe Lude.Bool)
caEnableSSL = Lens.lens (enableSSL :: CreateApp -> Lude.Maybe Lude.Bool) (\s a -> s {enableSSL = a} :: CreateApp)
{-# DEPRECATED caEnableSSL "Use generic-lens or generic-optics with 'enableSSL' instead." #-}

-- | The app's short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caShortname :: Lens.Lens' CreateApp (Lude.Maybe Lude.Text)
caShortname = Lens.lens (shortname :: CreateApp -> Lude.Maybe Lude.Text) (\s a -> s {shortname = a} :: CreateApp)
{-# DEPRECATED caShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | The app's data source.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDataSources :: Lens.Lens' CreateApp (Lude.Maybe [DataSource])
caDataSources = Lens.lens (dataSources :: CreateApp -> Lude.Maybe [DataSource]) (\s a -> s {dataSources = a} :: CreateApp)
{-# DEPRECATED caDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | A @Source@ object that specifies the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAppSource :: Lens.Lens' CreateApp (Lude.Maybe Source)
caAppSource = Lens.lens (appSource :: CreateApp -> Lude.Maybe Source) (\s a -> s {appSource = a} :: CreateApp)
{-# DEPRECATED caAppSource "Use generic-lens or generic-optics with 'appSource' instead." #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAttributes :: Lens.Lens' CreateApp (Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text)))
caAttributes = Lens.lens (attributes :: CreateApp -> Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text))) (\s a -> s {attributes = a} :: CreateApp)
{-# DEPRECATED caAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApp Lude.Text
caName = Lens.lens (name :: CreateApp -> Lude.Text) (\s a -> s {name = a} :: CreateApp)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The app type. Each supported type is associated with a particular layer. For example, PHP applications are associated with a PHP layer. AWS OpsWorks Stacks deploys an application to those instances that are members of the corresponding layer. If your app isn't one of the standard types, or you prefer to implement your own Deploy recipes, specify @other@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caType :: Lens.Lens' CreateApp AppType
caType = Lens.lens (type' :: CreateApp -> AppType) (\s a -> s {type' = a} :: CreateApp)
{-# DEPRECATED caType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStackId :: Lens.Lens' CreateApp Lude.Text
caStackId = Lens.lens (stackId :: CreateApp -> Lude.Text) (\s a -> s {stackId = a} :: CreateApp)
{-# DEPRECATED caStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The app virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDomains :: Lens.Lens' CreateApp (Lude.Maybe [Lude.Text])
caDomains = Lens.lens (domains :: CreateApp -> Lude.Maybe [Lude.Text]) (\s a -> s {domains = a} :: CreateApp)
{-# DEPRECATED caDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApp (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateApp -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateApp)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Lude.<$> (x Lude..?> "AppId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CreateApp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SslConfiguration" Lude..=) Lude.<$> sslConfiguration,
            ("Environment" Lude..=) Lude.<$> environment,
            ("EnableSsl" Lude..=) Lude.<$> enableSSL,
            ("Shortname" Lude..=) Lude.<$> shortname,
            ("DataSources" Lude..=) Lude.<$> dataSources,
            ("AppSource" Lude..=) Lude.<$> appSource,
            ("Attributes" Lude..=) Lude.<$> attributes,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("StackId" Lude..= stackId),
            ("Domains" Lude..=) Lude.<$> domains,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApp where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CreateApp@ request.
--
-- /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The app ID.
    appId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- * 'appId' - The app ID.
-- * 'responseStatus' - The response status code.
mkCreateAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAppResponse
mkCreateAppResponse pResponseStatus_ =
  CreateAppResponse'
    { appId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAppId :: Lens.Lens' CreateAppResponse (Lude.Maybe Lude.Text)
carsAppId = Lens.lens (appId :: CreateAppResponse -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: CreateAppResponse)
{-# DEPRECATED carsAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAppResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
