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
    uaSSLConfiguration,
    uaEnvironment,
    uaEnableSSL,
    uaDataSources,
    uaAppSource,
    uaAppId,
    uaAttributes,
    uaName,
    uaType,
    uaDomains,
    uaDescription,

    -- * Destructuring the response
    UpdateAppResponse (..),
    mkUpdateAppResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Lude.Maybe SSLConfiguration,
    -- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
    --
    -- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20 KB)."
    environment :: Lude.Maybe [EnvironmentVariable],
    -- | Whether SSL is enabled for the app.
    enableSSL :: Lude.Maybe Lude.Bool,
    -- | The app's data sources.
    dataSources :: Lude.Maybe [DataSource],
    -- | A @Source@ object that specifies the app repository.
    appSource :: Lude.Maybe Source,
    -- | The app ID.
    appId :: Lude.Text,
    -- | One or more user-defined key/value pairs to be added to the stack attributes.
    attributes :: Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text)),
    -- | The app name.
    name :: Lude.Maybe Lude.Text,
    -- | The app type.
    type' :: Lude.Maybe AppType,
    -- | The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
    domains :: Lude.Maybe [Lude.Text],
    -- | A description of the app.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApp' with the minimum fields required to make a request.
--
-- * 'sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
-- * 'environment' - An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20 KB)."
-- * 'enableSSL' - Whether SSL is enabled for the app.
-- * 'dataSources' - The app's data sources.
-- * 'appSource' - A @Source@ object that specifies the app repository.
-- * 'appId' - The app ID.
-- * 'attributes' - One or more user-defined key/value pairs to be added to the stack attributes.
-- * 'name' - The app name.
-- * 'type'' - The app type.
-- * 'domains' - The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
-- * 'description' - A description of the app.
mkUpdateApp ::
  -- | 'appId'
  Lude.Text ->
  UpdateApp
mkUpdateApp pAppId_ =
  UpdateApp'
    { sslConfiguration = Lude.Nothing,
      environment = Lude.Nothing,
      enableSSL = Lude.Nothing,
      dataSources = Lude.Nothing,
      appSource = Lude.Nothing,
      appId = pAppId_,
      attributes = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      domains = Lude.Nothing,
      description = Lude.Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaSSLConfiguration :: Lens.Lens' UpdateApp (Lude.Maybe SSLConfiguration)
uaSSLConfiguration = Lens.lens (sslConfiguration :: UpdateApp -> Lude.Maybe SSLConfiguration) (\s a -> s {sslConfiguration = a} :: UpdateApp)
{-# DEPRECATED uaSSLConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead." #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances.For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- There is no specific limit on the number of environment variables. However, the size of the associated data structure - which includes the variables' names, values, and protected flag values - cannot exceed 20 KB. This limit should accommodate most if not all use cases. Exceeding it will cause an exception with the message, "Environment: is too large (maximum is 20 KB)."
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEnvironment :: Lens.Lens' UpdateApp (Lude.Maybe [EnvironmentVariable])
uaEnvironment = Lens.lens (environment :: UpdateApp -> Lude.Maybe [EnvironmentVariable]) (\s a -> s {environment = a} :: UpdateApp)
{-# DEPRECATED uaEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Whether SSL is enabled for the app.
--
-- /Note:/ Consider using 'enableSSL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaEnableSSL :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Bool)
uaEnableSSL = Lens.lens (enableSSL :: UpdateApp -> Lude.Maybe Lude.Bool) (\s a -> s {enableSSL = a} :: UpdateApp)
{-# DEPRECATED uaEnableSSL "Use generic-lens or generic-optics with 'enableSSL' instead." #-}

-- | The app's data sources.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDataSources :: Lens.Lens' UpdateApp (Lude.Maybe [DataSource])
uaDataSources = Lens.lens (dataSources :: UpdateApp -> Lude.Maybe [DataSource]) (\s a -> s {dataSources = a} :: UpdateApp)
{-# DEPRECATED uaDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | A @Source@ object that specifies the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppSource :: Lens.Lens' UpdateApp (Lude.Maybe Source)
uaAppSource = Lens.lens (appSource :: UpdateApp -> Lude.Maybe Source) (\s a -> s {appSource = a} :: UpdateApp)
{-# DEPRECATED uaAppSource "Use generic-lens or generic-optics with 'appSource' instead." #-}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppId :: Lens.Lens' UpdateApp Lude.Text
uaAppId = Lens.lens (appId :: UpdateApp -> Lude.Text) (\s a -> s {appId = a} :: UpdateApp)
{-# DEPRECATED uaAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | One or more user-defined key/value pairs to be added to the stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAttributes :: Lens.Lens' UpdateApp (Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text)))
uaAttributes = Lens.lens (attributes :: UpdateApp -> Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text))) (\s a -> s {attributes = a} :: UpdateApp)
{-# DEPRECATED uaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Text)
uaName = Lens.lens (name :: UpdateApp -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateApp)
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The app type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaType :: Lens.Lens' UpdateApp (Lude.Maybe AppType)
uaType = Lens.lens (type' :: UpdateApp -> Lude.Maybe AppType) (\s a -> s {type' = a} :: UpdateApp)
{-# DEPRECATED uaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The app's virtual host settings, with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDomains :: Lens.Lens' UpdateApp (Lude.Maybe [Lude.Text])
uaDomains = Lens.lens (domains :: UpdateApp -> Lude.Maybe [Lude.Text]) (\s a -> s {domains = a} :: UpdateApp)
{-# DEPRECATED uaDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateApp -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApp)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateApp where
  type Rs UpdateApp = UpdateAppResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateAppResponse'

instance Lude.ToHeaders UpdateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateApp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SslConfiguration" Lude..=) Lude.<$> sslConfiguration,
            ("Environment" Lude..=) Lude.<$> environment,
            ("EnableSsl" Lude..=) Lude.<$> enableSSL,
            ("DataSources" Lude..=) Lude.<$> dataSources,
            ("AppSource" Lude..=) Lude.<$> appSource,
            Lude.Just ("AppId" Lude..= appId),
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("Name" Lude..=) Lude.<$> name,
            ("Type" Lude..=) Lude.<$> type',
            ("Domains" Lude..=) Lude.<$> domains,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAppResponse' with the minimum fields required to make a request.
mkUpdateAppResponse ::
  UpdateAppResponse
mkUpdateAppResponse = UpdateAppResponse'
