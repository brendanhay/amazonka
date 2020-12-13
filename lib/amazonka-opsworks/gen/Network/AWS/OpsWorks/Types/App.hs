{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.App
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.App
  ( App (..),

    -- * Smart constructor
    mkApp,

    -- * Lenses
    aSSLConfiguration,
    aEnvironment,
    aEnableSSL,
    aCreatedAt,
    aShortname,
    aDataSources,
    aAppSource,
    aAppId,
    aAttributes,
    aName,
    aType,
    aStackId,
    aDomains,
    aDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AppAttributesKeys
import Network.AWS.OpsWorks.Types.AppType
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.SSLConfiguration
import Network.AWS.OpsWorks.Types.Source
import qualified Network.AWS.Prelude as Lude

-- | A description of the app.
--
-- /See:/ 'mkApp' smart constructor.
data App = App'
  { -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Lude.Maybe SSLConfiguration,
    -- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
    environment :: Lude.Maybe [EnvironmentVariable],
    -- | Whether to enable SSL for the app.
    enableSSL :: Lude.Maybe Lude.Bool,
    -- | When the app was created.
    createdAt :: Lude.Maybe Lude.Text,
    -- | The app's short name.
    shortname :: Lude.Maybe Lude.Text,
    -- | The app's data sources.
    dataSources :: Lude.Maybe [DataSource],
    -- | A @Source@ object that describes the app repository.
    appSource :: Lude.Maybe Source,
    -- | The app ID.
    appId :: Lude.Maybe Lude.Text,
    -- | The stack attributes.
    attributes :: Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text)),
    -- | The app name.
    name :: Lude.Maybe Lude.Text,
    -- | The app type.
    type' :: Lude.Maybe AppType,
    -- | The app stack ID.
    stackId :: Lude.Maybe Lude.Text,
    -- | The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
    domains :: Lude.Maybe [Lude.Text],
    -- | A description of the app.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'App' with the minimum fields required to make a request.
--
-- * 'sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
-- * 'environment' - An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
-- * 'enableSSL' - Whether to enable SSL for the app.
-- * 'createdAt' - When the app was created.
-- * 'shortname' - The app's short name.
-- * 'dataSources' - The app's data sources.
-- * 'appSource' - A @Source@ object that describes the app repository.
-- * 'appId' - The app ID.
-- * 'attributes' - The stack attributes.
-- * 'name' - The app name.
-- * 'type'' - The app type.
-- * 'stackId' - The app stack ID.
-- * 'domains' - The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
-- * 'description' - A description of the app.
mkApp ::
  App
mkApp =
  App'
    { sslConfiguration = Lude.Nothing,
      environment = Lude.Nothing,
      enableSSL = Lude.Nothing,
      createdAt = Lude.Nothing,
      shortname = Lude.Nothing,
      dataSources = Lude.Nothing,
      appSource = Lude.Nothing,
      appId = Lude.Nothing,
      attributes = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      stackId = Lude.Nothing,
      domains = Lude.Nothing,
      description = Lude.Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
--
-- /Note:/ Consider using 'sslConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSSLConfiguration :: Lens.Lens' App (Lude.Maybe SSLConfiguration)
aSSLConfiguration = Lens.lens (sslConfiguration :: App -> Lude.Maybe SSLConfiguration) (\s a -> s {sslConfiguration = a} :: App)
{-# DEPRECATED aSSLConfiguration "Use generic-lens or generic-optics with 'sslConfiguration' instead." #-}

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnvironment :: Lens.Lens' App (Lude.Maybe [EnvironmentVariable])
aEnvironment = Lens.lens (environment :: App -> Lude.Maybe [EnvironmentVariable]) (\s a -> s {environment = a} :: App)
{-# DEPRECATED aEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Whether to enable SSL for the app.
--
-- /Note:/ Consider using 'enableSSL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnableSSL :: Lens.Lens' App (Lude.Maybe Lude.Bool)
aEnableSSL = Lens.lens (enableSSL :: App -> Lude.Maybe Lude.Bool) (\s a -> s {enableSSL = a} :: App)
{-# DEPRECATED aEnableSSL "Use generic-lens or generic-optics with 'enableSSL' instead." #-}

-- | When the app was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' App (Lude.Maybe Lude.Text)
aCreatedAt = Lens.lens (createdAt :: App -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: App)
{-# DEPRECATED aCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The app's short name.
--
-- /Note:/ Consider using 'shortname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aShortname :: Lens.Lens' App (Lude.Maybe Lude.Text)
aShortname = Lens.lens (shortname :: App -> Lude.Maybe Lude.Text) (\s a -> s {shortname = a} :: App)
{-# DEPRECATED aShortname "Use generic-lens or generic-optics with 'shortname' instead." #-}

-- | The app's data sources.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDataSources :: Lens.Lens' App (Lude.Maybe [DataSource])
aDataSources = Lens.lens (dataSources :: App -> Lude.Maybe [DataSource]) (\s a -> s {dataSources = a} :: App)
{-# DEPRECATED aDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | A @Source@ object that describes the app repository.
--
-- /Note:/ Consider using 'appSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAppSource :: Lens.Lens' App (Lude.Maybe Source)
aAppSource = Lens.lens (appSource :: App -> Lude.Maybe Source) (\s a -> s {appSource = a} :: App)
{-# DEPRECATED aAppSource "Use generic-lens or generic-optics with 'appSource' instead." #-}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAppId :: Lens.Lens' App (Lude.Maybe Lude.Text)
aAppId = Lens.lens (appId :: App -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: App)
{-# DEPRECATED aAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The stack attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAttributes :: Lens.Lens' App (Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text)))
aAttributes = Lens.lens (attributes :: App -> Lude.Maybe (Lude.HashMap AppAttributesKeys (Lude.Text))) (\s a -> s {attributes = a} :: App)
{-# DEPRECATED aAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The app name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' App (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: App -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: App)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The app type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' App (Lude.Maybe AppType)
aType = Lens.lens (type' :: App -> Lude.Maybe AppType) (\s a -> s {type' = a} :: App)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The app stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStackId :: Lens.Lens' App (Lude.Maybe Lude.Text)
aStackId = Lens.lens (stackId :: App -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: App)
{-# DEPRECATED aStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDomains :: Lens.Lens' App (Lude.Maybe [Lude.Text])
aDomains = Lens.lens (domains :: App -> Lude.Maybe [Lude.Text]) (\s a -> s {domains = a} :: App)
{-# DEPRECATED aDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | A description of the app.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' App (Lude.Maybe Lude.Text)
aDescription = Lens.lens (description :: App -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: App)
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON App where
  parseJSON =
    Lude.withObject
      "App"
      ( \x ->
          App'
            Lude.<$> (x Lude..:? "SslConfiguration")
            Lude.<*> (x Lude..:? "Environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EnableSsl")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "Shortname")
            Lude.<*> (x Lude..:? "DataSources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AppSource")
            Lude.<*> (x Lude..:? "AppId")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "Domains" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
      )
