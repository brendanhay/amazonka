{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.App
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.App where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.AppAttributesKeys
import Network.AWS.OpsWorks.Types.AppType
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.SslConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | A description of the app.
--
-- /See:/ 'newApp' smart constructor.
data App = App'
  { -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Prelude.Maybe SslConfiguration,
    -- | A @Source@ object that describes the app repository.
    appSource :: Prelude.Maybe Source,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The app\'s data sources.
    dataSources :: Prelude.Maybe [DataSource],
    -- | The app stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The app vhost settings with multiple domains separated by commas. For
    -- example: @\'www.example.com, example.com\'@
    domains :: Prelude.Maybe [Prelude.Text],
    -- | Whether to enable SSL for the app.
    enableSsl :: Prelude.Maybe Prelude.Bool,
    -- | The app\'s short name.
    shortname :: Prelude.Maybe Prelude.Text,
    -- | When the app was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | An array of @EnvironmentVariable@ objects that specify environment
    -- variables to be associated with the app. After you deploy the app, these
    -- variables are defined on the associated app server instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
    --
    -- There is no specific limit on the number of environment variables.
    -- However, the size of the associated data structure - which includes the
    -- variable names, values, and protected flag values - cannot exceed 20 KB.
    -- This limit should accommodate most if not all use cases, but if you do
    -- exceed it, you will cause an exception (API) with an \"Environment: is
    -- too large (maximum is 20 KB)\" message.
    environment :: Prelude.Maybe [EnvironmentVariable],
    -- | The stack attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text),
    -- | The app name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The app type.
    type' :: Prelude.Maybe AppType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'App' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslConfiguration', 'app_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- 'appSource', 'app_appSource' - A @Source@ object that describes the app repository.
--
-- 'appId', 'app_appId' - The app ID.
--
-- 'dataSources', 'app_dataSources' - The app\'s data sources.
--
-- 'stackId', 'app_stackId' - The app stack ID.
--
-- 'domains', 'app_domains' - The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
--
-- 'enableSsl', 'app_enableSsl' - Whether to enable SSL for the app.
--
-- 'shortname', 'app_shortname' - The app\'s short name.
--
-- 'createdAt', 'app_createdAt' - When the app was created.
--
-- 'environment', 'app_environment' - An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variable names, values, and protected flag values - cannot exceed 20 KB.
-- This limit should accommodate most if not all use cases, but if you do
-- exceed it, you will cause an exception (API) with an \"Environment: is
-- too large (maximum is 20 KB)\" message.
--
-- 'attributes', 'app_attributes' - The stack attributes.
--
-- 'name', 'app_name' - The app name.
--
-- 'description', 'app_description' - A description of the app.
--
-- 'type'', 'app_type' - The app type.
newApp ::
  App
newApp =
  App'
    { sslConfiguration = Prelude.Nothing,
      appSource = Prelude.Nothing,
      appId = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      stackId = Prelude.Nothing,
      domains = Prelude.Nothing,
      enableSsl = Prelude.Nothing,
      shortname = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      environment = Prelude.Nothing,
      attributes = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
app_sslConfiguration :: Lens.Lens' App (Prelude.Maybe SslConfiguration)
app_sslConfiguration = Lens.lens (\App' {sslConfiguration} -> sslConfiguration) (\s@App' {} a -> s {sslConfiguration = a} :: App)

-- | A @Source@ object that describes the app repository.
app_appSource :: Lens.Lens' App (Prelude.Maybe Source)
app_appSource = Lens.lens (\App' {appSource} -> appSource) (\s@App' {} a -> s {appSource = a} :: App)

-- | The app ID.
app_appId :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_appId = Lens.lens (\App' {appId} -> appId) (\s@App' {} a -> s {appId = a} :: App)

-- | The app\'s data sources.
app_dataSources :: Lens.Lens' App (Prelude.Maybe [DataSource])
app_dataSources = Lens.lens (\App' {dataSources} -> dataSources) (\s@App' {} a -> s {dataSources = a} :: App) Prelude.. Lens.mapping Lens._Coerce

-- | The app stack ID.
app_stackId :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_stackId = Lens.lens (\App' {stackId} -> stackId) (\s@App' {} a -> s {stackId = a} :: App)

-- | The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
app_domains :: Lens.Lens' App (Prelude.Maybe [Prelude.Text])
app_domains = Lens.lens (\App' {domains} -> domains) (\s@App' {} a -> s {domains = a} :: App) Prelude.. Lens.mapping Lens._Coerce

-- | Whether to enable SSL for the app.
app_enableSsl :: Lens.Lens' App (Prelude.Maybe Prelude.Bool)
app_enableSsl = Lens.lens (\App' {enableSsl} -> enableSsl) (\s@App' {} a -> s {enableSsl = a} :: App)

-- | The app\'s short name.
app_shortname :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_shortname = Lens.lens (\App' {shortname} -> shortname) (\s@App' {} a -> s {shortname = a} :: App)

-- | When the app was created.
app_createdAt :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_createdAt = Lens.lens (\App' {createdAt} -> createdAt) (\s@App' {} a -> s {createdAt = a} :: App)

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variable names, values, and protected flag values - cannot exceed 20 KB.
-- This limit should accommodate most if not all use cases, but if you do
-- exceed it, you will cause an exception (API) with an \"Environment: is
-- too large (maximum is 20 KB)\" message.
app_environment :: Lens.Lens' App (Prelude.Maybe [EnvironmentVariable])
app_environment = Lens.lens (\App' {environment} -> environment) (\s@App' {} a -> s {environment = a} :: App) Prelude.. Lens.mapping Lens._Coerce

-- | The stack attributes.
app_attributes :: Lens.Lens' App (Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text))
app_attributes = Lens.lens (\App' {attributes} -> attributes) (\s@App' {} a -> s {attributes = a} :: App) Prelude.. Lens.mapping Lens._Coerce

-- | The app name.
app_name :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_name = Lens.lens (\App' {name} -> name) (\s@App' {} a -> s {name = a} :: App)

-- | A description of the app.
app_description :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_description = Lens.lens (\App' {description} -> description) (\s@App' {} a -> s {description = a} :: App)

-- | The app type.
app_type :: Lens.Lens' App (Prelude.Maybe AppType)
app_type = Lens.lens (\App' {type'} -> type') (\s@App' {} a -> s {type' = a} :: App)

instance Core.FromJSON App where
  parseJSON =
    Core.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Core..:? "SslConfiguration")
            Prelude.<*> (x Core..:? "AppSource")
            Prelude.<*> (x Core..:? "AppId")
            Prelude.<*> (x Core..:? "DataSources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StackId")
            Prelude.<*> (x Core..:? "Domains" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EnableSsl")
            Prelude.<*> (x Core..:? "Shortname")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable App

instance Prelude.NFData App
