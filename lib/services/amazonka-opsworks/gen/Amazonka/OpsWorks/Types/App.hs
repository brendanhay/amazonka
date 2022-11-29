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
-- Module      : Amazonka.OpsWorks.Types.App
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.App where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types.AppAttributesKeys
import Amazonka.OpsWorks.Types.AppType
import Amazonka.OpsWorks.Types.DataSource
import Amazonka.OpsWorks.Types.EnvironmentVariable
import Amazonka.OpsWorks.Types.Source
import Amazonka.OpsWorks.Types.SslConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A description of the app.
--
-- /See:/ 'newApp' smart constructor.
data App = App'
  { -- | The app vhost settings with multiple domains separated by commas. For
    -- example: @\'www.example.com, example.com\'@
    domains :: Prelude.Maybe [Prelude.Text],
    -- | The app stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | A @Source@ object that describes the app repository.
    appSource :: Prelude.Maybe Source,
    -- | The app name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The app type.
    type' :: Prelude.Maybe AppType,
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
    -- | The app\'s data sources.
    dataSources :: Prelude.Maybe [DataSource],
    -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Prelude.Maybe SslConfiguration,
    -- | A description of the app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stack attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text),
    -- | Whether to enable SSL for the app.
    enableSsl :: Prelude.Maybe Prelude.Bool,
    -- | The app\'s short name.
    shortname :: Prelude.Maybe Prelude.Text,
    -- | When the app was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text
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
-- 'domains', 'app_domains' - The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
--
-- 'stackId', 'app_stackId' - The app stack ID.
--
-- 'appSource', 'app_appSource' - A @Source@ object that describes the app repository.
--
-- 'name', 'app_name' - The app name.
--
-- 'type'', 'app_type' - The app type.
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
-- 'dataSources', 'app_dataSources' - The app\'s data sources.
--
-- 'sslConfiguration', 'app_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- 'description', 'app_description' - A description of the app.
--
-- 'attributes', 'app_attributes' - The stack attributes.
--
-- 'enableSsl', 'app_enableSsl' - Whether to enable SSL for the app.
--
-- 'shortname', 'app_shortname' - The app\'s short name.
--
-- 'createdAt', 'app_createdAt' - When the app was created.
--
-- 'appId', 'app_appId' - The app ID.
newApp ::
  App
newApp =
  App'
    { domains = Prelude.Nothing,
      stackId = Prelude.Nothing,
      appSource = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      environment = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      sslConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      attributes = Prelude.Nothing,
      enableSsl = Prelude.Nothing,
      shortname = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
app_domains :: Lens.Lens' App (Prelude.Maybe [Prelude.Text])
app_domains = Lens.lens (\App' {domains} -> domains) (\s@App' {} a -> s {domains = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | The app stack ID.
app_stackId :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_stackId = Lens.lens (\App' {stackId} -> stackId) (\s@App' {} a -> s {stackId = a} :: App)

-- | A @Source@ object that describes the app repository.
app_appSource :: Lens.Lens' App (Prelude.Maybe Source)
app_appSource = Lens.lens (\App' {appSource} -> appSource) (\s@App' {} a -> s {appSource = a} :: App)

-- | The app name.
app_name :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_name = Lens.lens (\App' {name} -> name) (\s@App' {} a -> s {name = a} :: App)

-- | The app type.
app_type :: Lens.Lens' App (Prelude.Maybe AppType)
app_type = Lens.lens (\App' {type'} -> type') (\s@App' {} a -> s {type' = a} :: App)

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
app_environment = Lens.lens (\App' {environment} -> environment) (\s@App' {} a -> s {environment = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | The app\'s data sources.
app_dataSources :: Lens.Lens' App (Prelude.Maybe [DataSource])
app_dataSources = Lens.lens (\App' {dataSources} -> dataSources) (\s@App' {} a -> s {dataSources = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | An @SslConfiguration@ object with the SSL configuration.
app_sslConfiguration :: Lens.Lens' App (Prelude.Maybe SslConfiguration)
app_sslConfiguration = Lens.lens (\App' {sslConfiguration} -> sslConfiguration) (\s@App' {} a -> s {sslConfiguration = a} :: App)

-- | A description of the app.
app_description :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_description = Lens.lens (\App' {description} -> description) (\s@App' {} a -> s {description = a} :: App)

-- | The stack attributes.
app_attributes :: Lens.Lens' App (Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text))
app_attributes = Lens.lens (\App' {attributes} -> attributes) (\s@App' {} a -> s {attributes = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | Whether to enable SSL for the app.
app_enableSsl :: Lens.Lens' App (Prelude.Maybe Prelude.Bool)
app_enableSsl = Lens.lens (\App' {enableSsl} -> enableSsl) (\s@App' {} a -> s {enableSsl = a} :: App)

-- | The app\'s short name.
app_shortname :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_shortname = Lens.lens (\App' {shortname} -> shortname) (\s@App' {} a -> s {shortname = a} :: App)

-- | When the app was created.
app_createdAt :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_createdAt = Lens.lens (\App' {createdAt} -> createdAt) (\s@App' {} a -> s {createdAt = a} :: App)

-- | The app ID.
app_appId :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_appId = Lens.lens (\App' {appId} -> appId) (\s@App' {} a -> s {appId = a} :: App)

instance Core.FromJSON App where
  parseJSON =
    Core.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Core..:? "Domains" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StackId")
            Prelude.<*> (x Core..:? "AppSource")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DataSources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SslConfiguration")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EnableSsl")
            Prelude.<*> (x Core..:? "Shortname")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "AppId")
      )

instance Prelude.Hashable App where
  hashWithSalt _salt App' {..} =
    _salt `Prelude.hashWithSalt` domains
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` appSource
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` sslConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` enableSsl
      `Prelude.hashWithSalt` shortname
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` appId

instance Prelude.NFData App where
  rnf App' {..} =
    Prelude.rnf domains
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf appSource
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf sslConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf enableSsl
      `Prelude.seq` Prelude.rnf shortname
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf appId
