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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.App where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | A @Source@ object that describes the app repository.
    appSource :: Prelude.Maybe Source,
    -- | The stack attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text),
    -- | When the app was created.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The app\'s data sources.
    dataSources :: Prelude.Maybe [DataSource],
    -- | A description of the app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The app vhost settings with multiple domains separated by commas. For
    -- example: @\'www.example.com, example.com\'@
    domains :: Prelude.Maybe [Prelude.Text],
    -- | Whether to enable SSL for the app.
    enableSsl :: Prelude.Maybe Prelude.Bool,
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
    -- | The app name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The app\'s short name.
    shortname :: Prelude.Maybe Prelude.Text,
    -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Prelude.Maybe SslConfiguration,
    -- | The app stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
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
-- 'appId', 'app_appId' - The app ID.
--
-- 'appSource', 'app_appSource' - A @Source@ object that describes the app repository.
--
-- 'attributes', 'app_attributes' - The stack attributes.
--
-- 'createdAt', 'app_createdAt' - When the app was created.
--
-- 'dataSources', 'app_dataSources' - The app\'s data sources.
--
-- 'description', 'app_description' - A description of the app.
--
-- 'domains', 'app_domains' - The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
--
-- 'enableSsl', 'app_enableSsl' - Whether to enable SSL for the app.
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
-- 'name', 'app_name' - The app name.
--
-- 'shortname', 'app_shortname' - The app\'s short name.
--
-- 'sslConfiguration', 'app_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- 'stackId', 'app_stackId' - The app stack ID.
--
-- 'type'', 'app_type' - The app type.
newApp ::
  App
newApp =
  App'
    { appId = Prelude.Nothing,
      appSource = Prelude.Nothing,
      attributes = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      description = Prelude.Nothing,
      domains = Prelude.Nothing,
      enableSsl = Prelude.Nothing,
      environment = Prelude.Nothing,
      name = Prelude.Nothing,
      shortname = Prelude.Nothing,
      sslConfiguration = Prelude.Nothing,
      stackId = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The app ID.
app_appId :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_appId = Lens.lens (\App' {appId} -> appId) (\s@App' {} a -> s {appId = a} :: App)

-- | A @Source@ object that describes the app repository.
app_appSource :: Lens.Lens' App (Prelude.Maybe Source)
app_appSource = Lens.lens (\App' {appSource} -> appSource) (\s@App' {} a -> s {appSource = a} :: App)

-- | The stack attributes.
app_attributes :: Lens.Lens' App (Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text))
app_attributes = Lens.lens (\App' {attributes} -> attributes) (\s@App' {} a -> s {attributes = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | When the app was created.
app_createdAt :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_createdAt = Lens.lens (\App' {createdAt} -> createdAt) (\s@App' {} a -> s {createdAt = a} :: App)

-- | The app\'s data sources.
app_dataSources :: Lens.Lens' App (Prelude.Maybe [DataSource])
app_dataSources = Lens.lens (\App' {dataSources} -> dataSources) (\s@App' {} a -> s {dataSources = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | A description of the app.
app_description :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_description = Lens.lens (\App' {description} -> description) (\s@App' {} a -> s {description = a} :: App)

-- | The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
app_domains :: Lens.Lens' App (Prelude.Maybe [Prelude.Text])
app_domains = Lens.lens (\App' {domains} -> domains) (\s@App' {} a -> s {domains = a} :: App) Prelude.. Lens.mapping Lens.coerced

-- | Whether to enable SSL for the app.
app_enableSsl :: Lens.Lens' App (Prelude.Maybe Prelude.Bool)
app_enableSsl = Lens.lens (\App' {enableSsl} -> enableSsl) (\s@App' {} a -> s {enableSsl = a} :: App)

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

-- | The app name.
app_name :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_name = Lens.lens (\App' {name} -> name) (\s@App' {} a -> s {name = a} :: App)

-- | The app\'s short name.
app_shortname :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_shortname = Lens.lens (\App' {shortname} -> shortname) (\s@App' {} a -> s {shortname = a} :: App)

-- | An @SslConfiguration@ object with the SSL configuration.
app_sslConfiguration :: Lens.Lens' App (Prelude.Maybe SslConfiguration)
app_sslConfiguration = Lens.lens (\App' {sslConfiguration} -> sslConfiguration) (\s@App' {} a -> s {sslConfiguration = a} :: App)

-- | The app stack ID.
app_stackId :: Lens.Lens' App (Prelude.Maybe Prelude.Text)
app_stackId = Lens.lens (\App' {stackId} -> stackId) (\s@App' {} a -> s {stackId = a} :: App)

-- | The app type.
app_type :: Lens.Lens' App (Prelude.Maybe AppType)
app_type = Lens.lens (\App' {type'} -> type') (\s@App' {} a -> s {type' = a} :: App)

instance Data.FromJSON App where
  parseJSON =
    Data.withObject
      "App"
      ( \x ->
          App'
            Prelude.<$> (x Data..:? "AppId")
            Prelude.<*> (x Data..:? "AppSource")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DataSources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Domains" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EnableSsl")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Shortname")
            Prelude.<*> (x Data..:? "SslConfiguration")
            Prelude.<*> (x Data..:? "StackId")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable App where
  hashWithSalt _salt App' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` appSource
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domains
      `Prelude.hashWithSalt` enableSsl
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` shortname
      `Prelude.hashWithSalt` sslConfiguration
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData App where
  rnf App' {..} =
    Prelude.rnf appId `Prelude.seq`
      Prelude.rnf appSource `Prelude.seq`
        Prelude.rnf attributes `Prelude.seq`
          Prelude.rnf createdAt `Prelude.seq`
            Prelude.rnf dataSources `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf domains `Prelude.seq`
                  Prelude.rnf enableSsl `Prelude.seq`
                    Prelude.rnf environment `Prelude.seq`
                      Prelude.rnf name `Prelude.seq`
                        Prelude.rnf shortname `Prelude.seq`
                          Prelude.rnf sslConfiguration `Prelude.seq`
                            Prelude.rnf stackId `Prelude.seq`
                              Prelude.rnf type'
