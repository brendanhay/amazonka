{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified app.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Deploy or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_appSource,
    updateApp_attributes,
    updateApp_dataSources,
    updateApp_description,
    updateApp_domains,
    updateApp_enableSsl,
    updateApp_environment,
    updateApp_name,
    updateApp_sslConfiguration,
    updateApp_type,
    updateApp_appId,

    -- * Destructuring the Response
    UpdateAppResponse (..),
    newUpdateAppResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | A @Source@ object that specifies the app repository.
    appSource :: Prelude.Maybe Source,
    -- | One or more user-defined key\/value pairs to be added to the stack
    -- attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text),
    -- | The app\'s data sources.
    dataSources :: Prelude.Maybe [DataSource],
    -- | A description of the app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The app\'s virtual host settings, with multiple domains separated by
    -- commas. For example: @\'www.example.com, example.com\'@
    domains :: Prelude.Maybe [Prelude.Text],
    -- | Whether SSL is enabled for the app.
    enableSsl :: Prelude.Maybe Prelude.Bool,
    -- | An array of @EnvironmentVariable@ objects that specify environment
    -- variables to be associated with the app. After you deploy the app, these
    -- variables are defined on the associated app server instances.For more
    -- information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
    --
    -- There is no specific limit on the number of environment variables.
    -- However, the size of the associated data structure - which includes the
    -- variables\' names, values, and protected flag values - cannot exceed 20
    -- KB. This limit should accommodate most if not all use cases. Exceeding
    -- it will cause an exception with the message, \"Environment: is too large
    -- (maximum is 20 KB).\"
    --
    -- If you have specified one or more environment variables, you cannot
    -- modify the stack\'s Chef version.
    environment :: Prelude.Maybe [EnvironmentVariable],
    -- | The app name.
    name :: Prelude.Maybe Prelude.Text,
    -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Prelude.Maybe SslConfiguration,
    -- | The app type.
    type' :: Prelude.Maybe AppType,
    -- | The app ID.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appSource', 'updateApp_appSource' - A @Source@ object that specifies the app repository.
--
-- 'attributes', 'updateApp_attributes' - One or more user-defined key\/value pairs to be added to the stack
-- attributes.
--
-- 'dataSources', 'updateApp_dataSources' - The app\'s data sources.
--
-- 'description', 'updateApp_description' - A description of the app.
--
-- 'domains', 'updateApp_domains' - The app\'s virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
--
-- 'enableSsl', 'updateApp_enableSsl' - Whether SSL is enabled for the app.
--
-- 'environment', 'updateApp_environment' - An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances.For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 20
-- KB. This limit should accommodate most if not all use cases. Exceeding
-- it will cause an exception with the message, \"Environment: is too large
-- (maximum is 20 KB).\"
--
-- If you have specified one or more environment variables, you cannot
-- modify the stack\'s Chef version.
--
-- 'name', 'updateApp_name' - The app name.
--
-- 'sslConfiguration', 'updateApp_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- 'type'', 'updateApp_type' - The app type.
--
-- 'appId', 'updateApp_appId' - The app ID.
newUpdateApp ::
  -- | 'appId'
  Prelude.Text ->
  UpdateApp
newUpdateApp pAppId_ =
  UpdateApp'
    { appSource = Prelude.Nothing,
      attributes = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      description = Prelude.Nothing,
      domains = Prelude.Nothing,
      enableSsl = Prelude.Nothing,
      environment = Prelude.Nothing,
      name = Prelude.Nothing,
      sslConfiguration = Prelude.Nothing,
      type' = Prelude.Nothing,
      appId = pAppId_
    }

-- | A @Source@ object that specifies the app repository.
updateApp_appSource :: Lens.Lens' UpdateApp (Prelude.Maybe Source)
updateApp_appSource = Lens.lens (\UpdateApp' {appSource} -> appSource) (\s@UpdateApp' {} a -> s {appSource = a} :: UpdateApp)

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
updateApp_attributes :: Lens.Lens' UpdateApp (Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text))
updateApp_attributes = Lens.lens (\UpdateApp' {attributes} -> attributes) (\s@UpdateApp' {} a -> s {attributes = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The app\'s data sources.
updateApp_dataSources :: Lens.Lens' UpdateApp (Prelude.Maybe [DataSource])
updateApp_dataSources = Lens.lens (\UpdateApp' {dataSources} -> dataSources) (\s@UpdateApp' {} a -> s {dataSources = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | A description of the app.
updateApp_description :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

-- | The app\'s virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
updateApp_domains :: Lens.Lens' UpdateApp (Prelude.Maybe [Prelude.Text])
updateApp_domains = Lens.lens (\UpdateApp' {domains} -> domains) (\s@UpdateApp' {} a -> s {domains = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | Whether SSL is enabled for the app.
updateApp_enableSsl :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_enableSsl = Lens.lens (\UpdateApp' {enableSsl} -> enableSsl) (\s@UpdateApp' {} a -> s {enableSsl = a} :: UpdateApp)

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances.For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 20
-- KB. This limit should accommodate most if not all use cases. Exceeding
-- it will cause an exception with the message, \"Environment: is too large
-- (maximum is 20 KB).\"
--
-- If you have specified one or more environment variables, you cannot
-- modify the stack\'s Chef version.
updateApp_environment :: Lens.Lens' UpdateApp (Prelude.Maybe [EnvironmentVariable])
updateApp_environment = Lens.lens (\UpdateApp' {environment} -> environment) (\s@UpdateApp' {} a -> s {environment = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The app name.
updateApp_name :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_name = Lens.lens (\UpdateApp' {name} -> name) (\s@UpdateApp' {} a -> s {name = a} :: UpdateApp)

-- | An @SslConfiguration@ object with the SSL configuration.
updateApp_sslConfiguration :: Lens.Lens' UpdateApp (Prelude.Maybe SslConfiguration)
updateApp_sslConfiguration = Lens.lens (\UpdateApp' {sslConfiguration} -> sslConfiguration) (\s@UpdateApp' {} a -> s {sslConfiguration = a} :: UpdateApp)

-- | The app type.
updateApp_type :: Lens.Lens' UpdateApp (Prelude.Maybe AppType)
updateApp_type = Lens.lens (\UpdateApp' {type'} -> type') (\s@UpdateApp' {} a -> s {type' = a} :: UpdateApp)

-- | The app ID.
updateApp_appId :: Lens.Lens' UpdateApp Prelude.Text
updateApp_appId = Lens.lens (\UpdateApp' {appId} -> appId) (\s@UpdateApp' {} a -> s {appId = a} :: UpdateApp)

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull UpdateAppResponse'

instance Prelude.Hashable UpdateApp where
  hashWithSalt _salt UpdateApp' {..} =
    _salt `Prelude.hashWithSalt` appSource
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domains
      `Prelude.hashWithSalt` enableSsl
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sslConfiguration
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` appId

instance Prelude.NFData UpdateApp where
  rnf UpdateApp' {..} =
    Prelude.rnf appSource
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domains
      `Prelude.seq` Prelude.rnf enableSsl
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sslConfiguration
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders UpdateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.UpdateApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppSource" Data..=) Prelude.<$> appSource,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("DataSources" Data..=) Prelude.<$> dataSources,
            ("Description" Data..=) Prelude.<$> description,
            ("Domains" Data..=) Prelude.<$> domains,
            ("EnableSsl" Data..=) Prelude.<$> enableSsl,
            ("Environment" Data..=) Prelude.<$> environment,
            ("Name" Data..=) Prelude.<$> name,
            ("SslConfiguration" Data..=)
              Prelude.<$> sslConfiguration,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("AppId" Data..= appId)
          ]
      )

instance Data.ToPath UpdateApp where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAppResponse ::
  UpdateAppResponse
newUpdateAppResponse = UpdateAppResponse'

instance Prelude.NFData UpdateAppResponse where
  rnf _ = ()
