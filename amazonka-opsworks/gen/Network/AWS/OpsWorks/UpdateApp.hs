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
-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_sslConfiguration,
    updateApp_appSource,
    updateApp_dataSources,
    updateApp_domains,
    updateApp_enableSsl,
    updateApp_environment,
    updateApp_attributes,
    updateApp_name,
    updateApp_description,
    updateApp_type,
    updateApp_appId,

    -- * Destructuring the Response
    UpdateAppResponse (..),
    newUpdateAppResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Core.Maybe SslConfiguration,
    -- | A @Source@ object that specifies the app repository.
    appSource :: Core.Maybe Source,
    -- | The app\'s data sources.
    dataSources :: Core.Maybe [DataSource],
    -- | The app\'s virtual host settings, with multiple domains separated by
    -- commas. For example: @\'www.example.com, example.com\'@
    domains :: Core.Maybe [Core.Text],
    -- | Whether SSL is enabled for the app.
    enableSsl :: Core.Maybe Core.Bool,
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
    environment :: Core.Maybe [EnvironmentVariable],
    -- | One or more user-defined key\/value pairs to be added to the stack
    -- attributes.
    attributes :: Core.Maybe (Core.HashMap AppAttributesKeys Core.Text),
    -- | The app name.
    name :: Core.Maybe Core.Text,
    -- | A description of the app.
    description :: Core.Maybe Core.Text,
    -- | The app type.
    type' :: Core.Maybe AppType,
    -- | The app ID.
    appId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslConfiguration', 'updateApp_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- 'appSource', 'updateApp_appSource' - A @Source@ object that specifies the app repository.
--
-- 'dataSources', 'updateApp_dataSources' - The app\'s data sources.
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
-- 'attributes', 'updateApp_attributes' - One or more user-defined key\/value pairs to be added to the stack
-- attributes.
--
-- 'name', 'updateApp_name' - The app name.
--
-- 'description', 'updateApp_description' - A description of the app.
--
-- 'type'', 'updateApp_type' - The app type.
--
-- 'appId', 'updateApp_appId' - The app ID.
newUpdateApp ::
  -- | 'appId'
  Core.Text ->
  UpdateApp
newUpdateApp pAppId_ =
  UpdateApp'
    { sslConfiguration = Core.Nothing,
      appSource = Core.Nothing,
      dataSources = Core.Nothing,
      domains = Core.Nothing,
      enableSsl = Core.Nothing,
      environment = Core.Nothing,
      attributes = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      appId = pAppId_
    }

-- | An @SslConfiguration@ object with the SSL configuration.
updateApp_sslConfiguration :: Lens.Lens' UpdateApp (Core.Maybe SslConfiguration)
updateApp_sslConfiguration = Lens.lens (\UpdateApp' {sslConfiguration} -> sslConfiguration) (\s@UpdateApp' {} a -> s {sslConfiguration = a} :: UpdateApp)

-- | A @Source@ object that specifies the app repository.
updateApp_appSource :: Lens.Lens' UpdateApp (Core.Maybe Source)
updateApp_appSource = Lens.lens (\UpdateApp' {appSource} -> appSource) (\s@UpdateApp' {} a -> s {appSource = a} :: UpdateApp)

-- | The app\'s data sources.
updateApp_dataSources :: Lens.Lens' UpdateApp (Core.Maybe [DataSource])
updateApp_dataSources = Lens.lens (\UpdateApp' {dataSources} -> dataSources) (\s@UpdateApp' {} a -> s {dataSources = a} :: UpdateApp) Core.. Lens.mapping Lens._Coerce

-- | The app\'s virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
updateApp_domains :: Lens.Lens' UpdateApp (Core.Maybe [Core.Text])
updateApp_domains = Lens.lens (\UpdateApp' {domains} -> domains) (\s@UpdateApp' {} a -> s {domains = a} :: UpdateApp) Core.. Lens.mapping Lens._Coerce

-- | Whether SSL is enabled for the app.
updateApp_enableSsl :: Lens.Lens' UpdateApp (Core.Maybe Core.Bool)
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
updateApp_environment :: Lens.Lens' UpdateApp (Core.Maybe [EnvironmentVariable])
updateApp_environment = Lens.lens (\UpdateApp' {environment} -> environment) (\s@UpdateApp' {} a -> s {environment = a} :: UpdateApp) Core.. Lens.mapping Lens._Coerce

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
updateApp_attributes :: Lens.Lens' UpdateApp (Core.Maybe (Core.HashMap AppAttributesKeys Core.Text))
updateApp_attributes = Lens.lens (\UpdateApp' {attributes} -> attributes) (\s@UpdateApp' {} a -> s {attributes = a} :: UpdateApp) Core.. Lens.mapping Lens._Coerce

-- | The app name.
updateApp_name :: Lens.Lens' UpdateApp (Core.Maybe Core.Text)
updateApp_name = Lens.lens (\UpdateApp' {name} -> name) (\s@UpdateApp' {} a -> s {name = a} :: UpdateApp)

-- | A description of the app.
updateApp_description :: Lens.Lens' UpdateApp (Core.Maybe Core.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

-- | The app type.
updateApp_type :: Lens.Lens' UpdateApp (Core.Maybe AppType)
updateApp_type = Lens.lens (\UpdateApp' {type'} -> type') (\s@UpdateApp' {} a -> s {type' = a} :: UpdateApp)

-- | The app ID.
updateApp_appId :: Lens.Lens' UpdateApp Core.Text
updateApp_appId = Lens.lens (\UpdateApp' {appId} -> appId) (\s@UpdateApp' {} a -> s {appId = a} :: UpdateApp)

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull UpdateAppResponse'

instance Core.Hashable UpdateApp

instance Core.NFData UpdateApp

instance Core.ToHeaders UpdateApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.UpdateApp" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SslConfiguration" Core..=)
              Core.<$> sslConfiguration,
            ("AppSource" Core..=) Core.<$> appSource,
            ("DataSources" Core..=) Core.<$> dataSources,
            ("Domains" Core..=) Core.<$> domains,
            ("EnableSsl" Core..=) Core.<$> enableSsl,
            ("Environment" Core..=) Core.<$> environment,
            ("Attributes" Core..=) Core.<$> attributes,
            ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description,
            ("Type" Core..=) Core.<$> type',
            Core.Just ("AppId" Core..= appId)
          ]
      )

instance Core.ToPath UpdateApp where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAppResponse ::
  UpdateAppResponse
newUpdateAppResponse = UpdateAppResponse'

instance Core.NFData UpdateAppResponse
