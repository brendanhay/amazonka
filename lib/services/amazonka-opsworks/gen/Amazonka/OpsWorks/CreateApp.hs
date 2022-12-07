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
-- Module      : Amazonka.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an app for a specified stack. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_domains,
    createApp_appSource,
    createApp_environment,
    createApp_dataSources,
    createApp_sslConfiguration,
    createApp_description,
    createApp_attributes,
    createApp_enableSsl,
    createApp_shortname,
    createApp_stackId,
    createApp_name,
    createApp_type,

    -- * Destructuring the Response
    CreateAppResponse (..),
    newCreateAppResponse,

    -- * Response Lenses
    createAppResponse_appId,
    createAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The app virtual host settings, with multiple domains separated by
    -- commas. For example: @\'www.example.com, example.com\'@
    domains :: Prelude.Maybe [Prelude.Text],
    -- | A @Source@ object that specifies the app repository.
    appSource :: Prelude.Maybe Source,
    -- | An array of @EnvironmentVariable@ objects that specify environment
    -- variables to be associated with the app. After you deploy the app, these
    -- variables are defined on the associated app server instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
    --
    -- There is no specific limit on the number of environment variables.
    -- However, the size of the associated data structure - which includes the
    -- variables\' names, values, and protected flag values - cannot exceed 20
    -- KB. This limit should accommodate most if not all use cases. Exceeding
    -- it will cause an exception with the message, \"Environment: is too large
    -- (maximum is 20KB).\"
    --
    -- If you have specified one or more environment variables, you cannot
    -- modify the stack\'s Chef version.
    environment :: Prelude.Maybe [EnvironmentVariable],
    -- | The app\'s data source.
    dataSources :: Prelude.Maybe [DataSource],
    -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Prelude.Maybe SslConfiguration,
    -- | A description of the app.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more user-defined key\/value pairs to be added to the stack
    -- attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text),
    -- | Whether to enable SSL for the app.
    enableSsl :: Prelude.Maybe Prelude.Bool,
    -- | The app\'s short name.
    shortname :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Text,
    -- | The app name.
    name :: Prelude.Text,
    -- | The app type. Each supported type is associated with a particular layer.
    -- For example, PHP applications are associated with a PHP layer. AWS
    -- OpsWorks Stacks deploys an application to those instances that are
    -- members of the corresponding layer. If your app isn\'t one of the
    -- standard types, or you prefer to implement your own Deploy recipes,
    -- specify @other@.
    type' :: AppType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domains', 'createApp_domains' - The app virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
--
-- 'appSource', 'createApp_appSource' - A @Source@ object that specifies the app repository.
--
-- 'environment', 'createApp_environment' - An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instance. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 20
-- KB. This limit should accommodate most if not all use cases. Exceeding
-- it will cause an exception with the message, \"Environment: is too large
-- (maximum is 20KB).\"
--
-- If you have specified one or more environment variables, you cannot
-- modify the stack\'s Chef version.
--
-- 'dataSources', 'createApp_dataSources' - The app\'s data source.
--
-- 'sslConfiguration', 'createApp_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- 'description', 'createApp_description' - A description of the app.
--
-- 'attributes', 'createApp_attributes' - One or more user-defined key\/value pairs to be added to the stack
-- attributes.
--
-- 'enableSsl', 'createApp_enableSsl' - Whether to enable SSL for the app.
--
-- 'shortname', 'createApp_shortname' - The app\'s short name.
--
-- 'stackId', 'createApp_stackId' - The stack ID.
--
-- 'name', 'createApp_name' - The app name.
--
-- 'type'', 'createApp_type' - The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS
-- OpsWorks Stacks deploys an application to those instances that are
-- members of the corresponding layer. If your app isn\'t one of the
-- standard types, or you prefer to implement your own Deploy recipes,
-- specify @other@.
newCreateApp ::
  -- | 'stackId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  AppType ->
  CreateApp
newCreateApp pStackId_ pName_ pType_ =
  CreateApp'
    { domains = Prelude.Nothing,
      appSource = Prelude.Nothing,
      environment = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      sslConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      attributes = Prelude.Nothing,
      enableSsl = Prelude.Nothing,
      shortname = Prelude.Nothing,
      stackId = pStackId_,
      name = pName_,
      type' = pType_
    }

-- | The app virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
createApp_domains :: Lens.Lens' CreateApp (Prelude.Maybe [Prelude.Text])
createApp_domains = Lens.lens (\CreateApp' {domains} -> domains) (\s@CreateApp' {} a -> s {domains = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | A @Source@ object that specifies the app repository.
createApp_appSource :: Lens.Lens' CreateApp (Prelude.Maybe Source)
createApp_appSource = Lens.lens (\CreateApp' {appSource} -> appSource) (\s@CreateApp' {} a -> s {appSource = a} :: CreateApp)

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instance. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 20
-- KB. This limit should accommodate most if not all use cases. Exceeding
-- it will cause an exception with the message, \"Environment: is too large
-- (maximum is 20KB).\"
--
-- If you have specified one or more environment variables, you cannot
-- modify the stack\'s Chef version.
createApp_environment :: Lens.Lens' CreateApp (Prelude.Maybe [EnvironmentVariable])
createApp_environment = Lens.lens (\CreateApp' {environment} -> environment) (\s@CreateApp' {} a -> s {environment = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | The app\'s data source.
createApp_dataSources :: Lens.Lens' CreateApp (Prelude.Maybe [DataSource])
createApp_dataSources = Lens.lens (\CreateApp' {dataSources} -> dataSources) (\s@CreateApp' {} a -> s {dataSources = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | An @SslConfiguration@ object with the SSL configuration.
createApp_sslConfiguration :: Lens.Lens' CreateApp (Prelude.Maybe SslConfiguration)
createApp_sslConfiguration = Lens.lens (\CreateApp' {sslConfiguration} -> sslConfiguration) (\s@CreateApp' {} a -> s {sslConfiguration = a} :: CreateApp)

-- | A description of the app.
createApp_description :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_description = Lens.lens (\CreateApp' {description} -> description) (\s@CreateApp' {} a -> s {description = a} :: CreateApp)

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
createApp_attributes :: Lens.Lens' CreateApp (Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text))
createApp_attributes = Lens.lens (\CreateApp' {attributes} -> attributes) (\s@CreateApp' {} a -> s {attributes = a} :: CreateApp) Prelude.. Lens.mapping Lens.coerced

-- | Whether to enable SSL for the app.
createApp_enableSsl :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Bool)
createApp_enableSsl = Lens.lens (\CreateApp' {enableSsl} -> enableSsl) (\s@CreateApp' {} a -> s {enableSsl = a} :: CreateApp)

-- | The app\'s short name.
createApp_shortname :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_shortname = Lens.lens (\CreateApp' {shortname} -> shortname) (\s@CreateApp' {} a -> s {shortname = a} :: CreateApp)

-- | The stack ID.
createApp_stackId :: Lens.Lens' CreateApp Prelude.Text
createApp_stackId = Lens.lens (\CreateApp' {stackId} -> stackId) (\s@CreateApp' {} a -> s {stackId = a} :: CreateApp)

-- | The app name.
createApp_name :: Lens.Lens' CreateApp Prelude.Text
createApp_name = Lens.lens (\CreateApp' {name} -> name) (\s@CreateApp' {} a -> s {name = a} :: CreateApp)

-- | The app type. Each supported type is associated with a particular layer.
-- For example, PHP applications are associated with a PHP layer. AWS
-- OpsWorks Stacks deploys an application to those instances that are
-- members of the corresponding layer. If your app isn\'t one of the
-- standard types, or you prefer to implement your own Deploy recipes,
-- specify @other@.
createApp_type :: Lens.Lens' CreateApp AppType
createApp_type = Lens.lens (\CreateApp' {type'} -> type') (\s@CreateApp' {} a -> s {type' = a} :: CreateApp)

instance Core.AWSRequest CreateApp where
  type AWSResponse CreateApp = CreateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (x Data..?> "AppId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApp where
  hashWithSalt _salt CreateApp' {..} =
    _salt `Prelude.hashWithSalt` domains
      `Prelude.hashWithSalt` appSource
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` sslConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` enableSsl
      `Prelude.hashWithSalt` shortname
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateApp where
  rnf CreateApp' {..} =
    Prelude.rnf domains
      `Prelude.seq` Prelude.rnf appSource
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf sslConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf enableSsl
      `Prelude.seq` Prelude.rnf shortname
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.CreateApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domains" Data..=) Prelude.<$> domains,
            ("AppSource" Data..=) Prelude.<$> appSource,
            ("Environment" Data..=) Prelude.<$> environment,
            ("DataSources" Data..=) Prelude.<$> dataSources,
            ("SslConfiguration" Data..=)
              Prelude.<$> sslConfiguration,
            ("Description" Data..=) Prelude.<$> description,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("EnableSsl" Data..=) Prelude.<$> enableSsl,
            ("Shortname" Data..=) Prelude.<$> shortname,
            Prelude.Just ("StackId" Data..= stackId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath CreateApp where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateApp where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @CreateApp@ request.
--
-- /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The app ID.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'createAppResponse_appId' - The app ID.
--
-- 'httpStatus', 'createAppResponse_httpStatus' - The response's http status code.
newCreateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppResponse
newCreateAppResponse pHttpStatus_ =
  CreateAppResponse'
    { appId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app ID.
createAppResponse_appId :: Lens.Lens' CreateAppResponse (Prelude.Maybe Prelude.Text)
createAppResponse_appId = Lens.lens (\CreateAppResponse' {appId} -> appId) (\s@CreateAppResponse' {} a -> s {appId = a} :: CreateAppResponse)

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Prelude.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

instance Prelude.NFData CreateAppResponse where
  rnf CreateAppResponse' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf httpStatus
