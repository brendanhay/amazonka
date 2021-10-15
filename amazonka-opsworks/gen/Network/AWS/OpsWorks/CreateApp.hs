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
-- Module      : Network.AWS.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_sslConfiguration,
    createApp_environment,
    createApp_enableSsl,
    createApp_shortname,
    createApp_dataSources,
    createApp_appSource,
    createApp_attributes,
    createApp_domains,
    createApp_description,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | An @SslConfiguration@ object with the SSL configuration.
    sslConfiguration :: Prelude.Maybe SslConfiguration,
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
    -- | Whether to enable SSL for the app.
    enableSsl :: Prelude.Maybe Prelude.Bool,
    -- | The app\'s short name.
    shortname :: Prelude.Maybe Prelude.Text,
    -- | The app\'s data source.
    dataSources :: Prelude.Maybe [DataSource],
    -- | A @Source@ object that specifies the app repository.
    appSource :: Prelude.Maybe Source,
    -- | One or more user-defined key\/value pairs to be added to the stack
    -- attributes.
    attributes :: Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text),
    -- | The app virtual host settings, with multiple domains separated by
    -- commas. For example: @\'www.example.com, example.com\'@
    domains :: Prelude.Maybe [Prelude.Text],
    -- | A description of the app.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'sslConfiguration', 'createApp_sslConfiguration' - An @SslConfiguration@ object with the SSL configuration.
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
-- 'enableSsl', 'createApp_enableSsl' - Whether to enable SSL for the app.
--
-- 'shortname', 'createApp_shortname' - The app\'s short name.
--
-- 'dataSources', 'createApp_dataSources' - The app\'s data source.
--
-- 'appSource', 'createApp_appSource' - A @Source@ object that specifies the app repository.
--
-- 'attributes', 'createApp_attributes' - One or more user-defined key\/value pairs to be added to the stack
-- attributes.
--
-- 'domains', 'createApp_domains' - The app virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
--
-- 'description', 'createApp_description' - A description of the app.
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
    { sslConfiguration = Prelude.Nothing,
      environment = Prelude.Nothing,
      enableSsl = Prelude.Nothing,
      shortname = Prelude.Nothing,
      dataSources = Prelude.Nothing,
      appSource = Prelude.Nothing,
      attributes = Prelude.Nothing,
      domains = Prelude.Nothing,
      description = Prelude.Nothing,
      stackId = pStackId_,
      name = pName_,
      type' = pType_
    }

-- | An @SslConfiguration@ object with the SSL configuration.
createApp_sslConfiguration :: Lens.Lens' CreateApp (Prelude.Maybe SslConfiguration)
createApp_sslConfiguration = Lens.lens (\CreateApp' {sslConfiguration} -> sslConfiguration) (\s@CreateApp' {} a -> s {sslConfiguration = a} :: CreateApp)

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
createApp_environment = Lens.lens (\CreateApp' {environment} -> environment) (\s@CreateApp' {} a -> s {environment = a} :: CreateApp) Prelude.. Lens.mapping Lens._Coerce

-- | Whether to enable SSL for the app.
createApp_enableSsl :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Bool)
createApp_enableSsl = Lens.lens (\CreateApp' {enableSsl} -> enableSsl) (\s@CreateApp' {} a -> s {enableSsl = a} :: CreateApp)

-- | The app\'s short name.
createApp_shortname :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_shortname = Lens.lens (\CreateApp' {shortname} -> shortname) (\s@CreateApp' {} a -> s {shortname = a} :: CreateApp)

-- | The app\'s data source.
createApp_dataSources :: Lens.Lens' CreateApp (Prelude.Maybe [DataSource])
createApp_dataSources = Lens.lens (\CreateApp' {dataSources} -> dataSources) (\s@CreateApp' {} a -> s {dataSources = a} :: CreateApp) Prelude.. Lens.mapping Lens._Coerce

-- | A @Source@ object that specifies the app repository.
createApp_appSource :: Lens.Lens' CreateApp (Prelude.Maybe Source)
createApp_appSource = Lens.lens (\CreateApp' {appSource} -> appSource) (\s@CreateApp' {} a -> s {appSource = a} :: CreateApp)

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
createApp_attributes :: Lens.Lens' CreateApp (Prelude.Maybe (Prelude.HashMap AppAttributesKeys Prelude.Text))
createApp_attributes = Lens.lens (\CreateApp' {attributes} -> attributes) (\s@CreateApp' {} a -> s {attributes = a} :: CreateApp) Prelude.. Lens.mapping Lens._Coerce

-- | The app virtual host settings, with multiple domains separated by
-- commas. For example: @\'www.example.com, example.com\'@
createApp_domains :: Lens.Lens' CreateApp (Prelude.Maybe [Prelude.Text])
createApp_domains = Lens.lens (\CreateApp' {domains} -> domains) (\s@CreateApp' {} a -> s {domains = a} :: CreateApp) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the app.
createApp_description :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_description = Lens.lens (\CreateApp' {description} -> description) (\s@CreateApp' {} a -> s {description = a} :: CreateApp)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (x Core..?> "AppId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApp

instance Prelude.NFData CreateApp

instance Core.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.CreateApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SslConfiguration" Core..=)
              Prelude.<$> sslConfiguration,
            ("Environment" Core..=) Prelude.<$> environment,
            ("EnableSsl" Core..=) Prelude.<$> enableSsl,
            ("Shortname" Core..=) Prelude.<$> shortname,
            ("DataSources" Core..=) Prelude.<$> dataSources,
            ("AppSource" Core..=) Prelude.<$> appSource,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Domains" Core..=) Prelude.<$> domains,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("StackId" Core..= stackId),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath CreateApp where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApp where
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

instance Prelude.NFData CreateAppResponse
