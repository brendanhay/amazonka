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
-- Module      : Amazonka.AppConfig.GetConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Receive information about a configuration.
--
-- AWS AppConfig uses the value of the @ClientConfigurationVersion@
-- parameter to identify the configuration version on your clients. If you
-- don’t send @ClientConfigurationVersion@ with each call to
-- @GetConfiguration@, your clients receive the current configuration. You
-- are charged each time your clients receive a configuration.
--
-- To avoid excess charges, we recommend that you include the
-- @ClientConfigurationVersion@ value with every call to
-- @GetConfiguration@. This value must be saved on your client. Subsequent
-- calls to @GetConfiguration@ must pass this value by using the
-- @ClientConfigurationVersion@ parameter.
module Amazonka.AppConfig.GetConfiguration
  ( -- * Creating a Request
    GetConfiguration (..),
    newGetConfiguration,

    -- * Request Lenses
    getConfiguration_clientConfigurationVersion,
    getConfiguration_application,
    getConfiguration_environment,
    getConfiguration_configuration,
    getConfiguration_clientId,

    -- * Destructuring the Response
    GetConfigurationResponse (..),
    newGetConfigurationResponse,

    -- * Response Lenses
    getConfigurationResponse_configurationVersion,
    getConfigurationResponse_content,
    getConfigurationResponse_contentType,
    getConfigurationResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConfiguration' smart constructor.
data GetConfiguration = GetConfiguration'
  { -- | The configuration version returned in the most recent @GetConfiguration@
    -- response.
    --
    -- AWS AppConfig uses the value of the @ClientConfigurationVersion@
    -- parameter to identify the configuration version on your clients. If you
    -- don’t send @ClientConfigurationVersion@ with each call to
    -- @GetConfiguration@, your clients receive the current configuration. You
    -- are charged each time your clients receive a configuration.
    --
    -- To avoid excess charges, we recommend that you include the
    -- @ClientConfigurationVersion@ value with every call to
    -- @GetConfiguration@. This value must be saved on your client. Subsequent
    -- calls to @GetConfiguration@ must pass this value by using the
    -- @ClientConfigurationVersion@ parameter.
    --
    -- For more information about working with configurations, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig-retrieving-the-configuration.html Retrieving the Configuration>
    -- in the /AWS AppConfig User Guide/.
    clientConfigurationVersion :: Prelude.Maybe Prelude.Text,
    -- | The application to get. Specify either the application name or the
    -- application ID.
    application :: Prelude.Text,
    -- | The environment to get. Specify either the environment name or the
    -- environment ID.
    environment :: Prelude.Text,
    -- | The configuration to get. Specify either the configuration name or the
    -- configuration ID.
    configuration :: Prelude.Text,
    -- | A unique ID to identify the client for the configuration. This ID
    -- enables AppConfig to deploy the configuration in intervals, as defined
    -- in the deployment strategy.
    clientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientConfigurationVersion', 'getConfiguration_clientConfigurationVersion' - The configuration version returned in the most recent @GetConfiguration@
-- response.
--
-- AWS AppConfig uses the value of the @ClientConfigurationVersion@
-- parameter to identify the configuration version on your clients. If you
-- don’t send @ClientConfigurationVersion@ with each call to
-- @GetConfiguration@, your clients receive the current configuration. You
-- are charged each time your clients receive a configuration.
--
-- To avoid excess charges, we recommend that you include the
-- @ClientConfigurationVersion@ value with every call to
-- @GetConfiguration@. This value must be saved on your client. Subsequent
-- calls to @GetConfiguration@ must pass this value by using the
-- @ClientConfigurationVersion@ parameter.
--
-- For more information about working with configurations, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig-retrieving-the-configuration.html Retrieving the Configuration>
-- in the /AWS AppConfig User Guide/.
--
-- 'application', 'getConfiguration_application' - The application to get. Specify either the application name or the
-- application ID.
--
-- 'environment', 'getConfiguration_environment' - The environment to get. Specify either the environment name or the
-- environment ID.
--
-- 'configuration', 'getConfiguration_configuration' - The configuration to get. Specify either the configuration name or the
-- configuration ID.
--
-- 'clientId', 'getConfiguration_clientId' - A unique ID to identify the client for the configuration. This ID
-- enables AppConfig to deploy the configuration in intervals, as defined
-- in the deployment strategy.
newGetConfiguration ::
  -- | 'application'
  Prelude.Text ->
  -- | 'environment'
  Prelude.Text ->
  -- | 'configuration'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  GetConfiguration
newGetConfiguration
  pApplication_
  pEnvironment_
  pConfiguration_
  pClientId_ =
    GetConfiguration'
      { clientConfigurationVersion =
          Prelude.Nothing,
        application = pApplication_,
        environment = pEnvironment_,
        configuration = pConfiguration_,
        clientId = pClientId_
      }

-- | The configuration version returned in the most recent @GetConfiguration@
-- response.
--
-- AWS AppConfig uses the value of the @ClientConfigurationVersion@
-- parameter to identify the configuration version on your clients. If you
-- don’t send @ClientConfigurationVersion@ with each call to
-- @GetConfiguration@, your clients receive the current configuration. You
-- are charged each time your clients receive a configuration.
--
-- To avoid excess charges, we recommend that you include the
-- @ClientConfigurationVersion@ value with every call to
-- @GetConfiguration@. This value must be saved on your client. Subsequent
-- calls to @GetConfiguration@ must pass this value by using the
-- @ClientConfigurationVersion@ parameter.
--
-- For more information about working with configurations, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig-retrieving-the-configuration.html Retrieving the Configuration>
-- in the /AWS AppConfig User Guide/.
getConfiguration_clientConfigurationVersion :: Lens.Lens' GetConfiguration (Prelude.Maybe Prelude.Text)
getConfiguration_clientConfigurationVersion = Lens.lens (\GetConfiguration' {clientConfigurationVersion} -> clientConfigurationVersion) (\s@GetConfiguration' {} a -> s {clientConfigurationVersion = a} :: GetConfiguration)

-- | The application to get. Specify either the application name or the
-- application ID.
getConfiguration_application :: Lens.Lens' GetConfiguration Prelude.Text
getConfiguration_application = Lens.lens (\GetConfiguration' {application} -> application) (\s@GetConfiguration' {} a -> s {application = a} :: GetConfiguration)

-- | The environment to get. Specify either the environment name or the
-- environment ID.
getConfiguration_environment :: Lens.Lens' GetConfiguration Prelude.Text
getConfiguration_environment = Lens.lens (\GetConfiguration' {environment} -> environment) (\s@GetConfiguration' {} a -> s {environment = a} :: GetConfiguration)

-- | The configuration to get. Specify either the configuration name or the
-- configuration ID.
getConfiguration_configuration :: Lens.Lens' GetConfiguration Prelude.Text
getConfiguration_configuration = Lens.lens (\GetConfiguration' {configuration} -> configuration) (\s@GetConfiguration' {} a -> s {configuration = a} :: GetConfiguration)

-- | A unique ID to identify the client for the configuration. This ID
-- enables AppConfig to deploy the configuration in intervals, as defined
-- in the deployment strategy.
getConfiguration_clientId :: Lens.Lens' GetConfiguration Prelude.Text
getConfiguration_clientId = Lens.lens (\GetConfiguration' {clientId} -> clientId) (\s@GetConfiguration' {} a -> s {clientId = a} :: GetConfiguration)

instance Core.AWSRequest GetConfiguration where
  type
    AWSResponse GetConfiguration =
      GetConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetConfigurationResponse'
            Prelude.<$> (h Core..#? "Configuration-Version")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConfiguration

instance Prelude.NFData GetConfiguration

instance Core.ToHeaders GetConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetConfiguration where
  toPath GetConfiguration' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS application,
        "/environments/",
        Core.toBS environment,
        "/configurations/",
        Core.toBS configuration
      ]

instance Core.ToQuery GetConfiguration where
  toQuery GetConfiguration' {..} =
    Prelude.mconcat
      [ "client_configuration_version"
          Core.=: clientConfigurationVersion,
        "client_id" Core.=: clientId
      ]

-- | /See:/ 'newGetConfigurationResponse' smart constructor.
data GetConfigurationResponse = GetConfigurationResponse'
  { -- | The configuration version.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | The content of the configuration or the configuration data.
    content :: Prelude.Maybe (Core.Sensitive Prelude.ByteString),
    -- | A standard MIME type describing the format of the configuration content.
    -- For more information, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationVersion', 'getConfigurationResponse_configurationVersion' - The configuration version.
--
-- 'content', 'getConfigurationResponse_content' - The content of the configuration or the configuration data.
--
-- 'contentType', 'getConfigurationResponse_contentType' - A standard MIME type describing the format of the configuration content.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
--
-- 'httpStatus', 'getConfigurationResponse_httpStatus' - The response's http status code.
newGetConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConfigurationResponse
newGetConfigurationResponse pHttpStatus_ =
  GetConfigurationResponse'
    { configurationVersion =
        Prelude.Nothing,
      content = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration version.
getConfigurationResponse_configurationVersion :: Lens.Lens' GetConfigurationResponse (Prelude.Maybe Prelude.Text)
getConfigurationResponse_configurationVersion = Lens.lens (\GetConfigurationResponse' {configurationVersion} -> configurationVersion) (\s@GetConfigurationResponse' {} a -> s {configurationVersion = a} :: GetConfigurationResponse)

-- | The content of the configuration or the configuration data.
getConfigurationResponse_content :: Lens.Lens' GetConfigurationResponse (Prelude.Maybe Prelude.ByteString)
getConfigurationResponse_content = Lens.lens (\GetConfigurationResponse' {content} -> content) (\s@GetConfigurationResponse' {} a -> s {content = a} :: GetConfigurationResponse) Prelude.. Lens.mapping Core._Sensitive

-- | A standard MIME type describing the format of the configuration content.
-- For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
getConfigurationResponse_contentType :: Lens.Lens' GetConfigurationResponse (Prelude.Maybe Prelude.Text)
getConfigurationResponse_contentType = Lens.lens (\GetConfigurationResponse' {contentType} -> contentType) (\s@GetConfigurationResponse' {} a -> s {contentType = a} :: GetConfigurationResponse)

-- | The response's http status code.
getConfigurationResponse_httpStatus :: Lens.Lens' GetConfigurationResponse Prelude.Int
getConfigurationResponse_httpStatus = Lens.lens (\GetConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetConfigurationResponse' {} a -> s {httpStatus = a} :: GetConfigurationResponse)

instance Prelude.NFData GetConfigurationResponse
