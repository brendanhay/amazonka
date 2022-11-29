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
-- Module      : Amazonka.AppConfigData.GetLatestConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the latest deployed configuration. This API may return empty
-- configuration data if the client already has the latest version. For
-- more information about this API action and to view example CLI commands
-- that show how to use it with the StartConfigurationSession API action,
-- see
-- <http://docs.aws.amazon.com/appconfig/latest/userguide/appconfig-retrieving-the-configuration Receiving the configuration>
-- in the /AppConfig User Guide/.
--
-- Note the following important information.
--
-- -   Each configuration token is only valid for one call to
--     @GetLatestConfiguration@. The @GetLatestConfiguration@ response
--     includes a @NextPollConfigurationToken@ that should always replace
--     the token used for the just-completed call in preparation for the
--     next one.
--
-- -   @GetLatestConfiguration@ is a priced call. For more information, see
--     <https://aws.amazon.com/systems-manager/pricing/ Pricing>.
module Amazonka.AppConfigData.GetLatestConfiguration
  ( -- * Creating a Request
    GetLatestConfiguration (..),
    newGetLatestConfiguration,

    -- * Request Lenses
    getLatestConfiguration_configurationToken,

    -- * Destructuring the Response
    GetLatestConfigurationResponse (..),
    newGetLatestConfigurationResponse,

    -- * Response Lenses
    getLatestConfigurationResponse_configuration,
    getLatestConfigurationResponse_nextPollConfigurationToken,
    getLatestConfigurationResponse_nextPollIntervalInSeconds,
    getLatestConfigurationResponse_contentType,
    getLatestConfigurationResponse_httpStatus,
  )
where

import Amazonka.AppConfigData.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLatestConfiguration' smart constructor.
data GetLatestConfiguration = GetLatestConfiguration'
  { -- | Token describing the current state of the configuration session. To
    -- obtain a token, first call the StartConfigurationSession API. Note that
    -- every call to @GetLatestConfiguration@ will return a new
    -- @ConfigurationToken@ (@NextPollConfigurationToken@ in the response) and
    -- MUST be provided to subsequent @GetLatestConfiguration@ API calls.
    configurationToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLatestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationToken', 'getLatestConfiguration_configurationToken' - Token describing the current state of the configuration session. To
-- obtain a token, first call the StartConfigurationSession API. Note that
-- every call to @GetLatestConfiguration@ will return a new
-- @ConfigurationToken@ (@NextPollConfigurationToken@ in the response) and
-- MUST be provided to subsequent @GetLatestConfiguration@ API calls.
newGetLatestConfiguration ::
  -- | 'configurationToken'
  Prelude.Text ->
  GetLatestConfiguration
newGetLatestConfiguration pConfigurationToken_ =
  GetLatestConfiguration'
    { configurationToken =
        pConfigurationToken_
    }

-- | Token describing the current state of the configuration session. To
-- obtain a token, first call the StartConfigurationSession API. Note that
-- every call to @GetLatestConfiguration@ will return a new
-- @ConfigurationToken@ (@NextPollConfigurationToken@ in the response) and
-- MUST be provided to subsequent @GetLatestConfiguration@ API calls.
getLatestConfiguration_configurationToken :: Lens.Lens' GetLatestConfiguration Prelude.Text
getLatestConfiguration_configurationToken = Lens.lens (\GetLatestConfiguration' {configurationToken} -> configurationToken) (\s@GetLatestConfiguration' {} a -> s {configurationToken = a} :: GetLatestConfiguration)

instance Core.AWSRequest GetLatestConfiguration where
  type
    AWSResponse GetLatestConfiguration =
      GetLatestConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetLatestConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Core..#? "Next-Poll-Configuration-Token")
            Prelude.<*> (h Core..#? "Next-Poll-Interval-In-Seconds")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLatestConfiguration where
  hashWithSalt _salt GetLatestConfiguration' {..} =
    _salt `Prelude.hashWithSalt` configurationToken

instance Prelude.NFData GetLatestConfiguration where
  rnf GetLatestConfiguration' {..} =
    Prelude.rnf configurationToken

instance Core.ToHeaders GetLatestConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLatestConfiguration where
  toPath = Prelude.const "/configuration"

instance Core.ToQuery GetLatestConfiguration where
  toQuery GetLatestConfiguration' {..} =
    Prelude.mconcat
      ["configuration_token" Core.=: configurationToken]

-- | /See:/ 'newGetLatestConfigurationResponse' smart constructor.
data GetLatestConfigurationResponse = GetLatestConfigurationResponse'
  { -- | The data of the configuration. This may be empty if the client already
    -- has the latest version of configuration.
    configuration :: Prelude.Maybe (Core.Sensitive Prelude.ByteString),
    -- | The latest token describing the current state of the configuration
    -- session. This MUST be provided to the next call to
    -- @GetLatestConfiguration.@
    nextPollConfigurationToken :: Prelude.Maybe Prelude.Text,
    -- | The amount of time the client should wait before polling for
    -- configuration updates again. Use @RequiredMinimumPollIntervalInSeconds@
    -- to set the desired poll interval.
    nextPollIntervalInSeconds :: Prelude.Maybe Prelude.Int,
    -- | A standard MIME type describing the format of the configuration content.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLatestConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'getLatestConfigurationResponse_configuration' - The data of the configuration. This may be empty if the client already
-- has the latest version of configuration.
--
-- 'nextPollConfigurationToken', 'getLatestConfigurationResponse_nextPollConfigurationToken' - The latest token describing the current state of the configuration
-- session. This MUST be provided to the next call to
-- @GetLatestConfiguration.@
--
-- 'nextPollIntervalInSeconds', 'getLatestConfigurationResponse_nextPollIntervalInSeconds' - The amount of time the client should wait before polling for
-- configuration updates again. Use @RequiredMinimumPollIntervalInSeconds@
-- to set the desired poll interval.
--
-- 'contentType', 'getLatestConfigurationResponse_contentType' - A standard MIME type describing the format of the configuration content.
--
-- 'httpStatus', 'getLatestConfigurationResponse_httpStatus' - The response's http status code.
newGetLatestConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLatestConfigurationResponse
newGetLatestConfigurationResponse pHttpStatus_ =
  GetLatestConfigurationResponse'
    { configuration =
        Prelude.Nothing,
      nextPollConfigurationToken =
        Prelude.Nothing,
      nextPollIntervalInSeconds = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data of the configuration. This may be empty if the client already
-- has the latest version of configuration.
getLatestConfigurationResponse_configuration :: Lens.Lens' GetLatestConfigurationResponse (Prelude.Maybe Prelude.ByteString)
getLatestConfigurationResponse_configuration = Lens.lens (\GetLatestConfigurationResponse' {configuration} -> configuration) (\s@GetLatestConfigurationResponse' {} a -> s {configuration = a} :: GetLatestConfigurationResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The latest token describing the current state of the configuration
-- session. This MUST be provided to the next call to
-- @GetLatestConfiguration.@
getLatestConfigurationResponse_nextPollConfigurationToken :: Lens.Lens' GetLatestConfigurationResponse (Prelude.Maybe Prelude.Text)
getLatestConfigurationResponse_nextPollConfigurationToken = Lens.lens (\GetLatestConfigurationResponse' {nextPollConfigurationToken} -> nextPollConfigurationToken) (\s@GetLatestConfigurationResponse' {} a -> s {nextPollConfigurationToken = a} :: GetLatestConfigurationResponse)

-- | The amount of time the client should wait before polling for
-- configuration updates again. Use @RequiredMinimumPollIntervalInSeconds@
-- to set the desired poll interval.
getLatestConfigurationResponse_nextPollIntervalInSeconds :: Lens.Lens' GetLatestConfigurationResponse (Prelude.Maybe Prelude.Int)
getLatestConfigurationResponse_nextPollIntervalInSeconds = Lens.lens (\GetLatestConfigurationResponse' {nextPollIntervalInSeconds} -> nextPollIntervalInSeconds) (\s@GetLatestConfigurationResponse' {} a -> s {nextPollIntervalInSeconds = a} :: GetLatestConfigurationResponse)

-- | A standard MIME type describing the format of the configuration content.
getLatestConfigurationResponse_contentType :: Lens.Lens' GetLatestConfigurationResponse (Prelude.Maybe Prelude.Text)
getLatestConfigurationResponse_contentType = Lens.lens (\GetLatestConfigurationResponse' {contentType} -> contentType) (\s@GetLatestConfigurationResponse' {} a -> s {contentType = a} :: GetLatestConfigurationResponse)

-- | The response's http status code.
getLatestConfigurationResponse_httpStatus :: Lens.Lens' GetLatestConfigurationResponse Prelude.Int
getLatestConfigurationResponse_httpStatus = Lens.lens (\GetLatestConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetLatestConfigurationResponse' {} a -> s {httpStatus = a} :: GetLatestConfigurationResponse)

instance
  Prelude.NFData
    GetLatestConfigurationResponse
  where
  rnf GetLatestConfigurationResponse' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf nextPollConfigurationToken
      `Prelude.seq` Prelude.rnf nextPollIntervalInSeconds
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf httpStatus
