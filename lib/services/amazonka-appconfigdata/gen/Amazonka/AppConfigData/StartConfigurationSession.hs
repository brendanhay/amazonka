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
-- Module      : Amazonka.AppConfigData.StartConfigurationSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a configuration session used to retrieve a deployed
-- configuration. For more information about this API action and to view
-- example CLI commands that show how to use it with the
-- GetLatestConfiguration API action, see
-- <http://docs.aws.amazon.com/appconfig/latest/userguide/appconfig-retrieving-the-configuration Receiving the configuration>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfigData.StartConfigurationSession
  ( -- * Creating a Request
    StartConfigurationSession (..),
    newStartConfigurationSession,

    -- * Request Lenses
    startConfigurationSession_requiredMinimumPollIntervalInSeconds,
    startConfigurationSession_applicationIdentifier,
    startConfigurationSession_environmentIdentifier,
    startConfigurationSession_configurationProfileIdentifier,

    -- * Destructuring the Response
    StartConfigurationSessionResponse (..),
    newStartConfigurationSessionResponse,

    -- * Response Lenses
    startConfigurationSessionResponse_initialConfigurationToken,
    startConfigurationSessionResponse_httpStatus,
  )
where

import Amazonka.AppConfigData.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartConfigurationSession' smart constructor.
data StartConfigurationSession = StartConfigurationSession'
  { -- | Sets a constraint on a session. If you specify a value of, for example,
    -- 60 seconds, then the client that established the session can\'t call
    -- GetLatestConfiguration more frequently then every 60 seconds.
    requiredMinimumPollIntervalInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The application ID or the application name.
    applicationIdentifier :: Prelude.Text,
    -- | The environment ID or the environment name.
    environmentIdentifier :: Prelude.Text,
    -- | The configuration profile ID or the configuration profile name.
    configurationProfileIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConfigurationSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requiredMinimumPollIntervalInSeconds', 'startConfigurationSession_requiredMinimumPollIntervalInSeconds' - Sets a constraint on a session. If you specify a value of, for example,
-- 60 seconds, then the client that established the session can\'t call
-- GetLatestConfiguration more frequently then every 60 seconds.
--
-- 'applicationIdentifier', 'startConfigurationSession_applicationIdentifier' - The application ID or the application name.
--
-- 'environmentIdentifier', 'startConfigurationSession_environmentIdentifier' - The environment ID or the environment name.
--
-- 'configurationProfileIdentifier', 'startConfigurationSession_configurationProfileIdentifier' - The configuration profile ID or the configuration profile name.
newStartConfigurationSession ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  -- | 'configurationProfileIdentifier'
  Prelude.Text ->
  StartConfigurationSession
newStartConfigurationSession
  pApplicationIdentifier_
  pEnvironmentIdentifier_
  pConfigurationProfileIdentifier_ =
    StartConfigurationSession'
      { requiredMinimumPollIntervalInSeconds =
          Prelude.Nothing,
        applicationIdentifier = pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_,
        configurationProfileIdentifier =
          pConfigurationProfileIdentifier_
      }

-- | Sets a constraint on a session. If you specify a value of, for example,
-- 60 seconds, then the client that established the session can\'t call
-- GetLatestConfiguration more frequently then every 60 seconds.
startConfigurationSession_requiredMinimumPollIntervalInSeconds :: Lens.Lens' StartConfigurationSession (Prelude.Maybe Prelude.Natural)
startConfigurationSession_requiredMinimumPollIntervalInSeconds = Lens.lens (\StartConfigurationSession' {requiredMinimumPollIntervalInSeconds} -> requiredMinimumPollIntervalInSeconds) (\s@StartConfigurationSession' {} a -> s {requiredMinimumPollIntervalInSeconds = a} :: StartConfigurationSession)

-- | The application ID or the application name.
startConfigurationSession_applicationIdentifier :: Lens.Lens' StartConfigurationSession Prelude.Text
startConfigurationSession_applicationIdentifier = Lens.lens (\StartConfigurationSession' {applicationIdentifier} -> applicationIdentifier) (\s@StartConfigurationSession' {} a -> s {applicationIdentifier = a} :: StartConfigurationSession)

-- | The environment ID or the environment name.
startConfigurationSession_environmentIdentifier :: Lens.Lens' StartConfigurationSession Prelude.Text
startConfigurationSession_environmentIdentifier = Lens.lens (\StartConfigurationSession' {environmentIdentifier} -> environmentIdentifier) (\s@StartConfigurationSession' {} a -> s {environmentIdentifier = a} :: StartConfigurationSession)

-- | The configuration profile ID or the configuration profile name.
startConfigurationSession_configurationProfileIdentifier :: Lens.Lens' StartConfigurationSession Prelude.Text
startConfigurationSession_configurationProfileIdentifier = Lens.lens (\StartConfigurationSession' {configurationProfileIdentifier} -> configurationProfileIdentifier) (\s@StartConfigurationSession' {} a -> s {configurationProfileIdentifier = a} :: StartConfigurationSession)

instance Core.AWSRequest StartConfigurationSession where
  type
    AWSResponse StartConfigurationSession =
      StartConfigurationSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartConfigurationSessionResponse'
            Prelude.<$> (x Data..?> "InitialConfigurationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartConfigurationSession where
  hashWithSalt _salt StartConfigurationSession' {..} =
    _salt
      `Prelude.hashWithSalt` requiredMinimumPollIntervalInSeconds
      `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier
      `Prelude.hashWithSalt` configurationProfileIdentifier

instance Prelude.NFData StartConfigurationSession where
  rnf StartConfigurationSession' {..} =
    Prelude.rnf requiredMinimumPollIntervalInSeconds `Prelude.seq`
      Prelude.rnf applicationIdentifier `Prelude.seq`
        Prelude.rnf environmentIdentifier `Prelude.seq`
          Prelude.rnf configurationProfileIdentifier

instance Data.ToHeaders StartConfigurationSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartConfigurationSession where
  toJSON StartConfigurationSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RequiredMinimumPollIntervalInSeconds" Data..=)
              Prelude.<$> requiredMinimumPollIntervalInSeconds,
            Prelude.Just
              ( "ApplicationIdentifier"
                  Data..= applicationIdentifier
              ),
            Prelude.Just
              ( "EnvironmentIdentifier"
                  Data..= environmentIdentifier
              ),
            Prelude.Just
              ( "ConfigurationProfileIdentifier"
                  Data..= configurationProfileIdentifier
              )
          ]
      )

instance Data.ToPath StartConfigurationSession where
  toPath = Prelude.const "/configurationsessions"

instance Data.ToQuery StartConfigurationSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartConfigurationSessionResponse' smart constructor.
data StartConfigurationSessionResponse = StartConfigurationSessionResponse'
  { -- | Token encapsulating state about the configuration session. Provide this
    -- token to the @GetLatestConfiguration@ API to retrieve configuration
    -- data.
    --
    -- This token should only be used once in your first call to
    -- @GetLatestConfiguration@. You MUST use the new token in the
    -- @GetLatestConfiguration@ response (@NextPollConfigurationToken@) in each
    -- subsequent call to @GetLatestConfiguration@.
    initialConfigurationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartConfigurationSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialConfigurationToken', 'startConfigurationSessionResponse_initialConfigurationToken' - Token encapsulating state about the configuration session. Provide this
-- token to the @GetLatestConfiguration@ API to retrieve configuration
-- data.
--
-- This token should only be used once in your first call to
-- @GetLatestConfiguration@. You MUST use the new token in the
-- @GetLatestConfiguration@ response (@NextPollConfigurationToken@) in each
-- subsequent call to @GetLatestConfiguration@.
--
-- 'httpStatus', 'startConfigurationSessionResponse_httpStatus' - The response's http status code.
newStartConfigurationSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartConfigurationSessionResponse
newStartConfigurationSessionResponse pHttpStatus_ =
  StartConfigurationSessionResponse'
    { initialConfigurationToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token encapsulating state about the configuration session. Provide this
-- token to the @GetLatestConfiguration@ API to retrieve configuration
-- data.
--
-- This token should only be used once in your first call to
-- @GetLatestConfiguration@. You MUST use the new token in the
-- @GetLatestConfiguration@ response (@NextPollConfigurationToken@) in each
-- subsequent call to @GetLatestConfiguration@.
startConfigurationSessionResponse_initialConfigurationToken :: Lens.Lens' StartConfigurationSessionResponse (Prelude.Maybe Prelude.Text)
startConfigurationSessionResponse_initialConfigurationToken = Lens.lens (\StartConfigurationSessionResponse' {initialConfigurationToken} -> initialConfigurationToken) (\s@StartConfigurationSessionResponse' {} a -> s {initialConfigurationToken = a} :: StartConfigurationSessionResponse)

-- | The response's http status code.
startConfigurationSessionResponse_httpStatus :: Lens.Lens' StartConfigurationSessionResponse Prelude.Int
startConfigurationSessionResponse_httpStatus = Lens.lens (\StartConfigurationSessionResponse' {httpStatus} -> httpStatus) (\s@StartConfigurationSessionResponse' {} a -> s {httpStatus = a} :: StartConfigurationSessionResponse)

instance
  Prelude.NFData
    StartConfigurationSessionResponse
  where
  rnf StartConfigurationSessionResponse' {..} =
    Prelude.rnf initialConfigurationToken `Prelude.seq`
      Prelude.rnf httpStatus
