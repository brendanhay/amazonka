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
-- Module      : Amazonka.Chime.GetVoiceConnectorLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the logging configuration details for the specified Amazon
-- Chime Voice Connector. Shows whether SIP message logs are enabled for
-- sending to Amazon CloudWatch Logs.
module Amazonka.Chime.GetVoiceConnectorLoggingConfiguration
  ( -- * Creating a Request
    GetVoiceConnectorLoggingConfiguration (..),
    newGetVoiceConnectorLoggingConfiguration,

    -- * Request Lenses
    getVoiceConnectorLoggingConfiguration_voiceConnectorId,

    -- * Destructuring the Response
    GetVoiceConnectorLoggingConfigurationResponse (..),
    newGetVoiceConnectorLoggingConfigurationResponse,

    -- * Response Lenses
    getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    getVoiceConnectorLoggingConfigurationResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceConnectorLoggingConfiguration' smart constructor.
data GetVoiceConnectorLoggingConfiguration = GetVoiceConnectorLoggingConfiguration'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getVoiceConnectorLoggingConfiguration_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newGetVoiceConnectorLoggingConfiguration ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  GetVoiceConnectorLoggingConfiguration
newGetVoiceConnectorLoggingConfiguration
  pVoiceConnectorId_ =
    GetVoiceConnectorLoggingConfiguration'
      { voiceConnectorId =
          pVoiceConnectorId_
      }

-- | The Amazon Chime Voice Connector ID.
getVoiceConnectorLoggingConfiguration_voiceConnectorId :: Lens.Lens' GetVoiceConnectorLoggingConfiguration Prelude.Text
getVoiceConnectorLoggingConfiguration_voiceConnectorId = Lens.lens (\GetVoiceConnectorLoggingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@GetVoiceConnectorLoggingConfiguration' {} a -> s {voiceConnectorId = a} :: GetVoiceConnectorLoggingConfiguration)

instance
  Core.AWSRequest
    GetVoiceConnectorLoggingConfiguration
  where
  type
    AWSResponse
      GetVoiceConnectorLoggingConfiguration =
      GetVoiceConnectorLoggingConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceConnectorLoggingConfigurationResponse'
            Prelude.<$> (x Data..?> "LoggingConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVoiceConnectorLoggingConfiguration
  where
  hashWithSalt
    _salt
    GetVoiceConnectorLoggingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    GetVoiceConnectorLoggingConfiguration
  where
  rnf GetVoiceConnectorLoggingConfiguration' {..} =
    Prelude.rnf voiceConnectorId

instance
  Data.ToHeaders
    GetVoiceConnectorLoggingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetVoiceConnectorLoggingConfiguration
  where
  toPath GetVoiceConnectorLoggingConfiguration' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/logging-configuration"
      ]

instance
  Data.ToQuery
    GetVoiceConnectorLoggingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceConnectorLoggingConfigurationResponse' smart constructor.
data GetVoiceConnectorLoggingConfigurationResponse = GetVoiceConnectorLoggingConfigurationResponse'
  { -- | The logging configuration details.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration' - The logging configuration details.
--
-- 'httpStatus', 'getVoiceConnectorLoggingConfigurationResponse_httpStatus' - The response's http status code.
newGetVoiceConnectorLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceConnectorLoggingConfigurationResponse
newGetVoiceConnectorLoggingConfigurationResponse
  pHttpStatus_ =
    GetVoiceConnectorLoggingConfigurationResponse'
      { loggingConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The logging configuration details.
getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' GetVoiceConnectorLoggingConfigurationResponse (Prelude.Maybe LoggingConfiguration)
getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\GetVoiceConnectorLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@GetVoiceConnectorLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: GetVoiceConnectorLoggingConfigurationResponse)

-- | The response's http status code.
getVoiceConnectorLoggingConfigurationResponse_httpStatus :: Lens.Lens' GetVoiceConnectorLoggingConfigurationResponse Prelude.Int
getVoiceConnectorLoggingConfigurationResponse_httpStatus = Lens.lens (\GetVoiceConnectorLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetVoiceConnectorLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: GetVoiceConnectorLoggingConfigurationResponse)

instance
  Prelude.NFData
    GetVoiceConnectorLoggingConfigurationResponse
  where
  rnf
    GetVoiceConnectorLoggingConfigurationResponse' {..} =
      Prelude.rnf loggingConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
