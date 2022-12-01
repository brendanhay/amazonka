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
-- Module      : Amazonka.Chime.PutVoiceConnectorLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a logging configuration for the specified Amazon Chime Voice
-- Connector. The logging configuration specifies whether SIP message logs
-- are enabled for sending to Amazon CloudWatch Logs.
module Amazonka.Chime.PutVoiceConnectorLoggingConfiguration
  ( -- * Creating a Request
    PutVoiceConnectorLoggingConfiguration (..),
    newPutVoiceConnectorLoggingConfiguration,

    -- * Request Lenses
    putVoiceConnectorLoggingConfiguration_voiceConnectorId,
    putVoiceConnectorLoggingConfiguration_loggingConfiguration,

    -- * Destructuring the Response
    PutVoiceConnectorLoggingConfigurationResponse (..),
    newPutVoiceConnectorLoggingConfigurationResponse,

    -- * Response Lenses
    putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorLoggingConfiguration' smart constructor.
data PutVoiceConnectorLoggingConfiguration = PutVoiceConnectorLoggingConfiguration'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The logging configuration details to add.
    loggingConfiguration :: LoggingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'putVoiceConnectorLoggingConfiguration_voiceConnectorId' - The Amazon Chime Voice Connector ID.
--
-- 'loggingConfiguration', 'putVoiceConnectorLoggingConfiguration_loggingConfiguration' - The logging configuration details to add.
newPutVoiceConnectorLoggingConfiguration ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'loggingConfiguration'
  LoggingConfiguration ->
  PutVoiceConnectorLoggingConfiguration
newPutVoiceConnectorLoggingConfiguration
  pVoiceConnectorId_
  pLoggingConfiguration_ =
    PutVoiceConnectorLoggingConfiguration'
      { voiceConnectorId =
          pVoiceConnectorId_,
        loggingConfiguration =
          pLoggingConfiguration_
      }

-- | The Amazon Chime Voice Connector ID.
putVoiceConnectorLoggingConfiguration_voiceConnectorId :: Lens.Lens' PutVoiceConnectorLoggingConfiguration Prelude.Text
putVoiceConnectorLoggingConfiguration_voiceConnectorId = Lens.lens (\PutVoiceConnectorLoggingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorLoggingConfiguration' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorLoggingConfiguration)

-- | The logging configuration details to add.
putVoiceConnectorLoggingConfiguration_loggingConfiguration :: Lens.Lens' PutVoiceConnectorLoggingConfiguration LoggingConfiguration
putVoiceConnectorLoggingConfiguration_loggingConfiguration = Lens.lens (\PutVoiceConnectorLoggingConfiguration' {loggingConfiguration} -> loggingConfiguration) (\s@PutVoiceConnectorLoggingConfiguration' {} a -> s {loggingConfiguration = a} :: PutVoiceConnectorLoggingConfiguration)

instance
  Core.AWSRequest
    PutVoiceConnectorLoggingConfiguration
  where
  type
    AWSResponse
      PutVoiceConnectorLoggingConfiguration =
      PutVoiceConnectorLoggingConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorLoggingConfigurationResponse'
            Prelude.<$> (x Core..?> "LoggingConfiguration")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorLoggingConfiguration
  where
  hashWithSalt
    _salt
    PutVoiceConnectorLoggingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId
        `Prelude.hashWithSalt` loggingConfiguration

instance
  Prelude.NFData
    PutVoiceConnectorLoggingConfiguration
  where
  rnf PutVoiceConnectorLoggingConfiguration' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf loggingConfiguration

instance
  Core.ToHeaders
    PutVoiceConnectorLoggingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    PutVoiceConnectorLoggingConfiguration
  where
  toJSON PutVoiceConnectorLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LoggingConfiguration"
                  Core..= loggingConfiguration
              )
          ]
      )

instance
  Core.ToPath
    PutVoiceConnectorLoggingConfiguration
  where
  toPath PutVoiceConnectorLoggingConfiguration' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/logging-configuration"
      ]

instance
  Core.ToQuery
    PutVoiceConnectorLoggingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorLoggingConfigurationResponse' smart constructor.
data PutVoiceConnectorLoggingConfigurationResponse = PutVoiceConnectorLoggingConfigurationResponse'
  { -- | The updated logging configuration details.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration' - The updated logging configuration details.
--
-- 'httpStatus', 'putVoiceConnectorLoggingConfigurationResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorLoggingConfigurationResponse
newPutVoiceConnectorLoggingConfigurationResponse
  pHttpStatus_ =
    PutVoiceConnectorLoggingConfigurationResponse'
      { loggingConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated logging configuration details.
putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' PutVoiceConnectorLoggingConfigurationResponse (Prelude.Maybe LoggingConfiguration)
putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\PutVoiceConnectorLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@PutVoiceConnectorLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: PutVoiceConnectorLoggingConfigurationResponse)

-- | The response's http status code.
putVoiceConnectorLoggingConfigurationResponse_httpStatus :: Lens.Lens' PutVoiceConnectorLoggingConfigurationResponse Prelude.Int
putVoiceConnectorLoggingConfigurationResponse_httpStatus = Lens.lens (\PutVoiceConnectorLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorLoggingConfigurationResponse)

instance
  Prelude.NFData
    PutVoiceConnectorLoggingConfigurationResponse
  where
  rnf
    PutVoiceConnectorLoggingConfigurationResponse' {..} =
      Prelude.rnf loggingConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
