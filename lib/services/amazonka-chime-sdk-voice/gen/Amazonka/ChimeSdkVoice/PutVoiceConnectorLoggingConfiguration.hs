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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorLoggingConfiguration
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

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorLoggingConfiguration' smart constructor.
data PutVoiceConnectorLoggingConfiguration = PutVoiceConnectorLoggingConfiguration'
  { voiceConnectorId :: Prelude.Text,
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
-- 'voiceConnectorId', 'putVoiceConnectorLoggingConfiguration_voiceConnectorId' - Undocumented member.
--
-- 'loggingConfiguration', 'putVoiceConnectorLoggingConfiguration_loggingConfiguration' - Undocumented member.
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

-- | Undocumented member.
putVoiceConnectorLoggingConfiguration_voiceConnectorId :: Lens.Lens' PutVoiceConnectorLoggingConfiguration Prelude.Text
putVoiceConnectorLoggingConfiguration_voiceConnectorId = Lens.lens (\PutVoiceConnectorLoggingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorLoggingConfiguration' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorLoggingConfiguration)

-- | Undocumented member.
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
            Prelude.<$> (x Data..?> "LoggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorLoggingConfiguration
  where
  hashWithSalt
    _salt
    PutVoiceConnectorLoggingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` voiceConnectorId
        `Prelude.hashWithSalt` loggingConfiguration

instance
  Prelude.NFData
    PutVoiceConnectorLoggingConfiguration
  where
  rnf PutVoiceConnectorLoggingConfiguration' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf loggingConfiguration

instance
  Data.ToHeaders
    PutVoiceConnectorLoggingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutVoiceConnectorLoggingConfiguration
  where
  toJSON PutVoiceConnectorLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LoggingConfiguration"
                  Data..= loggingConfiguration
              )
          ]
      )

instance
  Data.ToPath
    PutVoiceConnectorLoggingConfiguration
  where
  toPath PutVoiceConnectorLoggingConfiguration' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/logging-configuration"
      ]

instance
  Data.ToQuery
    PutVoiceConnectorLoggingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorLoggingConfigurationResponse' smart constructor.
data PutVoiceConnectorLoggingConfigurationResponse = PutVoiceConnectorLoggingConfigurationResponse'
  { loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
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
-- 'loggingConfiguration', 'putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration' - Undocumented member.
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

-- | Undocumented member.
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
