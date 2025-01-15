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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorEmergencyCallingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorEmergencyCallingConfiguration
  ( -- * Creating a Request
    PutVoiceConnectorEmergencyCallingConfiguration (..),
    newPutVoiceConnectorEmergencyCallingConfiguration,

    -- * Request Lenses
    putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration,

    -- * Destructuring the Response
    PutVoiceConnectorEmergencyCallingConfigurationResponse (..),
    newPutVoiceConnectorEmergencyCallingConfigurationResponse,

    -- * Response Lenses
    putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorEmergencyCallingConfiguration' smart constructor.
data PutVoiceConnectorEmergencyCallingConfiguration = PutVoiceConnectorEmergencyCallingConfiguration'
  { voiceConnectorId :: Prelude.Text,
    emergencyCallingConfiguration :: EmergencyCallingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorEmergencyCallingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId' - Undocumented member.
--
-- 'emergencyCallingConfiguration', 'putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration' - Undocumented member.
newPutVoiceConnectorEmergencyCallingConfiguration ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'emergencyCallingConfiguration'
  EmergencyCallingConfiguration ->
  PutVoiceConnectorEmergencyCallingConfiguration
newPutVoiceConnectorEmergencyCallingConfiguration
  pVoiceConnectorId_
  pEmergencyCallingConfiguration_ =
    PutVoiceConnectorEmergencyCallingConfiguration'
      { voiceConnectorId =
          pVoiceConnectorId_,
        emergencyCallingConfiguration =
          pEmergencyCallingConfiguration_
      }

-- | Undocumented member.
putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId :: Lens.Lens' PutVoiceConnectorEmergencyCallingConfiguration Prelude.Text
putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId = Lens.lens (\PutVoiceConnectorEmergencyCallingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorEmergencyCallingConfiguration' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorEmergencyCallingConfiguration)

-- | Undocumented member.
putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration :: Lens.Lens' PutVoiceConnectorEmergencyCallingConfiguration EmergencyCallingConfiguration
putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration = Lens.lens (\PutVoiceConnectorEmergencyCallingConfiguration' {emergencyCallingConfiguration} -> emergencyCallingConfiguration) (\s@PutVoiceConnectorEmergencyCallingConfiguration' {} a -> s {emergencyCallingConfiguration = a} :: PutVoiceConnectorEmergencyCallingConfiguration)

instance
  Core.AWSRequest
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  type
    AWSResponse
      PutVoiceConnectorEmergencyCallingConfiguration =
      PutVoiceConnectorEmergencyCallingConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorEmergencyCallingConfigurationResponse'
            Prelude.<$> (x Data..?> "EmergencyCallingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  hashWithSalt
    _salt
    PutVoiceConnectorEmergencyCallingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` voiceConnectorId
        `Prelude.hashWithSalt` emergencyCallingConfiguration

instance
  Prelude.NFData
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  rnf
    PutVoiceConnectorEmergencyCallingConfiguration' {..} =
      Prelude.rnf voiceConnectorId `Prelude.seq`
        Prelude.rnf emergencyCallingConfiguration

instance
  Data.ToHeaders
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  toJSON
    PutVoiceConnectorEmergencyCallingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "EmergencyCallingConfiguration"
                    Data..= emergencyCallingConfiguration
                )
            ]
        )

instance
  Data.ToPath
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  toPath
    PutVoiceConnectorEmergencyCallingConfiguration' {..} =
      Prelude.mconcat
        [ "/voice-connectors/",
          Data.toBS voiceConnectorId,
          "/emergency-calling-configuration"
        ]

instance
  Data.ToQuery
    PutVoiceConnectorEmergencyCallingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorEmergencyCallingConfigurationResponse' smart constructor.
data PutVoiceConnectorEmergencyCallingConfigurationResponse = PutVoiceConnectorEmergencyCallingConfigurationResponse'
  { emergencyCallingConfiguration :: Prelude.Maybe EmergencyCallingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorEmergencyCallingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emergencyCallingConfiguration', 'putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration' - Undocumented member.
--
-- 'httpStatus', 'putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorEmergencyCallingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorEmergencyCallingConfigurationResponse
newPutVoiceConnectorEmergencyCallingConfigurationResponse
  pHttpStatus_ =
    PutVoiceConnectorEmergencyCallingConfigurationResponse'
      { emergencyCallingConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration :: Lens.Lens' PutVoiceConnectorEmergencyCallingConfigurationResponse (Prelude.Maybe EmergencyCallingConfiguration)
putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration = Lens.lens (\PutVoiceConnectorEmergencyCallingConfigurationResponse' {emergencyCallingConfiguration} -> emergencyCallingConfiguration) (\s@PutVoiceConnectorEmergencyCallingConfigurationResponse' {} a -> s {emergencyCallingConfiguration = a} :: PutVoiceConnectorEmergencyCallingConfigurationResponse)

-- | The response's http status code.
putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus :: Lens.Lens' PutVoiceConnectorEmergencyCallingConfigurationResponse Prelude.Int
putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus = Lens.lens (\PutVoiceConnectorEmergencyCallingConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorEmergencyCallingConfigurationResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorEmergencyCallingConfigurationResponse)

instance
  Prelude.NFData
    PutVoiceConnectorEmergencyCallingConfigurationResponse
  where
  rnf
    PutVoiceConnectorEmergencyCallingConfigurationResponse' {..} =
      Prelude.rnf emergencyCallingConfiguration `Prelude.seq`
        Prelude.rnf httpStatus
