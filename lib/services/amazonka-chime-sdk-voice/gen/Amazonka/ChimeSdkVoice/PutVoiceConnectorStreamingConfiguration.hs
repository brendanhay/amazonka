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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorStreamingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Voice Connector\'s streaming configuration settings.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorStreamingConfiguration
  ( -- * Creating a Request
    PutVoiceConnectorStreamingConfiguration (..),
    newPutVoiceConnectorStreamingConfiguration,

    -- * Request Lenses
    putVoiceConnectorStreamingConfiguration_voiceConnectorId,
    putVoiceConnectorStreamingConfiguration_streamingConfiguration,

    -- * Destructuring the Response
    PutVoiceConnectorStreamingConfigurationResponse (..),
    newPutVoiceConnectorStreamingConfigurationResponse,

    -- * Response Lenses
    putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorStreamingConfiguration' smart constructor.
data PutVoiceConnectorStreamingConfiguration = PutVoiceConnectorStreamingConfiguration'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The streaming settings being updated.
    streamingConfiguration :: StreamingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorStreamingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'putVoiceConnectorStreamingConfiguration_voiceConnectorId' - The Voice Connector ID.
--
-- 'streamingConfiguration', 'putVoiceConnectorStreamingConfiguration_streamingConfiguration' - The streaming settings being updated.
newPutVoiceConnectorStreamingConfiguration ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'streamingConfiguration'
  StreamingConfiguration ->
  PutVoiceConnectorStreamingConfiguration
newPutVoiceConnectorStreamingConfiguration
  pVoiceConnectorId_
  pStreamingConfiguration_ =
    PutVoiceConnectorStreamingConfiguration'
      { voiceConnectorId =
          pVoiceConnectorId_,
        streamingConfiguration =
          pStreamingConfiguration_
      }

-- | The Voice Connector ID.
putVoiceConnectorStreamingConfiguration_voiceConnectorId :: Lens.Lens' PutVoiceConnectorStreamingConfiguration Prelude.Text
putVoiceConnectorStreamingConfiguration_voiceConnectorId = Lens.lens (\PutVoiceConnectorStreamingConfiguration' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorStreamingConfiguration' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorStreamingConfiguration)

-- | The streaming settings being updated.
putVoiceConnectorStreamingConfiguration_streamingConfiguration :: Lens.Lens' PutVoiceConnectorStreamingConfiguration StreamingConfiguration
putVoiceConnectorStreamingConfiguration_streamingConfiguration = Lens.lens (\PutVoiceConnectorStreamingConfiguration' {streamingConfiguration} -> streamingConfiguration) (\s@PutVoiceConnectorStreamingConfiguration' {} a -> s {streamingConfiguration = a} :: PutVoiceConnectorStreamingConfiguration)

instance
  Core.AWSRequest
    PutVoiceConnectorStreamingConfiguration
  where
  type
    AWSResponse
      PutVoiceConnectorStreamingConfiguration =
      PutVoiceConnectorStreamingConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorStreamingConfigurationResponse'
            Prelude.<$> (x Data..?> "StreamingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorStreamingConfiguration
  where
  hashWithSalt
    _salt
    PutVoiceConnectorStreamingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` voiceConnectorId
        `Prelude.hashWithSalt` streamingConfiguration

instance
  Prelude.NFData
    PutVoiceConnectorStreamingConfiguration
  where
  rnf PutVoiceConnectorStreamingConfiguration' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf streamingConfiguration

instance
  Data.ToHeaders
    PutVoiceConnectorStreamingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutVoiceConnectorStreamingConfiguration
  where
  toJSON PutVoiceConnectorStreamingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StreamingConfiguration"
                  Data..= streamingConfiguration
              )
          ]
      )

instance
  Data.ToPath
    PutVoiceConnectorStreamingConfiguration
  where
  toPath PutVoiceConnectorStreamingConfiguration' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/streaming-configuration"
      ]

instance
  Data.ToQuery
    PutVoiceConnectorStreamingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorStreamingConfigurationResponse' smart constructor.
data PutVoiceConnectorStreamingConfigurationResponse = PutVoiceConnectorStreamingConfigurationResponse'
  { -- | The updated streaming settings.
    streamingConfiguration :: Prelude.Maybe StreamingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorStreamingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingConfiguration', 'putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration' - The updated streaming settings.
--
-- 'httpStatus', 'putVoiceConnectorStreamingConfigurationResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorStreamingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorStreamingConfigurationResponse
newPutVoiceConnectorStreamingConfigurationResponse
  pHttpStatus_ =
    PutVoiceConnectorStreamingConfigurationResponse'
      { streamingConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated streaming settings.
putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration :: Lens.Lens' PutVoiceConnectorStreamingConfigurationResponse (Prelude.Maybe StreamingConfiguration)
putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration = Lens.lens (\PutVoiceConnectorStreamingConfigurationResponse' {streamingConfiguration} -> streamingConfiguration) (\s@PutVoiceConnectorStreamingConfigurationResponse' {} a -> s {streamingConfiguration = a} :: PutVoiceConnectorStreamingConfigurationResponse)

-- | The response's http status code.
putVoiceConnectorStreamingConfigurationResponse_httpStatus :: Lens.Lens' PutVoiceConnectorStreamingConfigurationResponse Prelude.Int
putVoiceConnectorStreamingConfigurationResponse_httpStatus = Lens.lens (\PutVoiceConnectorStreamingConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorStreamingConfigurationResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorStreamingConfigurationResponse)

instance
  Prelude.NFData
    PutVoiceConnectorStreamingConfigurationResponse
  where
  rnf
    PutVoiceConnectorStreamingConfigurationResponse' {..} =
      Prelude.rnf streamingConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
