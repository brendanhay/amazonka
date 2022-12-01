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
-- Module      : Amazonka.Chime.GetVoiceConnectorTerminationHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the last time a SIP @OPTIONS@ ping was
-- received from your SIP infrastructure for the specified Amazon Chime
-- Voice Connector.
module Amazonka.Chime.GetVoiceConnectorTerminationHealth
  ( -- * Creating a Request
    GetVoiceConnectorTerminationHealth (..),
    newGetVoiceConnectorTerminationHealth,

    -- * Request Lenses
    getVoiceConnectorTerminationHealth_voiceConnectorId,

    -- * Destructuring the Response
    GetVoiceConnectorTerminationHealthResponse (..),
    newGetVoiceConnectorTerminationHealthResponse,

    -- * Response Lenses
    getVoiceConnectorTerminationHealthResponse_terminationHealth,
    getVoiceConnectorTerminationHealthResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceConnectorTerminationHealth' smart constructor.
data GetVoiceConnectorTerminationHealth = GetVoiceConnectorTerminationHealth'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorTerminationHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getVoiceConnectorTerminationHealth_voiceConnectorId' - The Amazon Chime Voice Connector ID.
newGetVoiceConnectorTerminationHealth ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  GetVoiceConnectorTerminationHealth
newGetVoiceConnectorTerminationHealth
  pVoiceConnectorId_ =
    GetVoiceConnectorTerminationHealth'
      { voiceConnectorId =
          pVoiceConnectorId_
      }

-- | The Amazon Chime Voice Connector ID.
getVoiceConnectorTerminationHealth_voiceConnectorId :: Lens.Lens' GetVoiceConnectorTerminationHealth Prelude.Text
getVoiceConnectorTerminationHealth_voiceConnectorId = Lens.lens (\GetVoiceConnectorTerminationHealth' {voiceConnectorId} -> voiceConnectorId) (\s@GetVoiceConnectorTerminationHealth' {} a -> s {voiceConnectorId = a} :: GetVoiceConnectorTerminationHealth)

instance
  Core.AWSRequest
    GetVoiceConnectorTerminationHealth
  where
  type
    AWSResponse GetVoiceConnectorTerminationHealth =
      GetVoiceConnectorTerminationHealthResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceConnectorTerminationHealthResponse'
            Prelude.<$> (x Core..?> "TerminationHealth")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVoiceConnectorTerminationHealth
  where
  hashWithSalt
    _salt
    GetVoiceConnectorTerminationHealth' {..} =
      _salt `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    GetVoiceConnectorTerminationHealth
  where
  rnf GetVoiceConnectorTerminationHealth' {..} =
    Prelude.rnf voiceConnectorId

instance
  Core.ToHeaders
    GetVoiceConnectorTerminationHealth
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetVoiceConnectorTerminationHealth
  where
  toPath GetVoiceConnectorTerminationHealth' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/termination/health"
      ]

instance
  Core.ToQuery
    GetVoiceConnectorTerminationHealth
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceConnectorTerminationHealthResponse' smart constructor.
data GetVoiceConnectorTerminationHealthResponse = GetVoiceConnectorTerminationHealthResponse'
  { -- | The termination health details.
    terminationHealth :: Prelude.Maybe TerminationHealth,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorTerminationHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminationHealth', 'getVoiceConnectorTerminationHealthResponse_terminationHealth' - The termination health details.
--
-- 'httpStatus', 'getVoiceConnectorTerminationHealthResponse_httpStatus' - The response's http status code.
newGetVoiceConnectorTerminationHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceConnectorTerminationHealthResponse
newGetVoiceConnectorTerminationHealthResponse
  pHttpStatus_ =
    GetVoiceConnectorTerminationHealthResponse'
      { terminationHealth =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The termination health details.
getVoiceConnectorTerminationHealthResponse_terminationHealth :: Lens.Lens' GetVoiceConnectorTerminationHealthResponse (Prelude.Maybe TerminationHealth)
getVoiceConnectorTerminationHealthResponse_terminationHealth = Lens.lens (\GetVoiceConnectorTerminationHealthResponse' {terminationHealth} -> terminationHealth) (\s@GetVoiceConnectorTerminationHealthResponse' {} a -> s {terminationHealth = a} :: GetVoiceConnectorTerminationHealthResponse)

-- | The response's http status code.
getVoiceConnectorTerminationHealthResponse_httpStatus :: Lens.Lens' GetVoiceConnectorTerminationHealthResponse Prelude.Int
getVoiceConnectorTerminationHealthResponse_httpStatus = Lens.lens (\GetVoiceConnectorTerminationHealthResponse' {httpStatus} -> httpStatus) (\s@GetVoiceConnectorTerminationHealthResponse' {} a -> s {httpStatus = a} :: GetVoiceConnectorTerminationHealthResponse)

instance
  Prelude.NFData
    GetVoiceConnectorTerminationHealthResponse
  where
  rnf GetVoiceConnectorTerminationHealthResponse' {..} =
    Prelude.rnf terminationHealth
      `Prelude.seq` Prelude.rnf httpStatus
