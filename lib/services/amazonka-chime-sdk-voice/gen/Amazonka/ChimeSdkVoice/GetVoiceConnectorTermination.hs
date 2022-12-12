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
-- Module      : Amazonka.ChimeSdkVoice.GetVoiceConnectorTermination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetVoiceConnectorTermination
  ( -- * Creating a Request
    GetVoiceConnectorTermination (..),
    newGetVoiceConnectorTermination,

    -- * Request Lenses
    getVoiceConnectorTermination_voiceConnectorId,

    -- * Destructuring the Response
    GetVoiceConnectorTerminationResponse (..),
    newGetVoiceConnectorTerminationResponse,

    -- * Response Lenses
    getVoiceConnectorTerminationResponse_termination,
    getVoiceConnectorTerminationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceConnectorTermination' smart constructor.
data GetVoiceConnectorTermination = GetVoiceConnectorTermination'
  { voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorTermination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getVoiceConnectorTermination_voiceConnectorId' - Undocumented member.
newGetVoiceConnectorTermination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  GetVoiceConnectorTermination
newGetVoiceConnectorTermination pVoiceConnectorId_ =
  GetVoiceConnectorTermination'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | Undocumented member.
getVoiceConnectorTermination_voiceConnectorId :: Lens.Lens' GetVoiceConnectorTermination Prelude.Text
getVoiceConnectorTermination_voiceConnectorId = Lens.lens (\GetVoiceConnectorTermination' {voiceConnectorId} -> voiceConnectorId) (\s@GetVoiceConnectorTermination' {} a -> s {voiceConnectorId = a} :: GetVoiceConnectorTermination)

instance Core.AWSRequest GetVoiceConnectorTermination where
  type
    AWSResponse GetVoiceConnectorTermination =
      GetVoiceConnectorTerminationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceConnectorTerminationResponse'
            Prelude.<$> (x Data..?> "Termination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVoiceConnectorTermination
  where
  hashWithSalt _salt GetVoiceConnectorTermination' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData GetVoiceConnectorTermination where
  rnf GetVoiceConnectorTermination' {..} =
    Prelude.rnf voiceConnectorId

instance Data.ToHeaders GetVoiceConnectorTermination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVoiceConnectorTermination where
  toPath GetVoiceConnectorTermination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/termination"
      ]

instance Data.ToQuery GetVoiceConnectorTermination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceConnectorTerminationResponse' smart constructor.
data GetVoiceConnectorTerminationResponse = GetVoiceConnectorTerminationResponse'
  { termination :: Prelude.Maybe Termination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorTerminationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'termination', 'getVoiceConnectorTerminationResponse_termination' - Undocumented member.
--
-- 'httpStatus', 'getVoiceConnectorTerminationResponse_httpStatus' - The response's http status code.
newGetVoiceConnectorTerminationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceConnectorTerminationResponse
newGetVoiceConnectorTerminationResponse pHttpStatus_ =
  GetVoiceConnectorTerminationResponse'
    { termination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getVoiceConnectorTerminationResponse_termination :: Lens.Lens' GetVoiceConnectorTerminationResponse (Prelude.Maybe Termination)
getVoiceConnectorTerminationResponse_termination = Lens.lens (\GetVoiceConnectorTerminationResponse' {termination} -> termination) (\s@GetVoiceConnectorTerminationResponse' {} a -> s {termination = a} :: GetVoiceConnectorTerminationResponse)

-- | The response's http status code.
getVoiceConnectorTerminationResponse_httpStatus :: Lens.Lens' GetVoiceConnectorTerminationResponse Prelude.Int
getVoiceConnectorTerminationResponse_httpStatus = Lens.lens (\GetVoiceConnectorTerminationResponse' {httpStatus} -> httpStatus) (\s@GetVoiceConnectorTerminationResponse' {} a -> s {httpStatus = a} :: GetVoiceConnectorTerminationResponse)

instance
  Prelude.NFData
    GetVoiceConnectorTerminationResponse
  where
  rnf GetVoiceConnectorTerminationResponse' {..} =
    Prelude.rnf termination
      `Prelude.seq` Prelude.rnf httpStatus
