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
-- Module      : Amazonka.ChimeSdkVoice.GetVoiceConnectorOrigination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetVoiceConnectorOrigination
  ( -- * Creating a Request
    GetVoiceConnectorOrigination (..),
    newGetVoiceConnectorOrigination,

    -- * Request Lenses
    getVoiceConnectorOrigination_voiceConnectorId,

    -- * Destructuring the Response
    GetVoiceConnectorOriginationResponse (..),
    newGetVoiceConnectorOriginationResponse,

    -- * Response Lenses
    getVoiceConnectorOriginationResponse_origination,
    getVoiceConnectorOriginationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceConnectorOrigination' smart constructor.
data GetVoiceConnectorOrigination = GetVoiceConnectorOrigination'
  { voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorOrigination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'getVoiceConnectorOrigination_voiceConnectorId' - Undocumented member.
newGetVoiceConnectorOrigination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  GetVoiceConnectorOrigination
newGetVoiceConnectorOrigination pVoiceConnectorId_ =
  GetVoiceConnectorOrigination'
    { voiceConnectorId =
        pVoiceConnectorId_
    }

-- | Undocumented member.
getVoiceConnectorOrigination_voiceConnectorId :: Lens.Lens' GetVoiceConnectorOrigination Prelude.Text
getVoiceConnectorOrigination_voiceConnectorId = Lens.lens (\GetVoiceConnectorOrigination' {voiceConnectorId} -> voiceConnectorId) (\s@GetVoiceConnectorOrigination' {} a -> s {voiceConnectorId = a} :: GetVoiceConnectorOrigination)

instance Core.AWSRequest GetVoiceConnectorOrigination where
  type
    AWSResponse GetVoiceConnectorOrigination =
      GetVoiceConnectorOriginationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceConnectorOriginationResponse'
            Prelude.<$> (x Data..?> "Origination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVoiceConnectorOrigination
  where
  hashWithSalt _salt GetVoiceConnectorOrigination' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData GetVoiceConnectorOrigination where
  rnf GetVoiceConnectorOrigination' {..} =
    Prelude.rnf voiceConnectorId

instance Data.ToHeaders GetVoiceConnectorOrigination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVoiceConnectorOrigination where
  toPath GetVoiceConnectorOrigination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/origination"
      ]

instance Data.ToQuery GetVoiceConnectorOrigination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceConnectorOriginationResponse' smart constructor.
data GetVoiceConnectorOriginationResponse = GetVoiceConnectorOriginationResponse'
  { origination :: Prelude.Maybe Origination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceConnectorOriginationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'origination', 'getVoiceConnectorOriginationResponse_origination' - Undocumented member.
--
-- 'httpStatus', 'getVoiceConnectorOriginationResponse_httpStatus' - The response's http status code.
newGetVoiceConnectorOriginationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceConnectorOriginationResponse
newGetVoiceConnectorOriginationResponse pHttpStatus_ =
  GetVoiceConnectorOriginationResponse'
    { origination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getVoiceConnectorOriginationResponse_origination :: Lens.Lens' GetVoiceConnectorOriginationResponse (Prelude.Maybe Origination)
getVoiceConnectorOriginationResponse_origination = Lens.lens (\GetVoiceConnectorOriginationResponse' {origination} -> origination) (\s@GetVoiceConnectorOriginationResponse' {} a -> s {origination = a} :: GetVoiceConnectorOriginationResponse)

-- | The response's http status code.
getVoiceConnectorOriginationResponse_httpStatus :: Lens.Lens' GetVoiceConnectorOriginationResponse Prelude.Int
getVoiceConnectorOriginationResponse_httpStatus = Lens.lens (\GetVoiceConnectorOriginationResponse' {httpStatus} -> httpStatus) (\s@GetVoiceConnectorOriginationResponse' {} a -> s {httpStatus = a} :: GetVoiceConnectorOriginationResponse)

instance
  Prelude.NFData
    GetVoiceConnectorOriginationResponse
  where
  rnf GetVoiceConnectorOriginationResponse' {..} =
    Prelude.rnf origination `Prelude.seq`
      Prelude.rnf httpStatus
