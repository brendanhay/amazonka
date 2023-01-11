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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorOrigination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorOrigination
  ( -- * Creating a Request
    PutVoiceConnectorOrigination (..),
    newPutVoiceConnectorOrigination,

    -- * Request Lenses
    putVoiceConnectorOrigination_voiceConnectorId,
    putVoiceConnectorOrigination_origination,

    -- * Destructuring the Response
    PutVoiceConnectorOriginationResponse (..),
    newPutVoiceConnectorOriginationResponse,

    -- * Response Lenses
    putVoiceConnectorOriginationResponse_origination,
    putVoiceConnectorOriginationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorOrigination' smart constructor.
data PutVoiceConnectorOrigination = PutVoiceConnectorOrigination'
  { voiceConnectorId :: Prelude.Text,
    origination :: Origination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorOrigination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'putVoiceConnectorOrigination_voiceConnectorId' - Undocumented member.
--
-- 'origination', 'putVoiceConnectorOrigination_origination' - Undocumented member.
newPutVoiceConnectorOrigination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'origination'
  Origination ->
  PutVoiceConnectorOrigination
newPutVoiceConnectorOrigination
  pVoiceConnectorId_
  pOrigination_ =
    PutVoiceConnectorOrigination'
      { voiceConnectorId =
          pVoiceConnectorId_,
        origination = pOrigination_
      }

-- | Undocumented member.
putVoiceConnectorOrigination_voiceConnectorId :: Lens.Lens' PutVoiceConnectorOrigination Prelude.Text
putVoiceConnectorOrigination_voiceConnectorId = Lens.lens (\PutVoiceConnectorOrigination' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorOrigination' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorOrigination)

-- | Undocumented member.
putVoiceConnectorOrigination_origination :: Lens.Lens' PutVoiceConnectorOrigination Origination
putVoiceConnectorOrigination_origination = Lens.lens (\PutVoiceConnectorOrigination' {origination} -> origination) (\s@PutVoiceConnectorOrigination' {} a -> s {origination = a} :: PutVoiceConnectorOrigination)

instance Core.AWSRequest PutVoiceConnectorOrigination where
  type
    AWSResponse PutVoiceConnectorOrigination =
      PutVoiceConnectorOriginationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorOriginationResponse'
            Prelude.<$> (x Data..?> "Origination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorOrigination
  where
  hashWithSalt _salt PutVoiceConnectorOrigination' {..} =
    _salt `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` origination

instance Prelude.NFData PutVoiceConnectorOrigination where
  rnf PutVoiceConnectorOrigination' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf origination

instance Data.ToHeaders PutVoiceConnectorOrigination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutVoiceConnectorOrigination where
  toJSON PutVoiceConnectorOrigination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Origination" Data..= origination)]
      )

instance Data.ToPath PutVoiceConnectorOrigination where
  toPath PutVoiceConnectorOrigination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/origination"
      ]

instance Data.ToQuery PutVoiceConnectorOrigination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorOriginationResponse' smart constructor.
data PutVoiceConnectorOriginationResponse = PutVoiceConnectorOriginationResponse'
  { origination :: Prelude.Maybe Origination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorOriginationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'origination', 'putVoiceConnectorOriginationResponse_origination' - Undocumented member.
--
-- 'httpStatus', 'putVoiceConnectorOriginationResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorOriginationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorOriginationResponse
newPutVoiceConnectorOriginationResponse pHttpStatus_ =
  PutVoiceConnectorOriginationResponse'
    { origination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putVoiceConnectorOriginationResponse_origination :: Lens.Lens' PutVoiceConnectorOriginationResponse (Prelude.Maybe Origination)
putVoiceConnectorOriginationResponse_origination = Lens.lens (\PutVoiceConnectorOriginationResponse' {origination} -> origination) (\s@PutVoiceConnectorOriginationResponse' {} a -> s {origination = a} :: PutVoiceConnectorOriginationResponse)

-- | The response's http status code.
putVoiceConnectorOriginationResponse_httpStatus :: Lens.Lens' PutVoiceConnectorOriginationResponse Prelude.Int
putVoiceConnectorOriginationResponse_httpStatus = Lens.lens (\PutVoiceConnectorOriginationResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorOriginationResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorOriginationResponse)

instance
  Prelude.NFData
    PutVoiceConnectorOriginationResponse
  where
  rnf PutVoiceConnectorOriginationResponse' {..} =
    Prelude.rnf origination
      `Prelude.seq` Prelude.rnf httpStatus
