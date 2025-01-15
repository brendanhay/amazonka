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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorTermination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorTermination
  ( -- * Creating a Request
    PutVoiceConnectorTermination (..),
    newPutVoiceConnectorTermination,

    -- * Request Lenses
    putVoiceConnectorTermination_voiceConnectorId,
    putVoiceConnectorTermination_termination,

    -- * Destructuring the Response
    PutVoiceConnectorTerminationResponse (..),
    newPutVoiceConnectorTerminationResponse,

    -- * Response Lenses
    putVoiceConnectorTerminationResponse_termination,
    putVoiceConnectorTerminationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorTermination' smart constructor.
data PutVoiceConnectorTermination = PutVoiceConnectorTermination'
  { voiceConnectorId :: Prelude.Text,
    termination :: Termination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorTermination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'putVoiceConnectorTermination_voiceConnectorId' - Undocumented member.
--
-- 'termination', 'putVoiceConnectorTermination_termination' - Undocumented member.
newPutVoiceConnectorTermination ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'termination'
  Termination ->
  PutVoiceConnectorTermination
newPutVoiceConnectorTermination
  pVoiceConnectorId_
  pTermination_ =
    PutVoiceConnectorTermination'
      { voiceConnectorId =
          pVoiceConnectorId_,
        termination = pTermination_
      }

-- | Undocumented member.
putVoiceConnectorTermination_voiceConnectorId :: Lens.Lens' PutVoiceConnectorTermination Prelude.Text
putVoiceConnectorTermination_voiceConnectorId = Lens.lens (\PutVoiceConnectorTermination' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorTermination' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorTermination)

-- | Undocumented member.
putVoiceConnectorTermination_termination :: Lens.Lens' PutVoiceConnectorTermination Termination
putVoiceConnectorTermination_termination = Lens.lens (\PutVoiceConnectorTermination' {termination} -> termination) (\s@PutVoiceConnectorTermination' {} a -> s {termination = a} :: PutVoiceConnectorTermination)

instance Core.AWSRequest PutVoiceConnectorTermination where
  type
    AWSResponse PutVoiceConnectorTermination =
      PutVoiceConnectorTerminationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorTerminationResponse'
            Prelude.<$> (x Data..?> "Termination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutVoiceConnectorTermination
  where
  hashWithSalt _salt PutVoiceConnectorTermination' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` termination

instance Prelude.NFData PutVoiceConnectorTermination where
  rnf PutVoiceConnectorTermination' {..} =
    Prelude.rnf voiceConnectorId `Prelude.seq`
      Prelude.rnf termination

instance Data.ToHeaders PutVoiceConnectorTermination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutVoiceConnectorTermination where
  toJSON PutVoiceConnectorTermination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Termination" Data..= termination)]
      )

instance Data.ToPath PutVoiceConnectorTermination where
  toPath PutVoiceConnectorTermination' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/termination"
      ]

instance Data.ToQuery PutVoiceConnectorTermination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorTerminationResponse' smart constructor.
data PutVoiceConnectorTerminationResponse = PutVoiceConnectorTerminationResponse'
  { termination :: Prelude.Maybe Termination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorTerminationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'termination', 'putVoiceConnectorTerminationResponse_termination' - Undocumented member.
--
-- 'httpStatus', 'putVoiceConnectorTerminationResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorTerminationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorTerminationResponse
newPutVoiceConnectorTerminationResponse pHttpStatus_ =
  PutVoiceConnectorTerminationResponse'
    { termination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putVoiceConnectorTerminationResponse_termination :: Lens.Lens' PutVoiceConnectorTerminationResponse (Prelude.Maybe Termination)
putVoiceConnectorTerminationResponse_termination = Lens.lens (\PutVoiceConnectorTerminationResponse' {termination} -> termination) (\s@PutVoiceConnectorTerminationResponse' {} a -> s {termination = a} :: PutVoiceConnectorTerminationResponse)

-- | The response's http status code.
putVoiceConnectorTerminationResponse_httpStatus :: Lens.Lens' PutVoiceConnectorTerminationResponse Prelude.Int
putVoiceConnectorTerminationResponse_httpStatus = Lens.lens (\PutVoiceConnectorTerminationResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorTerminationResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorTerminationResponse)

instance
  Prelude.NFData
    PutVoiceConnectorTerminationResponse
  where
  rnf PutVoiceConnectorTerminationResponse' {..} =
    Prelude.rnf termination `Prelude.seq`
      Prelude.rnf httpStatus
