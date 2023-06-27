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
-- Module      : Amazonka.IVSRealtime.GetParticipant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified participant token.
module Amazonka.IVSRealtime.GetParticipant
  ( -- * Creating a Request
    GetParticipant (..),
    newGetParticipant,

    -- * Request Lenses
    getParticipant_participantId,
    getParticipant_sessionId,
    getParticipant_stageArn,

    -- * Destructuring the Response
    GetParticipantResponse (..),
    newGetParticipantResponse,

    -- * Response Lenses
    getParticipantResponse_participant,
    getParticipantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetParticipant' smart constructor.
data GetParticipant = GetParticipant'
  { -- | Unique identifier for the participant. This is assigned by IVS and
    -- returned by CreateParticipantToken.
    participantId :: Prelude.Text,
    -- | ID of a session within the stage.
    sessionId :: Prelude.Text,
    -- | Stage ARN.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParticipant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantId', 'getParticipant_participantId' - Unique identifier for the participant. This is assigned by IVS and
-- returned by CreateParticipantToken.
--
-- 'sessionId', 'getParticipant_sessionId' - ID of a session within the stage.
--
-- 'stageArn', 'getParticipant_stageArn' - Stage ARN.
newGetParticipant ::
  -- | 'participantId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'stageArn'
  Prelude.Text ->
  GetParticipant
newGetParticipant
  pParticipantId_
  pSessionId_
  pStageArn_ =
    GetParticipant'
      { participantId = pParticipantId_,
        sessionId = pSessionId_,
        stageArn = pStageArn_
      }

-- | Unique identifier for the participant. This is assigned by IVS and
-- returned by CreateParticipantToken.
getParticipant_participantId :: Lens.Lens' GetParticipant Prelude.Text
getParticipant_participantId = Lens.lens (\GetParticipant' {participantId} -> participantId) (\s@GetParticipant' {} a -> s {participantId = a} :: GetParticipant)

-- | ID of a session within the stage.
getParticipant_sessionId :: Lens.Lens' GetParticipant Prelude.Text
getParticipant_sessionId = Lens.lens (\GetParticipant' {sessionId} -> sessionId) (\s@GetParticipant' {} a -> s {sessionId = a} :: GetParticipant)

-- | Stage ARN.
getParticipant_stageArn :: Lens.Lens' GetParticipant Prelude.Text
getParticipant_stageArn = Lens.lens (\GetParticipant' {stageArn} -> stageArn) (\s@GetParticipant' {} a -> s {stageArn = a} :: GetParticipant)

instance Core.AWSRequest GetParticipant where
  type
    AWSResponse GetParticipant =
      GetParticipantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParticipantResponse'
            Prelude.<$> (x Data..?> "participant")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParticipant where
  hashWithSalt _salt GetParticipant' {..} =
    _salt
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData GetParticipant where
  rnf GetParticipant' {..} =
    Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders GetParticipant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParticipant where
  toJSON GetParticipant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("participantId" Data..= participantId),
            Prelude.Just ("sessionId" Data..= sessionId),
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath GetParticipant where
  toPath = Prelude.const "/GetParticipant"

instance Data.ToQuery GetParticipant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParticipantResponse' smart constructor.
data GetParticipantResponse = GetParticipantResponse'
  { -- | The participant that is returned.
    participant :: Prelude.Maybe Participant,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParticipantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participant', 'getParticipantResponse_participant' - The participant that is returned.
--
-- 'httpStatus', 'getParticipantResponse_httpStatus' - The response's http status code.
newGetParticipantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParticipantResponse
newGetParticipantResponse pHttpStatus_ =
  GetParticipantResponse'
    { participant =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The participant that is returned.
getParticipantResponse_participant :: Lens.Lens' GetParticipantResponse (Prelude.Maybe Participant)
getParticipantResponse_participant = Lens.lens (\GetParticipantResponse' {participant} -> participant) (\s@GetParticipantResponse' {} a -> s {participant = a} :: GetParticipantResponse)

-- | The response's http status code.
getParticipantResponse_httpStatus :: Lens.Lens' GetParticipantResponse Prelude.Int
getParticipantResponse_httpStatus = Lens.lens (\GetParticipantResponse' {httpStatus} -> httpStatus) (\s@GetParticipantResponse' {} a -> s {httpStatus = a} :: GetParticipantResponse)

instance Prelude.NFData GetParticipantResponse where
  rnf GetParticipantResponse' {..} =
    Prelude.rnf participant
      `Prelude.seq` Prelude.rnf httpStatus
