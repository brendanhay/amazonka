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
-- Module      : Amazonka.IVSRealtime.DisconnectParticipant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects a specified participant and revokes the participant
-- permanently from a specified stage.
module Amazonka.IVSRealtime.DisconnectParticipant
  ( -- * Creating a Request
    DisconnectParticipant (..),
    newDisconnectParticipant,

    -- * Request Lenses
    disconnectParticipant_reason,
    disconnectParticipant_participantId,
    disconnectParticipant_stageArn,

    -- * Destructuring the Response
    DisconnectParticipantResponse (..),
    newDisconnectParticipantResponse,

    -- * Response Lenses
    disconnectParticipantResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectParticipant' smart constructor.
data DisconnectParticipant = DisconnectParticipant'
  { -- | Description of why this participant is being disconnected.
    reason :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the participant to be disconnected. This is assigned by
    -- IVS and returned by CreateParticipantToken.
    participantId :: Prelude.Text,
    -- | ARN of the stage to which the participant is attached.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectParticipant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'disconnectParticipant_reason' - Description of why this participant is being disconnected.
--
-- 'participantId', 'disconnectParticipant_participantId' - Identifier of the participant to be disconnected. This is assigned by
-- IVS and returned by CreateParticipantToken.
--
-- 'stageArn', 'disconnectParticipant_stageArn' - ARN of the stage to which the participant is attached.
newDisconnectParticipant ::
  -- | 'participantId'
  Prelude.Text ->
  -- | 'stageArn'
  Prelude.Text ->
  DisconnectParticipant
newDisconnectParticipant pParticipantId_ pStageArn_ =
  DisconnectParticipant'
    { reason = Prelude.Nothing,
      participantId = pParticipantId_,
      stageArn = pStageArn_
    }

-- | Description of why this participant is being disconnected.
disconnectParticipant_reason :: Lens.Lens' DisconnectParticipant (Prelude.Maybe Prelude.Text)
disconnectParticipant_reason = Lens.lens (\DisconnectParticipant' {reason} -> reason) (\s@DisconnectParticipant' {} a -> s {reason = a} :: DisconnectParticipant)

-- | Identifier of the participant to be disconnected. This is assigned by
-- IVS and returned by CreateParticipantToken.
disconnectParticipant_participantId :: Lens.Lens' DisconnectParticipant Prelude.Text
disconnectParticipant_participantId = Lens.lens (\DisconnectParticipant' {participantId} -> participantId) (\s@DisconnectParticipant' {} a -> s {participantId = a} :: DisconnectParticipant)

-- | ARN of the stage to which the participant is attached.
disconnectParticipant_stageArn :: Lens.Lens' DisconnectParticipant Prelude.Text
disconnectParticipant_stageArn = Lens.lens (\DisconnectParticipant' {stageArn} -> stageArn) (\s@DisconnectParticipant' {} a -> s {stageArn = a} :: DisconnectParticipant)

instance Core.AWSRequest DisconnectParticipant where
  type
    AWSResponse DisconnectParticipant =
      DisconnectParticipantResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisconnectParticipantResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisconnectParticipant where
  hashWithSalt _salt DisconnectParticipant' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` participantId
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData DisconnectParticipant where
  rnf DisconnectParticipant' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf participantId
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders DisconnectParticipant where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisconnectParticipant where
  toJSON DisconnectParticipant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("participantId" Data..= participantId),
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath DisconnectParticipant where
  toPath = Prelude.const "/DisconnectParticipant"

instance Data.ToQuery DisconnectParticipant where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisconnectParticipantResponse' smart constructor.
data DisconnectParticipantResponse = DisconnectParticipantResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectParticipantResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disconnectParticipantResponse_httpStatus' - The response's http status code.
newDisconnectParticipantResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisconnectParticipantResponse
newDisconnectParticipantResponse pHttpStatus_ =
  DisconnectParticipantResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disconnectParticipantResponse_httpStatus :: Lens.Lens' DisconnectParticipantResponse Prelude.Int
disconnectParticipantResponse_httpStatus = Lens.lens (\DisconnectParticipantResponse' {httpStatus} -> httpStatus) (\s@DisconnectParticipantResponse' {} a -> s {httpStatus = a} :: DisconnectParticipantResponse)

instance Prelude.NFData DisconnectParticipantResponse where
  rnf DisconnectParticipantResponse' {..} =
    Prelude.rnf httpStatus
