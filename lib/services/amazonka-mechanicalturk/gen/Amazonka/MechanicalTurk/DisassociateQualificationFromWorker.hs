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
-- Module      : Amazonka.MechanicalTurk.DisassociateQualificationFromWorker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DisassociateQualificationFromWorker@ revokes a previously granted
-- Qualification from a user.
--
-- You can provide a text message explaining why the Qualification was
-- revoked. The user who had the Qualification can see this message.
module Amazonka.MechanicalTurk.DisassociateQualificationFromWorker
  ( -- * Creating a Request
    DisassociateQualificationFromWorker (..),
    newDisassociateQualificationFromWorker,

    -- * Request Lenses
    disassociateQualificationFromWorker_reason,
    disassociateQualificationFromWorker_workerId,
    disassociateQualificationFromWorker_qualificationTypeId,

    -- * Destructuring the Response
    DisassociateQualificationFromWorkerResponse (..),
    newDisassociateQualificationFromWorkerResponse,

    -- * Response Lenses
    disassociateQualificationFromWorkerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateQualificationFromWorker' smart constructor.
data DisassociateQualificationFromWorker = DisassociateQualificationFromWorker'
  { -- | A text message that explains why the Qualification was revoked. The user
    -- who had the Qualification sees this message.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker who possesses the Qualification to be revoked.
    workerId :: Prelude.Text,
    -- | The ID of the Qualification type of the Qualification to be revoked.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateQualificationFromWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'disassociateQualificationFromWorker_reason' - A text message that explains why the Qualification was revoked. The user
-- who had the Qualification sees this message.
--
-- 'workerId', 'disassociateQualificationFromWorker_workerId' - The ID of the Worker who possesses the Qualification to be revoked.
--
-- 'qualificationTypeId', 'disassociateQualificationFromWorker_qualificationTypeId' - The ID of the Qualification type of the Qualification to be revoked.
newDisassociateQualificationFromWorker ::
  -- | 'workerId'
  Prelude.Text ->
  -- | 'qualificationTypeId'
  Prelude.Text ->
  DisassociateQualificationFromWorker
newDisassociateQualificationFromWorker
  pWorkerId_
  pQualificationTypeId_ =
    DisassociateQualificationFromWorker'
      { reason =
          Prelude.Nothing,
        workerId = pWorkerId_,
        qualificationTypeId =
          pQualificationTypeId_
      }

-- | A text message that explains why the Qualification was revoked. The user
-- who had the Qualification sees this message.
disassociateQualificationFromWorker_reason :: Lens.Lens' DisassociateQualificationFromWorker (Prelude.Maybe Prelude.Text)
disassociateQualificationFromWorker_reason = Lens.lens (\DisassociateQualificationFromWorker' {reason} -> reason) (\s@DisassociateQualificationFromWorker' {} a -> s {reason = a} :: DisassociateQualificationFromWorker)

-- | The ID of the Worker who possesses the Qualification to be revoked.
disassociateQualificationFromWorker_workerId :: Lens.Lens' DisassociateQualificationFromWorker Prelude.Text
disassociateQualificationFromWorker_workerId = Lens.lens (\DisassociateQualificationFromWorker' {workerId} -> workerId) (\s@DisassociateQualificationFromWorker' {} a -> s {workerId = a} :: DisassociateQualificationFromWorker)

-- | The ID of the Qualification type of the Qualification to be revoked.
disassociateQualificationFromWorker_qualificationTypeId :: Lens.Lens' DisassociateQualificationFromWorker Prelude.Text
disassociateQualificationFromWorker_qualificationTypeId = Lens.lens (\DisassociateQualificationFromWorker' {qualificationTypeId} -> qualificationTypeId) (\s@DisassociateQualificationFromWorker' {} a -> s {qualificationTypeId = a} :: DisassociateQualificationFromWorker)

instance
  Core.AWSRequest
    DisassociateQualificationFromWorker
  where
  type
    AWSResponse DisassociateQualificationFromWorker =
      DisassociateQualificationFromWorkerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateQualificationFromWorkerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateQualificationFromWorker
  where
  hashWithSalt
    _salt
    DisassociateQualificationFromWorker' {..} =
      _salt `Prelude.hashWithSalt` reason
        `Prelude.hashWithSalt` workerId
        `Prelude.hashWithSalt` qualificationTypeId

instance
  Prelude.NFData
    DisassociateQualificationFromWorker
  where
  rnf DisassociateQualificationFromWorker' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf workerId
      `Prelude.seq` Prelude.rnf qualificationTypeId

instance
  Data.ToHeaders
    DisassociateQualificationFromWorker
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.DisassociateQualificationFromWorker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateQualificationFromWorker
  where
  toJSON DisassociateQualificationFromWorker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("WorkerId" Data..= workerId),
            Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId)
          ]
      )

instance
  Data.ToPath
    DisassociateQualificationFromWorker
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateQualificationFromWorker
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateQualificationFromWorkerResponse' smart constructor.
data DisassociateQualificationFromWorkerResponse = DisassociateQualificationFromWorkerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateQualificationFromWorkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateQualificationFromWorkerResponse_httpStatus' - The response's http status code.
newDisassociateQualificationFromWorkerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateQualificationFromWorkerResponse
newDisassociateQualificationFromWorkerResponse
  pHttpStatus_ =
    DisassociateQualificationFromWorkerResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateQualificationFromWorkerResponse_httpStatus :: Lens.Lens' DisassociateQualificationFromWorkerResponse Prelude.Int
disassociateQualificationFromWorkerResponse_httpStatus = Lens.lens (\DisassociateQualificationFromWorkerResponse' {httpStatus} -> httpStatus) (\s@DisassociateQualificationFromWorkerResponse' {} a -> s {httpStatus = a} :: DisassociateQualificationFromWorkerResponse)

instance
  Prelude.NFData
    DisassociateQualificationFromWorkerResponse
  where
  rnf DisassociateQualificationFromWorkerResponse' {..} =
    Prelude.rnf httpStatus
