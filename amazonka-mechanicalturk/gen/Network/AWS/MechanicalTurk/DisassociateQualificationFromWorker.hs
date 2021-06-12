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
-- Module      : Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateQualificationFromWorker' smart constructor.
data DisassociateQualificationFromWorker = DisassociateQualificationFromWorker'
  { -- | A text message that explains why the Qualification was revoked. The user
    -- who had the Qualification sees this message.
    reason :: Core.Maybe Core.Text,
    -- | The ID of the Worker who possesses the Qualification to be revoked.
    workerId :: Core.Text,
    -- | The ID of the Qualification type of the Qualification to be revoked.
    qualificationTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'qualificationTypeId'
  Core.Text ->
  DisassociateQualificationFromWorker
newDisassociateQualificationFromWorker
  pWorkerId_
  pQualificationTypeId_ =
    DisassociateQualificationFromWorker'
      { reason =
          Core.Nothing,
        workerId = pWorkerId_,
        qualificationTypeId =
          pQualificationTypeId_
      }

-- | A text message that explains why the Qualification was revoked. The user
-- who had the Qualification sees this message.
disassociateQualificationFromWorker_reason :: Lens.Lens' DisassociateQualificationFromWorker (Core.Maybe Core.Text)
disassociateQualificationFromWorker_reason = Lens.lens (\DisassociateQualificationFromWorker' {reason} -> reason) (\s@DisassociateQualificationFromWorker' {} a -> s {reason = a} :: DisassociateQualificationFromWorker)

-- | The ID of the Worker who possesses the Qualification to be revoked.
disassociateQualificationFromWorker_workerId :: Lens.Lens' DisassociateQualificationFromWorker Core.Text
disassociateQualificationFromWorker_workerId = Lens.lens (\DisassociateQualificationFromWorker' {workerId} -> workerId) (\s@DisassociateQualificationFromWorker' {} a -> s {workerId = a} :: DisassociateQualificationFromWorker)

-- | The ID of the Qualification type of the Qualification to be revoked.
disassociateQualificationFromWorker_qualificationTypeId :: Lens.Lens' DisassociateQualificationFromWorker Core.Text
disassociateQualificationFromWorker_qualificationTypeId = Lens.lens (\DisassociateQualificationFromWorker' {qualificationTypeId} -> qualificationTypeId) (\s@DisassociateQualificationFromWorker' {} a -> s {qualificationTypeId = a} :: DisassociateQualificationFromWorker)

instance
  Core.AWSRequest
    DisassociateQualificationFromWorker
  where
  type
    AWSResponse DisassociateQualificationFromWorker =
      DisassociateQualificationFromWorkerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateQualificationFromWorkerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisassociateQualificationFromWorker

instance
  Core.NFData
    DisassociateQualificationFromWorker

instance
  Core.ToHeaders
    DisassociateQualificationFromWorker
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.DisassociateQualificationFromWorker" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DisassociateQualificationFromWorker
  where
  toJSON DisassociateQualificationFromWorker' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Reason" Core..=) Core.<$> reason,
            Core.Just ("WorkerId" Core..= workerId),
            Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId)
          ]
      )

instance
  Core.ToPath
    DisassociateQualificationFromWorker
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateQualificationFromWorker
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateQualificationFromWorkerResponse' smart constructor.
data DisassociateQualificationFromWorkerResponse = DisassociateQualificationFromWorkerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateQualificationFromWorkerResponse
newDisassociateQualificationFromWorkerResponse
  pHttpStatus_ =
    DisassociateQualificationFromWorkerResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateQualificationFromWorkerResponse_httpStatus :: Lens.Lens' DisassociateQualificationFromWorkerResponse Core.Int
disassociateQualificationFromWorkerResponse_httpStatus = Lens.lens (\DisassociateQualificationFromWorkerResponse' {httpStatus} -> httpStatus) (\s@DisassociateQualificationFromWorkerResponse' {} a -> s {httpStatus = a} :: DisassociateQualificationFromWorkerResponse)

instance
  Core.NFData
    DisassociateQualificationFromWorkerResponse
