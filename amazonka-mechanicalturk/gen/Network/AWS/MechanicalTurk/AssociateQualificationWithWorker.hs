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
-- Module      : Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AssociateQualificationWithWorker@ operation gives a Worker a
-- Qualification. @AssociateQualificationWithWorker@ does not require that
-- the Worker submit a Qualification request. It gives the Qualification
-- directly to the Worker.
--
-- You can only assign a Qualification of a Qualification type that you
-- created (using the @CreateQualificationType@ operation).
--
-- Note: @AssociateQualificationWithWorker@ does not affect any pending
-- Qualification requests for the Qualification by the Worker. If you
-- assign a Qualification to a Worker, then later grant a Qualification
-- request made by the Worker, the granting of the request may modify the
-- Qualification score. To resolve a pending Qualification request without
-- affecting the Qualification the Worker already has, reject the request
-- with the @RejectQualificationRequest@ operation.
module Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
  ( -- * Creating a Request
    AssociateQualificationWithWorker (..),
    newAssociateQualificationWithWorker,

    -- * Request Lenses
    associateQualificationWithWorker_sendNotification,
    associateQualificationWithWorker_integerValue,
    associateQualificationWithWorker_qualificationTypeId,
    associateQualificationWithWorker_workerId,

    -- * Destructuring the Response
    AssociateQualificationWithWorkerResponse (..),
    newAssociateQualificationWithWorkerResponse,

    -- * Response Lenses
    associateQualificationWithWorkerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateQualificationWithWorker' smart constructor.
data AssociateQualificationWithWorker = AssociateQualificationWithWorker'
  { -- | Specifies whether to send a notification email message to the Worker
    -- saying that the qualification was assigned to the Worker. Note: this is
    -- true by default.
    sendNotification :: Core.Maybe Core.Bool,
    -- | The value of the Qualification to assign.
    integerValue :: Core.Maybe Core.Int,
    -- | The ID of the Qualification type to use for the assigned Qualification.
    qualificationTypeId :: Core.Text,
    -- | The ID of the Worker to whom the Qualification is being assigned. Worker
    -- IDs are included with submitted HIT assignments and Qualification
    -- requests.
    workerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateQualificationWithWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendNotification', 'associateQualificationWithWorker_sendNotification' - Specifies whether to send a notification email message to the Worker
-- saying that the qualification was assigned to the Worker. Note: this is
-- true by default.
--
-- 'integerValue', 'associateQualificationWithWorker_integerValue' - The value of the Qualification to assign.
--
-- 'qualificationTypeId', 'associateQualificationWithWorker_qualificationTypeId' - The ID of the Qualification type to use for the assigned Qualification.
--
-- 'workerId', 'associateQualificationWithWorker_workerId' - The ID of the Worker to whom the Qualification is being assigned. Worker
-- IDs are included with submitted HIT assignments and Qualification
-- requests.
newAssociateQualificationWithWorker ::
  -- | 'qualificationTypeId'
  Core.Text ->
  -- | 'workerId'
  Core.Text ->
  AssociateQualificationWithWorker
newAssociateQualificationWithWorker
  pQualificationTypeId_
  pWorkerId_ =
    AssociateQualificationWithWorker'
      { sendNotification =
          Core.Nothing,
        integerValue = Core.Nothing,
        qualificationTypeId =
          pQualificationTypeId_,
        workerId = pWorkerId_
      }

-- | Specifies whether to send a notification email message to the Worker
-- saying that the qualification was assigned to the Worker. Note: this is
-- true by default.
associateQualificationWithWorker_sendNotification :: Lens.Lens' AssociateQualificationWithWorker (Core.Maybe Core.Bool)
associateQualificationWithWorker_sendNotification = Lens.lens (\AssociateQualificationWithWorker' {sendNotification} -> sendNotification) (\s@AssociateQualificationWithWorker' {} a -> s {sendNotification = a} :: AssociateQualificationWithWorker)

-- | The value of the Qualification to assign.
associateQualificationWithWorker_integerValue :: Lens.Lens' AssociateQualificationWithWorker (Core.Maybe Core.Int)
associateQualificationWithWorker_integerValue = Lens.lens (\AssociateQualificationWithWorker' {integerValue} -> integerValue) (\s@AssociateQualificationWithWorker' {} a -> s {integerValue = a} :: AssociateQualificationWithWorker)

-- | The ID of the Qualification type to use for the assigned Qualification.
associateQualificationWithWorker_qualificationTypeId :: Lens.Lens' AssociateQualificationWithWorker Core.Text
associateQualificationWithWorker_qualificationTypeId = Lens.lens (\AssociateQualificationWithWorker' {qualificationTypeId} -> qualificationTypeId) (\s@AssociateQualificationWithWorker' {} a -> s {qualificationTypeId = a} :: AssociateQualificationWithWorker)

-- | The ID of the Worker to whom the Qualification is being assigned. Worker
-- IDs are included with submitted HIT assignments and Qualification
-- requests.
associateQualificationWithWorker_workerId :: Lens.Lens' AssociateQualificationWithWorker Core.Text
associateQualificationWithWorker_workerId = Lens.lens (\AssociateQualificationWithWorker' {workerId} -> workerId) (\s@AssociateQualificationWithWorker' {} a -> s {workerId = a} :: AssociateQualificationWithWorker)

instance
  Core.AWSRequest
    AssociateQualificationWithWorker
  where
  type
    AWSResponse AssociateQualificationWithWorker =
      AssociateQualificationWithWorkerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateQualificationWithWorkerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociateQualificationWithWorker

instance Core.NFData AssociateQualificationWithWorker

instance
  Core.ToHeaders
    AssociateQualificationWithWorker
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.AssociateQualificationWithWorker" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateQualificationWithWorker where
  toJSON AssociateQualificationWithWorker' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SendNotification" Core..=)
              Core.<$> sendNotification,
            ("IntegerValue" Core..=) Core.<$> integerValue,
            Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId),
            Core.Just ("WorkerId" Core..= workerId)
          ]
      )

instance Core.ToPath AssociateQualificationWithWorker where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AssociateQualificationWithWorker
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateQualificationWithWorkerResponse' smart constructor.
data AssociateQualificationWithWorkerResponse = AssociateQualificationWithWorkerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateQualificationWithWorkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateQualificationWithWorkerResponse_httpStatus' - The response's http status code.
newAssociateQualificationWithWorkerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateQualificationWithWorkerResponse
newAssociateQualificationWithWorkerResponse
  pHttpStatus_ =
    AssociateQualificationWithWorkerResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateQualificationWithWorkerResponse_httpStatus :: Lens.Lens' AssociateQualificationWithWorkerResponse Core.Int
associateQualificationWithWorkerResponse_httpStatus = Lens.lens (\AssociateQualificationWithWorkerResponse' {httpStatus} -> httpStatus) (\s@AssociateQualificationWithWorkerResponse' {} a -> s {httpStatus = a} :: AssociateQualificationWithWorkerResponse)

instance
  Core.NFData
    AssociateQualificationWithWorkerResponse
