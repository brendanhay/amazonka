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
-- Module      : Amazonka.MechanicalTurk.AssociateQualificationWithWorker
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MechanicalTurk.AssociateQualificationWithWorker
  ( -- * Creating a Request
    AssociateQualificationWithWorker (..),
    newAssociateQualificationWithWorker,

    -- * Request Lenses
    associateQualificationWithWorker_integerValue,
    associateQualificationWithWorker_sendNotification,
    associateQualificationWithWorker_qualificationTypeId,
    associateQualificationWithWorker_workerId,

    -- * Destructuring the Response
    AssociateQualificationWithWorkerResponse (..),
    newAssociateQualificationWithWorkerResponse,

    -- * Response Lenses
    associateQualificationWithWorkerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateQualificationWithWorker' smart constructor.
data AssociateQualificationWithWorker = AssociateQualificationWithWorker'
  { -- | The value of the Qualification to assign.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether to send a notification email message to the Worker
    -- saying that the qualification was assigned to the Worker. Note: this is
    -- true by default.
    sendNotification :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Qualification type to use for the assigned Qualification.
    qualificationTypeId :: Prelude.Text,
    -- | The ID of the Worker to whom the Qualification is being assigned. Worker
    -- IDs are included with submitted HIT assignments and Qualification
    -- requests.
    workerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateQualificationWithWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerValue', 'associateQualificationWithWorker_integerValue' - The value of the Qualification to assign.
--
-- 'sendNotification', 'associateQualificationWithWorker_sendNotification' - Specifies whether to send a notification email message to the Worker
-- saying that the qualification was assigned to the Worker. Note: this is
-- true by default.
--
-- 'qualificationTypeId', 'associateQualificationWithWorker_qualificationTypeId' - The ID of the Qualification type to use for the assigned Qualification.
--
-- 'workerId', 'associateQualificationWithWorker_workerId' - The ID of the Worker to whom the Qualification is being assigned. Worker
-- IDs are included with submitted HIT assignments and Qualification
-- requests.
newAssociateQualificationWithWorker ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  -- | 'workerId'
  Prelude.Text ->
  AssociateQualificationWithWorker
newAssociateQualificationWithWorker
  pQualificationTypeId_
  pWorkerId_ =
    AssociateQualificationWithWorker'
      { integerValue =
          Prelude.Nothing,
        sendNotification = Prelude.Nothing,
        qualificationTypeId =
          pQualificationTypeId_,
        workerId = pWorkerId_
      }

-- | The value of the Qualification to assign.
associateQualificationWithWorker_integerValue :: Lens.Lens' AssociateQualificationWithWorker (Prelude.Maybe Prelude.Int)
associateQualificationWithWorker_integerValue = Lens.lens (\AssociateQualificationWithWorker' {integerValue} -> integerValue) (\s@AssociateQualificationWithWorker' {} a -> s {integerValue = a} :: AssociateQualificationWithWorker)

-- | Specifies whether to send a notification email message to the Worker
-- saying that the qualification was assigned to the Worker. Note: this is
-- true by default.
associateQualificationWithWorker_sendNotification :: Lens.Lens' AssociateQualificationWithWorker (Prelude.Maybe Prelude.Bool)
associateQualificationWithWorker_sendNotification = Lens.lens (\AssociateQualificationWithWorker' {sendNotification} -> sendNotification) (\s@AssociateQualificationWithWorker' {} a -> s {sendNotification = a} :: AssociateQualificationWithWorker)

-- | The ID of the Qualification type to use for the assigned Qualification.
associateQualificationWithWorker_qualificationTypeId :: Lens.Lens' AssociateQualificationWithWorker Prelude.Text
associateQualificationWithWorker_qualificationTypeId = Lens.lens (\AssociateQualificationWithWorker' {qualificationTypeId} -> qualificationTypeId) (\s@AssociateQualificationWithWorker' {} a -> s {qualificationTypeId = a} :: AssociateQualificationWithWorker)

-- | The ID of the Worker to whom the Qualification is being assigned. Worker
-- IDs are included with submitted HIT assignments and Qualification
-- requests.
associateQualificationWithWorker_workerId :: Lens.Lens' AssociateQualificationWithWorker Prelude.Text
associateQualificationWithWorker_workerId = Lens.lens (\AssociateQualificationWithWorker' {workerId} -> workerId) (\s@AssociateQualificationWithWorker' {} a -> s {workerId = a} :: AssociateQualificationWithWorker)

instance
  Core.AWSRequest
    AssociateQualificationWithWorker
  where
  type
    AWSResponse AssociateQualificationWithWorker =
      AssociateQualificationWithWorkerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateQualificationWithWorkerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateQualificationWithWorker
  where
  hashWithSalt
    _salt
    AssociateQualificationWithWorker' {..} =
      _salt `Prelude.hashWithSalt` integerValue
        `Prelude.hashWithSalt` sendNotification
        `Prelude.hashWithSalt` qualificationTypeId
        `Prelude.hashWithSalt` workerId

instance
  Prelude.NFData
    AssociateQualificationWithWorker
  where
  rnf AssociateQualificationWithWorker' {..} =
    Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf sendNotification
      `Prelude.seq` Prelude.rnf qualificationTypeId
      `Prelude.seq` Prelude.rnf workerId

instance
  Data.ToHeaders
    AssociateQualificationWithWorker
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.AssociateQualificationWithWorker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateQualificationWithWorker where
  toJSON AssociateQualificationWithWorker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IntegerValue" Data..=) Prelude.<$> integerValue,
            ("SendNotification" Data..=)
              Prelude.<$> sendNotification,
            Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId),
            Prelude.Just ("WorkerId" Data..= workerId)
          ]
      )

instance Data.ToPath AssociateQualificationWithWorker where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AssociateQualificationWithWorker
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateQualificationWithWorkerResponse' smart constructor.
data AssociateQualificationWithWorkerResponse = AssociateQualificationWithWorkerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateQualificationWithWorkerResponse
newAssociateQualificationWithWorkerResponse
  pHttpStatus_ =
    AssociateQualificationWithWorkerResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateQualificationWithWorkerResponse_httpStatus :: Lens.Lens' AssociateQualificationWithWorkerResponse Prelude.Int
associateQualificationWithWorkerResponse_httpStatus = Lens.lens (\AssociateQualificationWithWorkerResponse' {httpStatus} -> httpStatus) (\s@AssociateQualificationWithWorkerResponse' {} a -> s {httpStatus = a} :: AssociateQualificationWithWorkerResponse)

instance
  Prelude.NFData
    AssociateQualificationWithWorkerResponse
  where
  rnf AssociateQualificationWithWorkerResponse' {..} =
    Prelude.rnf httpStatus
