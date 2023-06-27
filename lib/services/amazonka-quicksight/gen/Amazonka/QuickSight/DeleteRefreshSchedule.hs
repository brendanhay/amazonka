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
-- Module      : Amazonka.QuickSight.DeleteRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a refresh schedule from a dataset.
module Amazonka.QuickSight.DeleteRefreshSchedule
  ( -- * Creating a Request
    DeleteRefreshSchedule (..),
    newDeleteRefreshSchedule,

    -- * Request Lenses
    deleteRefreshSchedule_dataSetId,
    deleteRefreshSchedule_awsAccountId,
    deleteRefreshSchedule_scheduleId,

    -- * Destructuring the Response
    DeleteRefreshScheduleResponse (..),
    newDeleteRefreshScheduleResponse,

    -- * Response Lenses
    deleteRefreshScheduleResponse_arn,
    deleteRefreshScheduleResponse_requestId,
    deleteRefreshScheduleResponse_scheduleId,
    deleteRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRefreshSchedule' smart constructor.
data DeleteRefreshSchedule = DeleteRefreshSchedule'
  { -- | The ID of the dataset.
    dataSetId :: Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the refresh schedule.
    scheduleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'deleteRefreshSchedule_dataSetId' - The ID of the dataset.
--
-- 'awsAccountId', 'deleteRefreshSchedule_awsAccountId' - The Amazon Web Services account ID.
--
-- 'scheduleId', 'deleteRefreshSchedule_scheduleId' - The ID of the refresh schedule.
newDeleteRefreshSchedule ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'scheduleId'
  Prelude.Text ->
  DeleteRefreshSchedule
newDeleteRefreshSchedule
  pDataSetId_
  pAwsAccountId_
  pScheduleId_ =
    DeleteRefreshSchedule'
      { dataSetId = pDataSetId_,
        awsAccountId = pAwsAccountId_,
        scheduleId = pScheduleId_
      }

-- | The ID of the dataset.
deleteRefreshSchedule_dataSetId :: Lens.Lens' DeleteRefreshSchedule Prelude.Text
deleteRefreshSchedule_dataSetId = Lens.lens (\DeleteRefreshSchedule' {dataSetId} -> dataSetId) (\s@DeleteRefreshSchedule' {} a -> s {dataSetId = a} :: DeleteRefreshSchedule)

-- | The Amazon Web Services account ID.
deleteRefreshSchedule_awsAccountId :: Lens.Lens' DeleteRefreshSchedule Prelude.Text
deleteRefreshSchedule_awsAccountId = Lens.lens (\DeleteRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@DeleteRefreshSchedule' {} a -> s {awsAccountId = a} :: DeleteRefreshSchedule)

-- | The ID of the refresh schedule.
deleteRefreshSchedule_scheduleId :: Lens.Lens' DeleteRefreshSchedule Prelude.Text
deleteRefreshSchedule_scheduleId = Lens.lens (\DeleteRefreshSchedule' {scheduleId} -> scheduleId) (\s@DeleteRefreshSchedule' {} a -> s {scheduleId = a} :: DeleteRefreshSchedule)

instance Core.AWSRequest DeleteRefreshSchedule where
  type
    AWSResponse DeleteRefreshSchedule =
      DeleteRefreshScheduleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ScheduleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRefreshSchedule where
  hashWithSalt _salt DeleteRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` scheduleId

instance Prelude.NFData DeleteRefreshSchedule where
  rnf DeleteRefreshSchedule' {..} =
    Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf scheduleId

instance Data.ToHeaders DeleteRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRefreshSchedule where
  toPath DeleteRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-schedules/",
        Data.toBS scheduleId
      ]

instance Data.ToQuery DeleteRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRefreshScheduleResponse' smart constructor.
data DeleteRefreshScheduleResponse = DeleteRefreshScheduleResponse'
  { -- | The Amazon Resource Name (ARN) for the refresh schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the refresh schedule.
    scheduleId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteRefreshScheduleResponse_arn' - The Amazon Resource Name (ARN) for the refresh schedule.
--
-- 'requestId', 'deleteRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'scheduleId', 'deleteRefreshScheduleResponse_scheduleId' - The ID of the refresh schedule.
--
-- 'status', 'deleteRefreshScheduleResponse_status' - The HTTP status of the request.
newDeleteRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteRefreshScheduleResponse
newDeleteRefreshScheduleResponse pStatus_ =
  DeleteRefreshScheduleResponse'
    { arn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      scheduleId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the refresh schedule.
deleteRefreshScheduleResponse_arn :: Lens.Lens' DeleteRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteRefreshScheduleResponse_arn = Lens.lens (\DeleteRefreshScheduleResponse' {arn} -> arn) (\s@DeleteRefreshScheduleResponse' {} a -> s {arn = a} :: DeleteRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
deleteRefreshScheduleResponse_requestId :: Lens.Lens' DeleteRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteRefreshScheduleResponse_requestId = Lens.lens (\DeleteRefreshScheduleResponse' {requestId} -> requestId) (\s@DeleteRefreshScheduleResponse' {} a -> s {requestId = a} :: DeleteRefreshScheduleResponse)

-- | The ID of the refresh schedule.
deleteRefreshScheduleResponse_scheduleId :: Lens.Lens' DeleteRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
deleteRefreshScheduleResponse_scheduleId = Lens.lens (\DeleteRefreshScheduleResponse' {scheduleId} -> scheduleId) (\s@DeleteRefreshScheduleResponse' {} a -> s {scheduleId = a} :: DeleteRefreshScheduleResponse)

-- | The HTTP status of the request.
deleteRefreshScheduleResponse_status :: Lens.Lens' DeleteRefreshScheduleResponse Prelude.Int
deleteRefreshScheduleResponse_status = Lens.lens (\DeleteRefreshScheduleResponse' {status} -> status) (\s@DeleteRefreshScheduleResponse' {} a -> s {status = a} :: DeleteRefreshScheduleResponse)

instance Prelude.NFData DeleteRefreshScheduleResponse where
  rnf DeleteRefreshScheduleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf scheduleId
      `Prelude.seq` Prelude.rnf status
