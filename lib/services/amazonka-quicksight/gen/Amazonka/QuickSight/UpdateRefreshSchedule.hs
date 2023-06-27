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
-- Module      : Amazonka.QuickSight.UpdateRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a refresh schedule for a dataset.
module Amazonka.QuickSight.UpdateRefreshSchedule
  ( -- * Creating a Request
    UpdateRefreshSchedule (..),
    newUpdateRefreshSchedule,

    -- * Request Lenses
    updateRefreshSchedule_dataSetId,
    updateRefreshSchedule_awsAccountId,
    updateRefreshSchedule_schedule,

    -- * Destructuring the Response
    UpdateRefreshScheduleResponse (..),
    newUpdateRefreshScheduleResponse,

    -- * Response Lenses
    updateRefreshScheduleResponse_arn,
    updateRefreshScheduleResponse_requestId,
    updateRefreshScheduleResponse_scheduleId,
    updateRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRefreshSchedule' smart constructor.
data UpdateRefreshSchedule = UpdateRefreshSchedule'
  { -- | The ID of the dataset.
    dataSetId :: Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The refresh schedule.
    schedule :: RefreshSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'updateRefreshSchedule_dataSetId' - The ID of the dataset.
--
-- 'awsAccountId', 'updateRefreshSchedule_awsAccountId' - The Amazon Web Services account ID.
--
-- 'schedule', 'updateRefreshSchedule_schedule' - The refresh schedule.
newUpdateRefreshSchedule ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'schedule'
  RefreshSchedule ->
  UpdateRefreshSchedule
newUpdateRefreshSchedule
  pDataSetId_
  pAwsAccountId_
  pSchedule_ =
    UpdateRefreshSchedule'
      { dataSetId = pDataSetId_,
        awsAccountId = pAwsAccountId_,
        schedule = pSchedule_
      }

-- | The ID of the dataset.
updateRefreshSchedule_dataSetId :: Lens.Lens' UpdateRefreshSchedule Prelude.Text
updateRefreshSchedule_dataSetId = Lens.lens (\UpdateRefreshSchedule' {dataSetId} -> dataSetId) (\s@UpdateRefreshSchedule' {} a -> s {dataSetId = a} :: UpdateRefreshSchedule)

-- | The Amazon Web Services account ID.
updateRefreshSchedule_awsAccountId :: Lens.Lens' UpdateRefreshSchedule Prelude.Text
updateRefreshSchedule_awsAccountId = Lens.lens (\UpdateRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@UpdateRefreshSchedule' {} a -> s {awsAccountId = a} :: UpdateRefreshSchedule)

-- | The refresh schedule.
updateRefreshSchedule_schedule :: Lens.Lens' UpdateRefreshSchedule RefreshSchedule
updateRefreshSchedule_schedule = Lens.lens (\UpdateRefreshSchedule' {schedule} -> schedule) (\s@UpdateRefreshSchedule' {} a -> s {schedule = a} :: UpdateRefreshSchedule)

instance Core.AWSRequest UpdateRefreshSchedule where
  type
    AWSResponse UpdateRefreshSchedule =
      UpdateRefreshScheduleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ScheduleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRefreshSchedule where
  hashWithSalt _salt UpdateRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` schedule

instance Prelude.NFData UpdateRefreshSchedule where
  rnf UpdateRefreshSchedule' {..} =
    Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf schedule

instance Data.ToHeaders UpdateRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRefreshSchedule where
  toJSON UpdateRefreshSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Schedule" Data..= schedule)]
      )

instance Data.ToPath UpdateRefreshSchedule where
  toPath UpdateRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-schedules"
      ]

instance Data.ToQuery UpdateRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRefreshScheduleResponse' smart constructor.
data UpdateRefreshScheduleResponse = UpdateRefreshScheduleResponse'
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
-- Create a value of 'UpdateRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateRefreshScheduleResponse_arn' - The Amazon Resource Name (ARN) for the refresh schedule.
--
-- 'requestId', 'updateRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'scheduleId', 'updateRefreshScheduleResponse_scheduleId' - The ID of the refresh schedule.
--
-- 'status', 'updateRefreshScheduleResponse_status' - The HTTP status of the request.
newUpdateRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateRefreshScheduleResponse
newUpdateRefreshScheduleResponse pStatus_ =
  UpdateRefreshScheduleResponse'
    { arn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      scheduleId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the refresh schedule.
updateRefreshScheduleResponse_arn :: Lens.Lens' UpdateRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateRefreshScheduleResponse_arn = Lens.lens (\UpdateRefreshScheduleResponse' {arn} -> arn) (\s@UpdateRefreshScheduleResponse' {} a -> s {arn = a} :: UpdateRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
updateRefreshScheduleResponse_requestId :: Lens.Lens' UpdateRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateRefreshScheduleResponse_requestId = Lens.lens (\UpdateRefreshScheduleResponse' {requestId} -> requestId) (\s@UpdateRefreshScheduleResponse' {} a -> s {requestId = a} :: UpdateRefreshScheduleResponse)

-- | The ID of the refresh schedule.
updateRefreshScheduleResponse_scheduleId :: Lens.Lens' UpdateRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
updateRefreshScheduleResponse_scheduleId = Lens.lens (\UpdateRefreshScheduleResponse' {scheduleId} -> scheduleId) (\s@UpdateRefreshScheduleResponse' {} a -> s {scheduleId = a} :: UpdateRefreshScheduleResponse)

-- | The HTTP status of the request.
updateRefreshScheduleResponse_status :: Lens.Lens' UpdateRefreshScheduleResponse Prelude.Int
updateRefreshScheduleResponse_status = Lens.lens (\UpdateRefreshScheduleResponse' {status} -> status) (\s@UpdateRefreshScheduleResponse' {} a -> s {status = a} :: UpdateRefreshScheduleResponse)

instance Prelude.NFData UpdateRefreshScheduleResponse where
  rnf UpdateRefreshScheduleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf scheduleId
      `Prelude.seq` Prelude.rnf status
