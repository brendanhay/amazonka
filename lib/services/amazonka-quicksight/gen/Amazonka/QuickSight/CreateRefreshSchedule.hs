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
-- Module      : Amazonka.QuickSight.CreateRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a refresh schedule for a dataset. You can create up to 5
-- different schedules for a single dataset.
module Amazonka.QuickSight.CreateRefreshSchedule
  ( -- * Creating a Request
    CreateRefreshSchedule (..),
    newCreateRefreshSchedule,

    -- * Request Lenses
    createRefreshSchedule_dataSetId,
    createRefreshSchedule_awsAccountId,
    createRefreshSchedule_schedule,

    -- * Destructuring the Response
    CreateRefreshScheduleResponse (..),
    newCreateRefreshScheduleResponse,

    -- * Response Lenses
    createRefreshScheduleResponse_arn,
    createRefreshScheduleResponse_requestId,
    createRefreshScheduleResponse_scheduleId,
    createRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRefreshSchedule' smart constructor.
data CreateRefreshSchedule = CreateRefreshSchedule'
  { -- | The ID of the dataset.
    dataSetId :: Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The refresh schedule.
    schedule :: RefreshSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'createRefreshSchedule_dataSetId' - The ID of the dataset.
--
-- 'awsAccountId', 'createRefreshSchedule_awsAccountId' - The Amazon Web Services account ID.
--
-- 'schedule', 'createRefreshSchedule_schedule' - The refresh schedule.
newCreateRefreshSchedule ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'schedule'
  RefreshSchedule ->
  CreateRefreshSchedule
newCreateRefreshSchedule
  pDataSetId_
  pAwsAccountId_
  pSchedule_ =
    CreateRefreshSchedule'
      { dataSetId = pDataSetId_,
        awsAccountId = pAwsAccountId_,
        schedule = pSchedule_
      }

-- | The ID of the dataset.
createRefreshSchedule_dataSetId :: Lens.Lens' CreateRefreshSchedule Prelude.Text
createRefreshSchedule_dataSetId = Lens.lens (\CreateRefreshSchedule' {dataSetId} -> dataSetId) (\s@CreateRefreshSchedule' {} a -> s {dataSetId = a} :: CreateRefreshSchedule)

-- | The Amazon Web Services account ID.
createRefreshSchedule_awsAccountId :: Lens.Lens' CreateRefreshSchedule Prelude.Text
createRefreshSchedule_awsAccountId = Lens.lens (\CreateRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@CreateRefreshSchedule' {} a -> s {awsAccountId = a} :: CreateRefreshSchedule)

-- | The refresh schedule.
createRefreshSchedule_schedule :: Lens.Lens' CreateRefreshSchedule RefreshSchedule
createRefreshSchedule_schedule = Lens.lens (\CreateRefreshSchedule' {schedule} -> schedule) (\s@CreateRefreshSchedule' {} a -> s {schedule = a} :: CreateRefreshSchedule)

instance Core.AWSRequest CreateRefreshSchedule where
  type
    AWSResponse CreateRefreshSchedule =
      CreateRefreshScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ScheduleId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRefreshSchedule where
  hashWithSalt _salt CreateRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` schedule

instance Prelude.NFData CreateRefreshSchedule where
  rnf CreateRefreshSchedule' {..} =
    Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf schedule

instance Data.ToHeaders CreateRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRefreshSchedule where
  toJSON CreateRefreshSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Schedule" Data..= schedule)]
      )

instance Data.ToPath CreateRefreshSchedule where
  toPath CreateRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-schedules"
      ]

instance Data.ToQuery CreateRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRefreshScheduleResponse' smart constructor.
data CreateRefreshScheduleResponse = CreateRefreshScheduleResponse'
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
-- Create a value of 'CreateRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createRefreshScheduleResponse_arn' - The Amazon Resource Name (ARN) for the refresh schedule.
--
-- 'requestId', 'createRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'scheduleId', 'createRefreshScheduleResponse_scheduleId' - The ID of the refresh schedule.
--
-- 'status', 'createRefreshScheduleResponse_status' - The HTTP status of the request.
newCreateRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateRefreshScheduleResponse
newCreateRefreshScheduleResponse pStatus_ =
  CreateRefreshScheduleResponse'
    { arn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      scheduleId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the refresh schedule.
createRefreshScheduleResponse_arn :: Lens.Lens' CreateRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createRefreshScheduleResponse_arn = Lens.lens (\CreateRefreshScheduleResponse' {arn} -> arn) (\s@CreateRefreshScheduleResponse' {} a -> s {arn = a} :: CreateRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
createRefreshScheduleResponse_requestId :: Lens.Lens' CreateRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createRefreshScheduleResponse_requestId = Lens.lens (\CreateRefreshScheduleResponse' {requestId} -> requestId) (\s@CreateRefreshScheduleResponse' {} a -> s {requestId = a} :: CreateRefreshScheduleResponse)

-- | The ID of the refresh schedule.
createRefreshScheduleResponse_scheduleId :: Lens.Lens' CreateRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
createRefreshScheduleResponse_scheduleId = Lens.lens (\CreateRefreshScheduleResponse' {scheduleId} -> scheduleId) (\s@CreateRefreshScheduleResponse' {} a -> s {scheduleId = a} :: CreateRefreshScheduleResponse)

-- | The HTTP status of the request.
createRefreshScheduleResponse_status :: Lens.Lens' CreateRefreshScheduleResponse Prelude.Int
createRefreshScheduleResponse_status = Lens.lens (\CreateRefreshScheduleResponse' {status} -> status) (\s@CreateRefreshScheduleResponse' {} a -> s {status = a} :: CreateRefreshScheduleResponse)

instance Prelude.NFData CreateRefreshScheduleResponse where
  rnf CreateRefreshScheduleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf scheduleId
      `Prelude.seq` Prelude.rnf status
