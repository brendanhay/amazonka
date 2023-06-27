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
-- Module      : Amazonka.QuickSight.DescribeRefreshSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a summary of a refresh schedule.
module Amazonka.QuickSight.DescribeRefreshSchedule
  ( -- * Creating a Request
    DescribeRefreshSchedule (..),
    newDescribeRefreshSchedule,

    -- * Request Lenses
    describeRefreshSchedule_awsAccountId,
    describeRefreshSchedule_dataSetId,
    describeRefreshSchedule_scheduleId,

    -- * Destructuring the Response
    DescribeRefreshScheduleResponse (..),
    newDescribeRefreshScheduleResponse,

    -- * Response Lenses
    describeRefreshScheduleResponse_arn,
    describeRefreshScheduleResponse_refreshSchedule,
    describeRefreshScheduleResponse_requestId,
    describeRefreshScheduleResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRefreshSchedule' smart constructor.
data DescribeRefreshSchedule = DescribeRefreshSchedule'
  { -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Text,
    -- | The ID of the refresh schedule.
    scheduleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRefreshSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeRefreshSchedule_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'describeRefreshSchedule_dataSetId' - The ID of the dataset.
--
-- 'scheduleId', 'describeRefreshSchedule_scheduleId' - The ID of the refresh schedule.
newDescribeRefreshSchedule ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'scheduleId'
  Prelude.Text ->
  DescribeRefreshSchedule
newDescribeRefreshSchedule
  pAwsAccountId_
  pDataSetId_
  pScheduleId_ =
    DescribeRefreshSchedule'
      { awsAccountId =
          pAwsAccountId_,
        dataSetId = pDataSetId_,
        scheduleId = pScheduleId_
      }

-- | The Amazon Web Services account ID.
describeRefreshSchedule_awsAccountId :: Lens.Lens' DescribeRefreshSchedule Prelude.Text
describeRefreshSchedule_awsAccountId = Lens.lens (\DescribeRefreshSchedule' {awsAccountId} -> awsAccountId) (\s@DescribeRefreshSchedule' {} a -> s {awsAccountId = a} :: DescribeRefreshSchedule)

-- | The ID of the dataset.
describeRefreshSchedule_dataSetId :: Lens.Lens' DescribeRefreshSchedule Prelude.Text
describeRefreshSchedule_dataSetId = Lens.lens (\DescribeRefreshSchedule' {dataSetId} -> dataSetId) (\s@DescribeRefreshSchedule' {} a -> s {dataSetId = a} :: DescribeRefreshSchedule)

-- | The ID of the refresh schedule.
describeRefreshSchedule_scheduleId :: Lens.Lens' DescribeRefreshSchedule Prelude.Text
describeRefreshSchedule_scheduleId = Lens.lens (\DescribeRefreshSchedule' {scheduleId} -> scheduleId) (\s@DescribeRefreshSchedule' {} a -> s {scheduleId = a} :: DescribeRefreshSchedule)

instance Core.AWSRequest DescribeRefreshSchedule where
  type
    AWSResponse DescribeRefreshSchedule =
      DescribeRefreshScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRefreshScheduleResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RefreshSchedule")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRefreshSchedule where
  hashWithSalt _salt DescribeRefreshSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` scheduleId

instance Prelude.NFData DescribeRefreshSchedule where
  rnf DescribeRefreshSchedule' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf scheduleId

instance Data.ToHeaders DescribeRefreshSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRefreshSchedule where
  toPath DescribeRefreshSchedule' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/data-sets/",
        Data.toBS dataSetId,
        "/refresh-schedules/",
        Data.toBS scheduleId
      ]

instance Data.ToQuery DescribeRefreshSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRefreshScheduleResponse' smart constructor.
data DescribeRefreshScheduleResponse = DescribeRefreshScheduleResponse'
  { -- | The Amazon Resource Name (ARN) for the refresh schedule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The refresh schedule.
    refreshSchedule :: Prelude.Maybe RefreshSchedule,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRefreshScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeRefreshScheduleResponse_arn' - The Amazon Resource Name (ARN) for the refresh schedule.
--
-- 'refreshSchedule', 'describeRefreshScheduleResponse_refreshSchedule' - The refresh schedule.
--
-- 'requestId', 'describeRefreshScheduleResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeRefreshScheduleResponse_status' - The HTTP status of the request.
newDescribeRefreshScheduleResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeRefreshScheduleResponse
newDescribeRefreshScheduleResponse pStatus_ =
  DescribeRefreshScheduleResponse'
    { arn =
        Prelude.Nothing,
      refreshSchedule = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the refresh schedule.
describeRefreshScheduleResponse_arn :: Lens.Lens' DescribeRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
describeRefreshScheduleResponse_arn = Lens.lens (\DescribeRefreshScheduleResponse' {arn} -> arn) (\s@DescribeRefreshScheduleResponse' {} a -> s {arn = a} :: DescribeRefreshScheduleResponse)

-- | The refresh schedule.
describeRefreshScheduleResponse_refreshSchedule :: Lens.Lens' DescribeRefreshScheduleResponse (Prelude.Maybe RefreshSchedule)
describeRefreshScheduleResponse_refreshSchedule = Lens.lens (\DescribeRefreshScheduleResponse' {refreshSchedule} -> refreshSchedule) (\s@DescribeRefreshScheduleResponse' {} a -> s {refreshSchedule = a} :: DescribeRefreshScheduleResponse)

-- | The Amazon Web Services request ID for this operation.
describeRefreshScheduleResponse_requestId :: Lens.Lens' DescribeRefreshScheduleResponse (Prelude.Maybe Prelude.Text)
describeRefreshScheduleResponse_requestId = Lens.lens (\DescribeRefreshScheduleResponse' {requestId} -> requestId) (\s@DescribeRefreshScheduleResponse' {} a -> s {requestId = a} :: DescribeRefreshScheduleResponse)

-- | The HTTP status of the request.
describeRefreshScheduleResponse_status :: Lens.Lens' DescribeRefreshScheduleResponse Prelude.Int
describeRefreshScheduleResponse_status = Lens.lens (\DescribeRefreshScheduleResponse' {status} -> status) (\s@DescribeRefreshScheduleResponse' {} a -> s {status = a} :: DescribeRefreshScheduleResponse)

instance
  Prelude.NFData
    DescribeRefreshScheduleResponse
  where
  rnf DescribeRefreshScheduleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf refreshSchedule
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
