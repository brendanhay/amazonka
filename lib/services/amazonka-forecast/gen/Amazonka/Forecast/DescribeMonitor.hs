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
-- Module      : Amazonka.Forecast.DescribeMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a monitor resource. In addition to listing the properties
-- provided in the CreateMonitor request, this operation lists the
-- following properties:
--
-- -   @Baseline@
--
-- -   @CreationTime@
--
-- -   @LastEvaluationTime@
--
-- -   @LastEvaluationState@
--
-- -   @LastModificationTime@
--
-- -   @Message@
--
-- -   @Status@
module Amazonka.Forecast.DescribeMonitor
  ( -- * Creating a Request
    DescribeMonitor (..),
    newDescribeMonitor,

    -- * Request Lenses
    describeMonitor_monitorArn,

    -- * Destructuring the Response
    DescribeMonitorResponse (..),
    newDescribeMonitorResponse,

    -- * Response Lenses
    describeMonitorResponse_baseline,
    describeMonitorResponse_creationTime,
    describeMonitorResponse_estimatedEvaluationTimeRemainingInMinutes,
    describeMonitorResponse_lastEvaluationState,
    describeMonitorResponse_lastEvaluationTime,
    describeMonitorResponse_lastModificationTime,
    describeMonitorResponse_message,
    describeMonitorResponse_monitorArn,
    describeMonitorResponse_monitorName,
    describeMonitorResponse_resourceArn,
    describeMonitorResponse_status,
    describeMonitorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMonitor' smart constructor.
data DescribeMonitor = DescribeMonitor'
  { -- | The Amazon Resource Name (ARN) of the monitor resource to describe.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorArn', 'describeMonitor_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource to describe.
newDescribeMonitor ::
  -- | 'monitorArn'
  Prelude.Text ->
  DescribeMonitor
newDescribeMonitor pMonitorArn_ =
  DescribeMonitor' {monitorArn = pMonitorArn_}

-- | The Amazon Resource Name (ARN) of the monitor resource to describe.
describeMonitor_monitorArn :: Lens.Lens' DescribeMonitor Prelude.Text
describeMonitor_monitorArn = Lens.lens (\DescribeMonitor' {monitorArn} -> monitorArn) (\s@DescribeMonitor' {} a -> s {monitorArn = a} :: DescribeMonitor)

instance Core.AWSRequest DescribeMonitor where
  type
    AWSResponse DescribeMonitor =
      DescribeMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMonitorResponse'
            Prelude.<$> (x Data..?> "Baseline")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> ( x
                            Data..?> "EstimatedEvaluationTimeRemainingInMinutes"
                        )
            Prelude.<*> (x Data..?> "LastEvaluationState")
            Prelude.<*> (x Data..?> "LastEvaluationTime")
            Prelude.<*> (x Data..?> "LastModificationTime")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "MonitorArn")
            Prelude.<*> (x Data..?> "MonitorName")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMonitor where
  hashWithSalt _salt DescribeMonitor' {..} =
    _salt `Prelude.hashWithSalt` monitorArn

instance Prelude.NFData DescribeMonitor where
  rnf DescribeMonitor' {..} = Prelude.rnf monitorArn

instance Data.ToHeaders DescribeMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DescribeMonitor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMonitor where
  toJSON DescribeMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MonitorArn" Data..= monitorArn)]
      )

instance Data.ToPath DescribeMonitor where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMonitorResponse' smart constructor.
data DescribeMonitorResponse = DescribeMonitorResponse'
  { -- | Metrics you can use as a baseline for comparison purposes. Use these
    -- values you interpret monitoring results for an auto predictor.
    baseline :: Prelude.Maybe Baseline,
    -- | The timestamp for when the monitor resource was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The estimated number of minutes remaining before the monitor resource
    -- finishes its current evaluation.
    estimatedEvaluationTimeRemainingInMinutes :: Prelude.Maybe Prelude.Integer,
    -- | The state of the monitor\'s latest evaluation.
    lastEvaluationState :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the latest evaluation completed by the monitor.
    lastEvaluationTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of the latest modification to the monitor.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | An error message, if any, for the monitor.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitor resource described.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the monitor.
    monitorName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the auto predictor being monitored.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the monitor resource.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseline', 'describeMonitorResponse_baseline' - Metrics you can use as a baseline for comparison purposes. Use these
-- values you interpret monitoring results for an auto predictor.
--
-- 'creationTime', 'describeMonitorResponse_creationTime' - The timestamp for when the monitor resource was created.
--
-- 'estimatedEvaluationTimeRemainingInMinutes', 'describeMonitorResponse_estimatedEvaluationTimeRemainingInMinutes' - The estimated number of minutes remaining before the monitor resource
-- finishes its current evaluation.
--
-- 'lastEvaluationState', 'describeMonitorResponse_lastEvaluationState' - The state of the monitor\'s latest evaluation.
--
-- 'lastEvaluationTime', 'describeMonitorResponse_lastEvaluationTime' - The timestamp of the latest evaluation completed by the monitor.
--
-- 'lastModificationTime', 'describeMonitorResponse_lastModificationTime' - The timestamp of the latest modification to the monitor.
--
-- 'message', 'describeMonitorResponse_message' - An error message, if any, for the monitor.
--
-- 'monitorArn', 'describeMonitorResponse_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource described.
--
-- 'monitorName', 'describeMonitorResponse_monitorName' - The name of the monitor.
--
-- 'resourceArn', 'describeMonitorResponse_resourceArn' - The Amazon Resource Name (ARN) of the auto predictor being monitored.
--
-- 'status', 'describeMonitorResponse_status' - The status of the monitor resource.
--
-- 'httpStatus', 'describeMonitorResponse_httpStatus' - The response's http status code.
newDescribeMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMonitorResponse
newDescribeMonitorResponse pHttpStatus_ =
  DescribeMonitorResponse'
    { baseline =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      estimatedEvaluationTimeRemainingInMinutes =
        Prelude.Nothing,
      lastEvaluationState = Prelude.Nothing,
      lastEvaluationTime = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      monitorArn = Prelude.Nothing,
      monitorName = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metrics you can use as a baseline for comparison purposes. Use these
-- values you interpret monitoring results for an auto predictor.
describeMonitorResponse_baseline :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Baseline)
describeMonitorResponse_baseline = Lens.lens (\DescribeMonitorResponse' {baseline} -> baseline) (\s@DescribeMonitorResponse' {} a -> s {baseline = a} :: DescribeMonitorResponse)

-- | The timestamp for when the monitor resource was created.
describeMonitorResponse_creationTime :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.UTCTime)
describeMonitorResponse_creationTime = Lens.lens (\DescribeMonitorResponse' {creationTime} -> creationTime) (\s@DescribeMonitorResponse' {} a -> s {creationTime = a} :: DescribeMonitorResponse) Prelude.. Lens.mapping Data._Time

-- | The estimated number of minutes remaining before the monitor resource
-- finishes its current evaluation.
describeMonitorResponse_estimatedEvaluationTimeRemainingInMinutes :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Integer)
describeMonitorResponse_estimatedEvaluationTimeRemainingInMinutes = Lens.lens (\DescribeMonitorResponse' {estimatedEvaluationTimeRemainingInMinutes} -> estimatedEvaluationTimeRemainingInMinutes) (\s@DescribeMonitorResponse' {} a -> s {estimatedEvaluationTimeRemainingInMinutes = a} :: DescribeMonitorResponse)

-- | The state of the monitor\'s latest evaluation.
describeMonitorResponse_lastEvaluationState :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Text)
describeMonitorResponse_lastEvaluationState = Lens.lens (\DescribeMonitorResponse' {lastEvaluationState} -> lastEvaluationState) (\s@DescribeMonitorResponse' {} a -> s {lastEvaluationState = a} :: DescribeMonitorResponse)

-- | The timestamp of the latest evaluation completed by the monitor.
describeMonitorResponse_lastEvaluationTime :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.UTCTime)
describeMonitorResponse_lastEvaluationTime = Lens.lens (\DescribeMonitorResponse' {lastEvaluationTime} -> lastEvaluationTime) (\s@DescribeMonitorResponse' {} a -> s {lastEvaluationTime = a} :: DescribeMonitorResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp of the latest modification to the monitor.
describeMonitorResponse_lastModificationTime :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.UTCTime)
describeMonitorResponse_lastModificationTime = Lens.lens (\DescribeMonitorResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeMonitorResponse' {} a -> s {lastModificationTime = a} :: DescribeMonitorResponse) Prelude.. Lens.mapping Data._Time

-- | An error message, if any, for the monitor.
describeMonitorResponse_message :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Text)
describeMonitorResponse_message = Lens.lens (\DescribeMonitorResponse' {message} -> message) (\s@DescribeMonitorResponse' {} a -> s {message = a} :: DescribeMonitorResponse)

-- | The Amazon Resource Name (ARN) of the monitor resource described.
describeMonitorResponse_monitorArn :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Text)
describeMonitorResponse_monitorArn = Lens.lens (\DescribeMonitorResponse' {monitorArn} -> monitorArn) (\s@DescribeMonitorResponse' {} a -> s {monitorArn = a} :: DescribeMonitorResponse)

-- | The name of the monitor.
describeMonitorResponse_monitorName :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Text)
describeMonitorResponse_monitorName = Lens.lens (\DescribeMonitorResponse' {monitorName} -> monitorName) (\s@DescribeMonitorResponse' {} a -> s {monitorName = a} :: DescribeMonitorResponse)

-- | The Amazon Resource Name (ARN) of the auto predictor being monitored.
describeMonitorResponse_resourceArn :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Text)
describeMonitorResponse_resourceArn = Lens.lens (\DescribeMonitorResponse' {resourceArn} -> resourceArn) (\s@DescribeMonitorResponse' {} a -> s {resourceArn = a} :: DescribeMonitorResponse)

-- | The status of the monitor resource.
describeMonitorResponse_status :: Lens.Lens' DescribeMonitorResponse (Prelude.Maybe Prelude.Text)
describeMonitorResponse_status = Lens.lens (\DescribeMonitorResponse' {status} -> status) (\s@DescribeMonitorResponse' {} a -> s {status = a} :: DescribeMonitorResponse)

-- | The response's http status code.
describeMonitorResponse_httpStatus :: Lens.Lens' DescribeMonitorResponse Prelude.Int
describeMonitorResponse_httpStatus = Lens.lens (\DescribeMonitorResponse' {httpStatus} -> httpStatus) (\s@DescribeMonitorResponse' {} a -> s {httpStatus = a} :: DescribeMonitorResponse)

instance Prelude.NFData DescribeMonitorResponse where
  rnf DescribeMonitorResponse' {..} =
    Prelude.rnf baseline
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf estimatedEvaluationTimeRemainingInMinutes
      `Prelude.seq` Prelude.rnf lastEvaluationState
      `Prelude.seq` Prelude.rnf lastEvaluationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
