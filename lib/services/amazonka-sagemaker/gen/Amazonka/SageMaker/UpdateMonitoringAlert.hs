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
-- Module      : Amazonka.SageMaker.UpdateMonitoringAlert
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the parameters of a model monitor alert.
module Amazonka.SageMaker.UpdateMonitoringAlert
  ( -- * Creating a Request
    UpdateMonitoringAlert (..),
    newUpdateMonitoringAlert,

    -- * Request Lenses
    updateMonitoringAlert_monitoringScheduleName,
    updateMonitoringAlert_monitoringAlertName,
    updateMonitoringAlert_datapointsToAlert,
    updateMonitoringAlert_evaluationPeriod,

    -- * Destructuring the Response
    UpdateMonitoringAlertResponse (..),
    newUpdateMonitoringAlertResponse,

    -- * Response Lenses
    updateMonitoringAlertResponse_monitoringAlertName,
    updateMonitoringAlertResponse_httpStatus,
    updateMonitoringAlertResponse_monitoringScheduleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateMonitoringAlert' smart constructor.
data UpdateMonitoringAlert = UpdateMonitoringAlert'
  { -- | The name of a monitoring schedule.
    monitoringScheduleName :: Prelude.Text,
    -- | The name of a monitoring alert.
    monitoringAlertName :: Prelude.Text,
    -- | Within @EvaluationPeriod@, how many execution failures will raise an
    -- alert.
    datapointsToAlert :: Prelude.Natural,
    -- | The number of most recent monitoring executions to consider when
    -- evaluating alert status.
    evaluationPeriod :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitoringAlert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'updateMonitoringAlert_monitoringScheduleName' - The name of a monitoring schedule.
--
-- 'monitoringAlertName', 'updateMonitoringAlert_monitoringAlertName' - The name of a monitoring alert.
--
-- 'datapointsToAlert', 'updateMonitoringAlert_datapointsToAlert' - Within @EvaluationPeriod@, how many execution failures will raise an
-- alert.
--
-- 'evaluationPeriod', 'updateMonitoringAlert_evaluationPeriod' - The number of most recent monitoring executions to consider when
-- evaluating alert status.
newUpdateMonitoringAlert ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'monitoringAlertName'
  Prelude.Text ->
  -- | 'datapointsToAlert'
  Prelude.Natural ->
  -- | 'evaluationPeriod'
  Prelude.Natural ->
  UpdateMonitoringAlert
newUpdateMonitoringAlert
  pMonitoringScheduleName_
  pMonitoringAlertName_
  pDatapointsToAlert_
  pEvaluationPeriod_ =
    UpdateMonitoringAlert'
      { monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringAlertName = pMonitoringAlertName_,
        datapointsToAlert = pDatapointsToAlert_,
        evaluationPeriod = pEvaluationPeriod_
      }

-- | The name of a monitoring schedule.
updateMonitoringAlert_monitoringScheduleName :: Lens.Lens' UpdateMonitoringAlert Prelude.Text
updateMonitoringAlert_monitoringScheduleName = Lens.lens (\UpdateMonitoringAlert' {monitoringScheduleName} -> monitoringScheduleName) (\s@UpdateMonitoringAlert' {} a -> s {monitoringScheduleName = a} :: UpdateMonitoringAlert)

-- | The name of a monitoring alert.
updateMonitoringAlert_monitoringAlertName :: Lens.Lens' UpdateMonitoringAlert Prelude.Text
updateMonitoringAlert_monitoringAlertName = Lens.lens (\UpdateMonitoringAlert' {monitoringAlertName} -> monitoringAlertName) (\s@UpdateMonitoringAlert' {} a -> s {monitoringAlertName = a} :: UpdateMonitoringAlert)

-- | Within @EvaluationPeriod@, how many execution failures will raise an
-- alert.
updateMonitoringAlert_datapointsToAlert :: Lens.Lens' UpdateMonitoringAlert Prelude.Natural
updateMonitoringAlert_datapointsToAlert = Lens.lens (\UpdateMonitoringAlert' {datapointsToAlert} -> datapointsToAlert) (\s@UpdateMonitoringAlert' {} a -> s {datapointsToAlert = a} :: UpdateMonitoringAlert)

-- | The number of most recent monitoring executions to consider when
-- evaluating alert status.
updateMonitoringAlert_evaluationPeriod :: Lens.Lens' UpdateMonitoringAlert Prelude.Natural
updateMonitoringAlert_evaluationPeriod = Lens.lens (\UpdateMonitoringAlert' {evaluationPeriod} -> evaluationPeriod) (\s@UpdateMonitoringAlert' {} a -> s {evaluationPeriod = a} :: UpdateMonitoringAlert)

instance Core.AWSRequest UpdateMonitoringAlert where
  type
    AWSResponse UpdateMonitoringAlert =
      UpdateMonitoringAlertResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitoringAlertResponse'
            Prelude.<$> (x Data..?> "MonitoringAlertName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MonitoringScheduleArn")
      )

instance Prelude.Hashable UpdateMonitoringAlert where
  hashWithSalt _salt UpdateMonitoringAlert' {..} =
    _salt `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` monitoringAlertName
      `Prelude.hashWithSalt` datapointsToAlert
      `Prelude.hashWithSalt` evaluationPeriod

instance Prelude.NFData UpdateMonitoringAlert where
  rnf UpdateMonitoringAlert' {..} =
    Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf monitoringAlertName
      `Prelude.seq` Prelude.rnf datapointsToAlert
      `Prelude.seq` Prelude.rnf evaluationPeriod

instance Data.ToHeaders UpdateMonitoringAlert where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateMonitoringAlert" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMonitoringAlert where
  toJSON UpdateMonitoringAlert' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Data..= monitoringScheduleName
              ),
            Prelude.Just
              ("MonitoringAlertName" Data..= monitoringAlertName),
            Prelude.Just
              ("DatapointsToAlert" Data..= datapointsToAlert),
            Prelude.Just
              ("EvaluationPeriod" Data..= evaluationPeriod)
          ]
      )

instance Data.ToPath UpdateMonitoringAlert where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMonitoringAlert where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMonitoringAlertResponse' smart constructor.
data UpdateMonitoringAlertResponse = UpdateMonitoringAlertResponse'
  { -- | The name of a monitoring alert.
    monitoringAlertName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitoringAlertResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringAlertName', 'updateMonitoringAlertResponse_monitoringAlertName' - The name of a monitoring alert.
--
-- 'httpStatus', 'updateMonitoringAlertResponse_httpStatus' - The response's http status code.
--
-- 'monitoringScheduleArn', 'updateMonitoringAlertResponse_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
newUpdateMonitoringAlertResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitoringScheduleArn'
  Prelude.Text ->
  UpdateMonitoringAlertResponse
newUpdateMonitoringAlertResponse
  pHttpStatus_
  pMonitoringScheduleArn_ =
    UpdateMonitoringAlertResponse'
      { monitoringAlertName =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        monitoringScheduleArn =
          pMonitoringScheduleArn_
      }

-- | The name of a monitoring alert.
updateMonitoringAlertResponse_monitoringAlertName :: Lens.Lens' UpdateMonitoringAlertResponse (Prelude.Maybe Prelude.Text)
updateMonitoringAlertResponse_monitoringAlertName = Lens.lens (\UpdateMonitoringAlertResponse' {monitoringAlertName} -> monitoringAlertName) (\s@UpdateMonitoringAlertResponse' {} a -> s {monitoringAlertName = a} :: UpdateMonitoringAlertResponse)

-- | The response's http status code.
updateMonitoringAlertResponse_httpStatus :: Lens.Lens' UpdateMonitoringAlertResponse Prelude.Int
updateMonitoringAlertResponse_httpStatus = Lens.lens (\UpdateMonitoringAlertResponse' {httpStatus} -> httpStatus) (\s@UpdateMonitoringAlertResponse' {} a -> s {httpStatus = a} :: UpdateMonitoringAlertResponse)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
updateMonitoringAlertResponse_monitoringScheduleArn :: Lens.Lens' UpdateMonitoringAlertResponse Prelude.Text
updateMonitoringAlertResponse_monitoringScheduleArn = Lens.lens (\UpdateMonitoringAlertResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@UpdateMonitoringAlertResponse' {} a -> s {monitoringScheduleArn = a} :: UpdateMonitoringAlertResponse)

instance Prelude.NFData UpdateMonitoringAlertResponse where
  rnf UpdateMonitoringAlertResponse' {..} =
    Prelude.rnf monitoringAlertName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
