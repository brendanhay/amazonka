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
-- Module      : Amazonka.SageMaker.UpdateMonitoringSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created schedule.
module Amazonka.SageMaker.UpdateMonitoringSchedule
  ( -- * Creating a Request
    UpdateMonitoringSchedule (..),
    newUpdateMonitoringSchedule,

    -- * Request Lenses
    updateMonitoringSchedule_monitoringScheduleName,
    updateMonitoringSchedule_monitoringScheduleConfig,

    -- * Destructuring the Response
    UpdateMonitoringScheduleResponse (..),
    newUpdateMonitoringScheduleResponse,

    -- * Response Lenses
    updateMonitoringScheduleResponse_httpStatus,
    updateMonitoringScheduleResponse_monitoringScheduleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { -- | The name of the monitoring schedule. The name must be unique within an
    -- Amazon Web Services Region within an Amazon Web Services account.
    monitoringScheduleName :: Prelude.Text,
    -- | The configuration object that specifies the monitoring schedule and
    -- defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'updateMonitoringSchedule_monitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an
-- Amazon Web Services Region within an Amazon Web Services account.
--
-- 'monitoringScheduleConfig', 'updateMonitoringSchedule_monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
newUpdateMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'monitoringScheduleConfig'
  MonitoringScheduleConfig ->
  UpdateMonitoringSchedule
newUpdateMonitoringSchedule
  pMonitoringScheduleName_
  pMonitoringScheduleConfig_ =
    UpdateMonitoringSchedule'
      { monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringScheduleConfig =
          pMonitoringScheduleConfig_
      }

-- | The name of the monitoring schedule. The name must be unique within an
-- Amazon Web Services Region within an Amazon Web Services account.
updateMonitoringSchedule_monitoringScheduleName :: Lens.Lens' UpdateMonitoringSchedule Prelude.Text
updateMonitoringSchedule_monitoringScheduleName = Lens.lens (\UpdateMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@UpdateMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: UpdateMonitoringSchedule)

-- | The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
updateMonitoringSchedule_monitoringScheduleConfig :: Lens.Lens' UpdateMonitoringSchedule MonitoringScheduleConfig
updateMonitoringSchedule_monitoringScheduleConfig = Lens.lens (\UpdateMonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@UpdateMonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: UpdateMonitoringSchedule)

instance Core.AWSRequest UpdateMonitoringSchedule where
  type
    AWSResponse UpdateMonitoringSchedule =
      UpdateMonitoringScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitoringScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MonitoringScheduleArn")
      )

instance Prelude.Hashable UpdateMonitoringSchedule where
  hashWithSalt _salt UpdateMonitoringSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` monitoringScheduleConfig

instance Prelude.NFData UpdateMonitoringSchedule where
  rnf UpdateMonitoringSchedule' {..} =
    Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf monitoringScheduleConfig

instance Data.ToHeaders UpdateMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateMonitoringSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMonitoringSchedule where
  toJSON UpdateMonitoringSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Data..= monitoringScheduleName
              ),
            Prelude.Just
              ( "MonitoringScheduleConfig"
                  Data..= monitoringScheduleConfig
              )
          ]
      )

instance Data.ToPath UpdateMonitoringSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateMonitoringSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitoringScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMonitoringScheduleResponse_httpStatus' - The response's http status code.
--
-- 'monitoringScheduleArn', 'updateMonitoringScheduleResponse_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
newUpdateMonitoringScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitoringScheduleArn'
  Prelude.Text ->
  UpdateMonitoringScheduleResponse
newUpdateMonitoringScheduleResponse
  pHttpStatus_
  pMonitoringScheduleArn_ =
    UpdateMonitoringScheduleResponse'
      { httpStatus =
          pHttpStatus_,
        monitoringScheduleArn =
          pMonitoringScheduleArn_
      }

-- | The response's http status code.
updateMonitoringScheduleResponse_httpStatus :: Lens.Lens' UpdateMonitoringScheduleResponse Prelude.Int
updateMonitoringScheduleResponse_httpStatus = Lens.lens (\UpdateMonitoringScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateMonitoringScheduleResponse' {} a -> s {httpStatus = a} :: UpdateMonitoringScheduleResponse)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
updateMonitoringScheduleResponse_monitoringScheduleArn :: Lens.Lens' UpdateMonitoringScheduleResponse Prelude.Text
updateMonitoringScheduleResponse_monitoringScheduleArn = Lens.lens (\UpdateMonitoringScheduleResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@UpdateMonitoringScheduleResponse' {} a -> s {monitoringScheduleArn = a} :: UpdateMonitoringScheduleResponse)

instance
  Prelude.NFData
    UpdateMonitoringScheduleResponse
  where
  rnf UpdateMonitoringScheduleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitoringScheduleArn
