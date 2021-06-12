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
-- Module      : Network.AWS.SageMaker.UpdateMonitoringSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created schedule.
module Network.AWS.SageMaker.UpdateMonitoringSchedule
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { -- | The name of the monitoring schedule. The name must be unique within an
    -- AWS Region within an AWS account.
    monitoringScheduleName :: Core.Text,
    -- | The configuration object that specifies the monitoring schedule and
    -- defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'updateMonitoringSchedule_monitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an
-- AWS Region within an AWS account.
--
-- 'monitoringScheduleConfig', 'updateMonitoringSchedule_monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
newUpdateMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Core.Text ->
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
-- AWS Region within an AWS account.
updateMonitoringSchedule_monitoringScheduleName :: Lens.Lens' UpdateMonitoringSchedule Core.Text
updateMonitoringSchedule_monitoringScheduleName = Lens.lens (\UpdateMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@UpdateMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: UpdateMonitoringSchedule)

-- | The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
updateMonitoringSchedule_monitoringScheduleConfig :: Lens.Lens' UpdateMonitoringSchedule MonitoringScheduleConfig
updateMonitoringSchedule_monitoringScheduleConfig = Lens.lens (\UpdateMonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@UpdateMonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: UpdateMonitoringSchedule)

instance Core.AWSRequest UpdateMonitoringSchedule where
  type
    AWSResponse UpdateMonitoringSchedule =
      UpdateMonitoringScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitoringScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "MonitoringScheduleArn")
      )

instance Core.Hashable UpdateMonitoringSchedule

instance Core.NFData UpdateMonitoringSchedule

instance Core.ToHeaders UpdateMonitoringSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateMonitoringSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateMonitoringSchedule where
  toJSON UpdateMonitoringSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "MonitoringScheduleName"
                  Core..= monitoringScheduleName
              ),
            Core.Just
              ( "MonitoringScheduleConfig"
                  Core..= monitoringScheduleConfig
              )
          ]
      )

instance Core.ToPath UpdateMonitoringSchedule where
  toPath = Core.const "/"

instance Core.ToQuery UpdateMonitoringSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'monitoringScheduleArn'
  Core.Text ->
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
updateMonitoringScheduleResponse_httpStatus :: Lens.Lens' UpdateMonitoringScheduleResponse Core.Int
updateMonitoringScheduleResponse_httpStatus = Lens.lens (\UpdateMonitoringScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateMonitoringScheduleResponse' {} a -> s {httpStatus = a} :: UpdateMonitoringScheduleResponse)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
updateMonitoringScheduleResponse_monitoringScheduleArn :: Lens.Lens' UpdateMonitoringScheduleResponse Core.Text
updateMonitoringScheduleResponse_monitoringScheduleArn = Lens.lens (\UpdateMonitoringScheduleResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@UpdateMonitoringScheduleResponse' {} a -> s {monitoringScheduleArn = a} :: UpdateMonitoringScheduleResponse)

instance Core.NFData UpdateMonitoringScheduleResponse
