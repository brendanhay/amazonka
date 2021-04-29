{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateMonitoringSchedule' smart constructor.
data UpdateMonitoringSchedule = UpdateMonitoringSchedule'
  { -- | The name of the monitoring schedule. The name must be unique within an
    -- AWS Region within an AWS account.
    monitoringScheduleName :: Prelude.Text,
    -- | The configuration object that specifies the monitoring schedule and
    -- defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- AWS Region within an AWS account.
updateMonitoringSchedule_monitoringScheduleName :: Lens.Lens' UpdateMonitoringSchedule Prelude.Text
updateMonitoringSchedule_monitoringScheduleName = Lens.lens (\UpdateMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@UpdateMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: UpdateMonitoringSchedule)

-- | The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
updateMonitoringSchedule_monitoringScheduleConfig :: Lens.Lens' UpdateMonitoringSchedule MonitoringScheduleConfig
updateMonitoringSchedule_monitoringScheduleConfig = Lens.lens (\UpdateMonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@UpdateMonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: UpdateMonitoringSchedule)

instance Prelude.AWSRequest UpdateMonitoringSchedule where
  type
    Rs UpdateMonitoringSchedule =
      UpdateMonitoringScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitoringScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "MonitoringScheduleArn")
      )

instance Prelude.Hashable UpdateMonitoringSchedule

instance Prelude.NFData UpdateMonitoringSchedule

instance Prelude.ToHeaders UpdateMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.UpdateMonitoringSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateMonitoringSchedule where
  toJSON UpdateMonitoringSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MonitoringScheduleName"
                  Prelude..= monitoringScheduleName
              ),
            Prelude.Just
              ( "MonitoringScheduleConfig"
                  Prelude..= monitoringScheduleConfig
              )
          ]
      )

instance Prelude.ToPath UpdateMonitoringSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateMonitoringSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMonitoringScheduleResponse' smart constructor.
data UpdateMonitoringScheduleResponse = UpdateMonitoringScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
