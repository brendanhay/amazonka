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
-- Module      : Amazonka.SageMaker.GetDeviceFleetReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a fleet.
module Amazonka.SageMaker.GetDeviceFleetReport
  ( -- * Creating a Request
    GetDeviceFleetReport (..),
    newGetDeviceFleetReport,

    -- * Request Lenses
    getDeviceFleetReport_deviceFleetName,

    -- * Destructuring the Response
    GetDeviceFleetReportResponse (..),
    newGetDeviceFleetReportResponse,

    -- * Response Lenses
    getDeviceFleetReportResponse_modelStats,
    getDeviceFleetReportResponse_description,
    getDeviceFleetReportResponse_agentVersions,
    getDeviceFleetReportResponse_reportGenerated,
    getDeviceFleetReportResponse_deviceStats,
    getDeviceFleetReportResponse_outputConfig,
    getDeviceFleetReportResponse_httpStatus,
    getDeviceFleetReportResponse_deviceFleetArn,
    getDeviceFleetReportResponse_deviceFleetName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newGetDeviceFleetReport' smart constructor.
data GetDeviceFleetReport = GetDeviceFleetReport'
  { -- | The name of the fleet.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceFleetReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'getDeviceFleetReport_deviceFleetName' - The name of the fleet.
newGetDeviceFleetReport ::
  -- | 'deviceFleetName'
  Prelude.Text ->
  GetDeviceFleetReport
newGetDeviceFleetReport pDeviceFleetName_ =
  GetDeviceFleetReport'
    { deviceFleetName =
        pDeviceFleetName_
    }

-- | The name of the fleet.
getDeviceFleetReport_deviceFleetName :: Lens.Lens' GetDeviceFleetReport Prelude.Text
getDeviceFleetReport_deviceFleetName = Lens.lens (\GetDeviceFleetReport' {deviceFleetName} -> deviceFleetName) (\s@GetDeviceFleetReport' {} a -> s {deviceFleetName = a} :: GetDeviceFleetReport)

instance Core.AWSRequest GetDeviceFleetReport where
  type
    AWSResponse GetDeviceFleetReport =
      GetDeviceFleetReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceFleetReportResponse'
            Prelude.<$> (x Core..?> "ModelStats" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "AgentVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ReportGenerated")
            Prelude.<*> (x Core..?> "DeviceStats")
            Prelude.<*> (x Core..?> "OutputConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DeviceFleetArn")
            Prelude.<*> (x Core..:> "DeviceFleetName")
      )

instance Prelude.Hashable GetDeviceFleetReport where
  hashWithSalt _salt GetDeviceFleetReport' {..} =
    _salt `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData GetDeviceFleetReport where
  rnf GetDeviceFleetReport' {..} =
    Prelude.rnf deviceFleetName

instance Core.ToHeaders GetDeviceFleetReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.GetDeviceFleetReport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDeviceFleetReport where
  toJSON GetDeviceFleetReport' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeviceFleetName" Core..= deviceFleetName)
          ]
      )

instance Core.ToPath GetDeviceFleetReport where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDeviceFleetReport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeviceFleetReportResponse' smart constructor.
data GetDeviceFleetReportResponse = GetDeviceFleetReportResponse'
  { -- | Status of model on device.
    modelStats :: Prelude.Maybe [EdgeModelStat],
    -- | Description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The versions of Edge Manager agent deployed on the fleet.
    agentVersions :: Prelude.Maybe [AgentVersion],
    -- | Timestamp of when the report was generated.
    reportGenerated :: Prelude.Maybe Core.POSIX,
    -- | Status of devices.
    deviceStats :: Prelude.Maybe DeviceStats,
    -- | The output configuration for storing sample data collected by the fleet.
    outputConfig :: Prelude.Maybe EdgeOutputConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the device.
    deviceFleetArn :: Prelude.Text,
    -- | The name of the fleet.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceFleetReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelStats', 'getDeviceFleetReportResponse_modelStats' - Status of model on device.
--
-- 'description', 'getDeviceFleetReportResponse_description' - Description of the fleet.
--
-- 'agentVersions', 'getDeviceFleetReportResponse_agentVersions' - The versions of Edge Manager agent deployed on the fleet.
--
-- 'reportGenerated', 'getDeviceFleetReportResponse_reportGenerated' - Timestamp of when the report was generated.
--
-- 'deviceStats', 'getDeviceFleetReportResponse_deviceStats' - Status of devices.
--
-- 'outputConfig', 'getDeviceFleetReportResponse_outputConfig' - The output configuration for storing sample data collected by the fleet.
--
-- 'httpStatus', 'getDeviceFleetReportResponse_httpStatus' - The response's http status code.
--
-- 'deviceFleetArn', 'getDeviceFleetReportResponse_deviceFleetArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'deviceFleetName', 'getDeviceFleetReportResponse_deviceFleetName' - The name of the fleet.
newGetDeviceFleetReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deviceFleetArn'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  GetDeviceFleetReportResponse
newGetDeviceFleetReportResponse
  pHttpStatus_
  pDeviceFleetArn_
  pDeviceFleetName_ =
    GetDeviceFleetReportResponse'
      { modelStats =
          Prelude.Nothing,
        description = Prelude.Nothing,
        agentVersions = Prelude.Nothing,
        reportGenerated = Prelude.Nothing,
        deviceStats = Prelude.Nothing,
        outputConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        deviceFleetArn = pDeviceFleetArn_,
        deviceFleetName = pDeviceFleetName_
      }

-- | Status of model on device.
getDeviceFleetReportResponse_modelStats :: Lens.Lens' GetDeviceFleetReportResponse (Prelude.Maybe [EdgeModelStat])
getDeviceFleetReportResponse_modelStats = Lens.lens (\GetDeviceFleetReportResponse' {modelStats} -> modelStats) (\s@GetDeviceFleetReportResponse' {} a -> s {modelStats = a} :: GetDeviceFleetReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | Description of the fleet.
getDeviceFleetReportResponse_description :: Lens.Lens' GetDeviceFleetReportResponse (Prelude.Maybe Prelude.Text)
getDeviceFleetReportResponse_description = Lens.lens (\GetDeviceFleetReportResponse' {description} -> description) (\s@GetDeviceFleetReportResponse' {} a -> s {description = a} :: GetDeviceFleetReportResponse)

-- | The versions of Edge Manager agent deployed on the fleet.
getDeviceFleetReportResponse_agentVersions :: Lens.Lens' GetDeviceFleetReportResponse (Prelude.Maybe [AgentVersion])
getDeviceFleetReportResponse_agentVersions = Lens.lens (\GetDeviceFleetReportResponse' {agentVersions} -> agentVersions) (\s@GetDeviceFleetReportResponse' {} a -> s {agentVersions = a} :: GetDeviceFleetReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | Timestamp of when the report was generated.
getDeviceFleetReportResponse_reportGenerated :: Lens.Lens' GetDeviceFleetReportResponse (Prelude.Maybe Prelude.UTCTime)
getDeviceFleetReportResponse_reportGenerated = Lens.lens (\GetDeviceFleetReportResponse' {reportGenerated} -> reportGenerated) (\s@GetDeviceFleetReportResponse' {} a -> s {reportGenerated = a} :: GetDeviceFleetReportResponse) Prelude.. Lens.mapping Core._Time

-- | Status of devices.
getDeviceFleetReportResponse_deviceStats :: Lens.Lens' GetDeviceFleetReportResponse (Prelude.Maybe DeviceStats)
getDeviceFleetReportResponse_deviceStats = Lens.lens (\GetDeviceFleetReportResponse' {deviceStats} -> deviceStats) (\s@GetDeviceFleetReportResponse' {} a -> s {deviceStats = a} :: GetDeviceFleetReportResponse)

-- | The output configuration for storing sample data collected by the fleet.
getDeviceFleetReportResponse_outputConfig :: Lens.Lens' GetDeviceFleetReportResponse (Prelude.Maybe EdgeOutputConfig)
getDeviceFleetReportResponse_outputConfig = Lens.lens (\GetDeviceFleetReportResponse' {outputConfig} -> outputConfig) (\s@GetDeviceFleetReportResponse' {} a -> s {outputConfig = a} :: GetDeviceFleetReportResponse)

-- | The response's http status code.
getDeviceFleetReportResponse_httpStatus :: Lens.Lens' GetDeviceFleetReportResponse Prelude.Int
getDeviceFleetReportResponse_httpStatus = Lens.lens (\GetDeviceFleetReportResponse' {httpStatus} -> httpStatus) (\s@GetDeviceFleetReportResponse' {} a -> s {httpStatus = a} :: GetDeviceFleetReportResponse)

-- | The Amazon Resource Name (ARN) of the device.
getDeviceFleetReportResponse_deviceFleetArn :: Lens.Lens' GetDeviceFleetReportResponse Prelude.Text
getDeviceFleetReportResponse_deviceFleetArn = Lens.lens (\GetDeviceFleetReportResponse' {deviceFleetArn} -> deviceFleetArn) (\s@GetDeviceFleetReportResponse' {} a -> s {deviceFleetArn = a} :: GetDeviceFleetReportResponse)

-- | The name of the fleet.
getDeviceFleetReportResponse_deviceFleetName :: Lens.Lens' GetDeviceFleetReportResponse Prelude.Text
getDeviceFleetReportResponse_deviceFleetName = Lens.lens (\GetDeviceFleetReportResponse' {deviceFleetName} -> deviceFleetName) (\s@GetDeviceFleetReportResponse' {} a -> s {deviceFleetName = a} :: GetDeviceFleetReportResponse)

instance Prelude.NFData GetDeviceFleetReportResponse where
  rnf GetDeviceFleetReportResponse' {..} =
    Prelude.rnf modelStats
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf agentVersions
      `Prelude.seq` Prelude.rnf reportGenerated
      `Prelude.seq` Prelude.rnf deviceStats
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deviceFleetArn
      `Prelude.seq` Prelude.rnf deviceFleetName
