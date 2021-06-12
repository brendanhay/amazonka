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
-- Module      : Network.AWS.SageMaker.GetDeviceFleetReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a fleet.
module Network.AWS.SageMaker.GetDeviceFleetReport
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
    getDeviceFleetReportResponse_outputConfig,
    getDeviceFleetReportResponse_reportGenerated,
    getDeviceFleetReportResponse_deviceStats,
    getDeviceFleetReportResponse_description,
    getDeviceFleetReportResponse_agentVersions,
    getDeviceFleetReportResponse_httpStatus,
    getDeviceFleetReportResponse_deviceFleetArn,
    getDeviceFleetReportResponse_deviceFleetName,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newGetDeviceFleetReport' smart constructor.
data GetDeviceFleetReport = GetDeviceFleetReport'
  { -- | The name of the fleet.
    deviceFleetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetDeviceFleetReport
newGetDeviceFleetReport pDeviceFleetName_ =
  GetDeviceFleetReport'
    { deviceFleetName =
        pDeviceFleetName_
    }

-- | The name of the fleet.
getDeviceFleetReport_deviceFleetName :: Lens.Lens' GetDeviceFleetReport Core.Text
getDeviceFleetReport_deviceFleetName = Lens.lens (\GetDeviceFleetReport' {deviceFleetName} -> deviceFleetName) (\s@GetDeviceFleetReport' {} a -> s {deviceFleetName = a} :: GetDeviceFleetReport)

instance Core.AWSRequest GetDeviceFleetReport where
  type
    AWSResponse GetDeviceFleetReport =
      GetDeviceFleetReportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceFleetReportResponse'
            Core.<$> (x Core..?> "ModelStats" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "OutputConfig")
            Core.<*> (x Core..?> "ReportGenerated")
            Core.<*> (x Core..?> "DeviceStats")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "AgentVersions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "DeviceFleetArn")
            Core.<*> (x Core..:> "DeviceFleetName")
      )

instance Core.Hashable GetDeviceFleetReport

instance Core.NFData GetDeviceFleetReport

instance Core.ToHeaders GetDeviceFleetReport where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.GetDeviceFleetReport" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDeviceFleetReport where
  toJSON GetDeviceFleetReport' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DeviceFleetName" Core..= deviceFleetName)
          ]
      )

instance Core.ToPath GetDeviceFleetReport where
  toPath = Core.const "/"

instance Core.ToQuery GetDeviceFleetReport where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDeviceFleetReportResponse' smart constructor.
data GetDeviceFleetReportResponse = GetDeviceFleetReportResponse'
  { -- | Status of model on device.
    modelStats :: Core.Maybe [EdgeModelStat],
    -- | The output configuration for storing sample data collected by the fleet.
    outputConfig :: Core.Maybe EdgeOutputConfig,
    -- | Timestamp of when the report was generated.
    reportGenerated :: Core.Maybe Core.POSIX,
    -- | Status of devices.
    deviceStats :: Core.Maybe DeviceStats,
    -- | Description of the fleet.
    description :: Core.Maybe Core.Text,
    -- | The versions of Edge Manager agent deployed on the fleet.
    agentVersions :: Core.Maybe [AgentVersion],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the device.
    deviceFleetArn :: Core.Text,
    -- | The name of the fleet.
    deviceFleetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'outputConfig', 'getDeviceFleetReportResponse_outputConfig' - The output configuration for storing sample data collected by the fleet.
--
-- 'reportGenerated', 'getDeviceFleetReportResponse_reportGenerated' - Timestamp of when the report was generated.
--
-- 'deviceStats', 'getDeviceFleetReportResponse_deviceStats' - Status of devices.
--
-- 'description', 'getDeviceFleetReportResponse_description' - Description of the fleet.
--
-- 'agentVersions', 'getDeviceFleetReportResponse_agentVersions' - The versions of Edge Manager agent deployed on the fleet.
--
-- 'httpStatus', 'getDeviceFleetReportResponse_httpStatus' - The response's http status code.
--
-- 'deviceFleetArn', 'getDeviceFleetReportResponse_deviceFleetArn' - The Amazon Resource Name (ARN) of the device.
--
-- 'deviceFleetName', 'getDeviceFleetReportResponse_deviceFleetName' - The name of the fleet.
newGetDeviceFleetReportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'deviceFleetArn'
  Core.Text ->
  -- | 'deviceFleetName'
  Core.Text ->
  GetDeviceFleetReportResponse
newGetDeviceFleetReportResponse
  pHttpStatus_
  pDeviceFleetArn_
  pDeviceFleetName_ =
    GetDeviceFleetReportResponse'
      { modelStats =
          Core.Nothing,
        outputConfig = Core.Nothing,
        reportGenerated = Core.Nothing,
        deviceStats = Core.Nothing,
        description = Core.Nothing,
        agentVersions = Core.Nothing,
        httpStatus = pHttpStatus_,
        deviceFleetArn = pDeviceFleetArn_,
        deviceFleetName = pDeviceFleetName_
      }

-- | Status of model on device.
getDeviceFleetReportResponse_modelStats :: Lens.Lens' GetDeviceFleetReportResponse (Core.Maybe [EdgeModelStat])
getDeviceFleetReportResponse_modelStats = Lens.lens (\GetDeviceFleetReportResponse' {modelStats} -> modelStats) (\s@GetDeviceFleetReportResponse' {} a -> s {modelStats = a} :: GetDeviceFleetReportResponse) Core.. Lens.mapping Lens._Coerce

-- | The output configuration for storing sample data collected by the fleet.
getDeviceFleetReportResponse_outputConfig :: Lens.Lens' GetDeviceFleetReportResponse (Core.Maybe EdgeOutputConfig)
getDeviceFleetReportResponse_outputConfig = Lens.lens (\GetDeviceFleetReportResponse' {outputConfig} -> outputConfig) (\s@GetDeviceFleetReportResponse' {} a -> s {outputConfig = a} :: GetDeviceFleetReportResponse)

-- | Timestamp of when the report was generated.
getDeviceFleetReportResponse_reportGenerated :: Lens.Lens' GetDeviceFleetReportResponse (Core.Maybe Core.UTCTime)
getDeviceFleetReportResponse_reportGenerated = Lens.lens (\GetDeviceFleetReportResponse' {reportGenerated} -> reportGenerated) (\s@GetDeviceFleetReportResponse' {} a -> s {reportGenerated = a} :: GetDeviceFleetReportResponse) Core.. Lens.mapping Core._Time

-- | Status of devices.
getDeviceFleetReportResponse_deviceStats :: Lens.Lens' GetDeviceFleetReportResponse (Core.Maybe DeviceStats)
getDeviceFleetReportResponse_deviceStats = Lens.lens (\GetDeviceFleetReportResponse' {deviceStats} -> deviceStats) (\s@GetDeviceFleetReportResponse' {} a -> s {deviceStats = a} :: GetDeviceFleetReportResponse)

-- | Description of the fleet.
getDeviceFleetReportResponse_description :: Lens.Lens' GetDeviceFleetReportResponse (Core.Maybe Core.Text)
getDeviceFleetReportResponse_description = Lens.lens (\GetDeviceFleetReportResponse' {description} -> description) (\s@GetDeviceFleetReportResponse' {} a -> s {description = a} :: GetDeviceFleetReportResponse)

-- | The versions of Edge Manager agent deployed on the fleet.
getDeviceFleetReportResponse_agentVersions :: Lens.Lens' GetDeviceFleetReportResponse (Core.Maybe [AgentVersion])
getDeviceFleetReportResponse_agentVersions = Lens.lens (\GetDeviceFleetReportResponse' {agentVersions} -> agentVersions) (\s@GetDeviceFleetReportResponse' {} a -> s {agentVersions = a} :: GetDeviceFleetReportResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDeviceFleetReportResponse_httpStatus :: Lens.Lens' GetDeviceFleetReportResponse Core.Int
getDeviceFleetReportResponse_httpStatus = Lens.lens (\GetDeviceFleetReportResponse' {httpStatus} -> httpStatus) (\s@GetDeviceFleetReportResponse' {} a -> s {httpStatus = a} :: GetDeviceFleetReportResponse)

-- | The Amazon Resource Name (ARN) of the device.
getDeviceFleetReportResponse_deviceFleetArn :: Lens.Lens' GetDeviceFleetReportResponse Core.Text
getDeviceFleetReportResponse_deviceFleetArn = Lens.lens (\GetDeviceFleetReportResponse' {deviceFleetArn} -> deviceFleetArn) (\s@GetDeviceFleetReportResponse' {} a -> s {deviceFleetArn = a} :: GetDeviceFleetReportResponse)

-- | The name of the fleet.
getDeviceFleetReportResponse_deviceFleetName :: Lens.Lens' GetDeviceFleetReportResponse Core.Text
getDeviceFleetReportResponse_deviceFleetName = Lens.lens (\GetDeviceFleetReportResponse' {deviceFleetName} -> deviceFleetName) (\s@GetDeviceFleetReportResponse' {} a -> s {deviceFleetName = a} :: GetDeviceFleetReportResponse)

instance Core.NFData GetDeviceFleetReportResponse
