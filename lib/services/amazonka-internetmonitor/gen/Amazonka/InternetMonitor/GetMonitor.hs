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
-- Module      : Amazonka.InternetMonitor.GetMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a monitor in Amazon CloudWatch Internet Monitor
-- based on a monitor name. The information returned includes the Amazon
-- Resource Name (ARN), create time, modified time, resources included in
-- the monitor, and status information.
module Amazonka.InternetMonitor.GetMonitor
  ( -- * Creating a Request
    GetMonitor (..),
    newGetMonitor,

    -- * Request Lenses
    getMonitor_monitorName,

    -- * Destructuring the Response
    GetMonitorResponse (..),
    newGetMonitorResponse,

    -- * Response Lenses
    getMonitorResponse_internetMeasurementsLogDelivery,
    getMonitorResponse_maxCityNetworksToMonitor,
    getMonitorResponse_processingStatus,
    getMonitorResponse_processingStatusInfo,
    getMonitorResponse_tags,
    getMonitorResponse_trafficPercentageToMonitor,
    getMonitorResponse_httpStatus,
    getMonitorResponse_monitorName,
    getMonitorResponse_monitorArn,
    getMonitorResponse_resources,
    getMonitorResponse_status,
    getMonitorResponse_createdAt,
    getMonitorResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMonitor' smart constructor.
data GetMonitor = GetMonitor'
  { -- | The name of the monitor.
    monitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorName', 'getMonitor_monitorName' - The name of the monitor.
newGetMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  GetMonitor
newGetMonitor pMonitorName_ =
  GetMonitor' {monitorName = pMonitorName_}

-- | The name of the monitor.
getMonitor_monitorName :: Lens.Lens' GetMonitor Prelude.Text
getMonitor_monitorName = Lens.lens (\GetMonitor' {monitorName} -> monitorName) (\s@GetMonitor' {} a -> s {monitorName = a} :: GetMonitor)

instance Core.AWSRequest GetMonitor where
  type AWSResponse GetMonitor = GetMonitorResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMonitorResponse'
            Prelude.<$> (x Data..?> "InternetMeasurementsLogDelivery")
            Prelude.<*> (x Data..?> "MaxCityNetworksToMonitor")
            Prelude.<*> (x Data..?> "ProcessingStatus")
            Prelude.<*> (x Data..?> "ProcessingStatusInfo")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TrafficPercentageToMonitor")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MonitorName")
            Prelude.<*> (x Data..:> "MonitorArn")
            Prelude.<*> (x Data..?> "Resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "Status")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable GetMonitor where
  hashWithSalt _salt GetMonitor' {..} =
    _salt `Prelude.hashWithSalt` monitorName

instance Prelude.NFData GetMonitor where
  rnf GetMonitor' {..} = Prelude.rnf monitorName

instance Data.ToHeaders GetMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMonitor where
  toPath GetMonitor' {..} =
    Prelude.mconcat
      ["/v20210603/Monitors/", Data.toBS monitorName]

instance Data.ToQuery GetMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMonitorResponse' smart constructor.
data GetMonitorResponse = GetMonitorResponse'
  { -- | Publish internet measurements for Internet Monitor to another location,
    -- such as an Amazon S3 bucket. The measurements are also published to
    -- Amazon CloudWatch Logs.
    internetMeasurementsLogDelivery :: Prelude.Maybe InternetMeasurementsLogDelivery,
    -- | The maximum number of city-networks to monitor for your resources. A
    -- city-network is the location (city) where clients access your
    -- application resources from and the network or ASN, such as an internet
    -- service provider (ISP), that clients access the resources through. This
    -- limit helps control billing costs.
    --
    -- To learn more, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/IMCityNetworksMaximum.html Choosing a city-network maximum value>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    maxCityNetworksToMonitor :: Prelude.Maybe Prelude.Natural,
    -- | The health of the data processing for the monitor.
    processingStatus :: Prelude.Maybe MonitorProcessingStatusCode,
    -- | Additional information about the health of the data processing for the
    -- monitor.
    processingStatusInfo :: Prelude.Maybe Prelude.Text,
    -- | The tags that have been added to monitor.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The percentage of the internet-facing traffic for your application that
    -- you want to monitor with this monitor.
    trafficPercentageToMonitor :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the monitor.
    monitorName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitor.
    monitorArn :: Prelude.Text,
    -- | The resources that have been added for the monitor. Resources are listed
    -- by their Amazon Resource Names (ARNs).
    resources :: [Prelude.Text],
    -- | The status of the monitor.
    status :: MonitorConfigState,
    -- | The time when the monitor was created.
    createdAt :: Data.ISO8601,
    -- | The last time that the monitor was modified.
    modifiedAt :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'internetMeasurementsLogDelivery', 'getMonitorResponse_internetMeasurementsLogDelivery' - Publish internet measurements for Internet Monitor to another location,
-- such as an Amazon S3 bucket. The measurements are also published to
-- Amazon CloudWatch Logs.
--
-- 'maxCityNetworksToMonitor', 'getMonitorResponse_maxCityNetworksToMonitor' - The maximum number of city-networks to monitor for your resources. A
-- city-network is the location (city) where clients access your
-- application resources from and the network or ASN, such as an internet
-- service provider (ISP), that clients access the resources through. This
-- limit helps control billing costs.
--
-- To learn more, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/IMCityNetworksMaximum.html Choosing a city-network maximum value>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- 'processingStatus', 'getMonitorResponse_processingStatus' - The health of the data processing for the monitor.
--
-- 'processingStatusInfo', 'getMonitorResponse_processingStatusInfo' - Additional information about the health of the data processing for the
-- monitor.
--
-- 'tags', 'getMonitorResponse_tags' - The tags that have been added to monitor.
--
-- 'trafficPercentageToMonitor', 'getMonitorResponse_trafficPercentageToMonitor' - The percentage of the internet-facing traffic for your application that
-- you want to monitor with this monitor.
--
-- 'httpStatus', 'getMonitorResponse_httpStatus' - The response's http status code.
--
-- 'monitorName', 'getMonitorResponse_monitorName' - The name of the monitor.
--
-- 'monitorArn', 'getMonitorResponse_monitorArn' - The Amazon Resource Name (ARN) of the monitor.
--
-- 'resources', 'getMonitorResponse_resources' - The resources that have been added for the monitor. Resources are listed
-- by their Amazon Resource Names (ARNs).
--
-- 'status', 'getMonitorResponse_status' - The status of the monitor.
--
-- 'createdAt', 'getMonitorResponse_createdAt' - The time when the monitor was created.
--
-- 'modifiedAt', 'getMonitorResponse_modifiedAt' - The last time that the monitor was modified.
newGetMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitorName'
  Prelude.Text ->
  -- | 'monitorArn'
  Prelude.Text ->
  -- | 'status'
  MonitorConfigState ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  GetMonitorResponse
newGetMonitorResponse
  pHttpStatus_
  pMonitorName_
  pMonitorArn_
  pStatus_
  pCreatedAt_
  pModifiedAt_ =
    GetMonitorResponse'
      { internetMeasurementsLogDelivery =
          Prelude.Nothing,
        maxCityNetworksToMonitor = Prelude.Nothing,
        processingStatus = Prelude.Nothing,
        processingStatusInfo = Prelude.Nothing,
        tags = Prelude.Nothing,
        trafficPercentageToMonitor = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        monitorName = pMonitorName_,
        monitorArn = pMonitorArn_,
        resources = Prelude.mempty,
        status = pStatus_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | Publish internet measurements for Internet Monitor to another location,
-- such as an Amazon S3 bucket. The measurements are also published to
-- Amazon CloudWatch Logs.
getMonitorResponse_internetMeasurementsLogDelivery :: Lens.Lens' GetMonitorResponse (Prelude.Maybe InternetMeasurementsLogDelivery)
getMonitorResponse_internetMeasurementsLogDelivery = Lens.lens (\GetMonitorResponse' {internetMeasurementsLogDelivery} -> internetMeasurementsLogDelivery) (\s@GetMonitorResponse' {} a -> s {internetMeasurementsLogDelivery = a} :: GetMonitorResponse)

-- | The maximum number of city-networks to monitor for your resources. A
-- city-network is the location (city) where clients access your
-- application resources from and the network or ASN, such as an internet
-- service provider (ISP), that clients access the resources through. This
-- limit helps control billing costs.
--
-- To learn more, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/IMCityNetworksMaximum.html Choosing a city-network maximum value>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
getMonitorResponse_maxCityNetworksToMonitor :: Lens.Lens' GetMonitorResponse (Prelude.Maybe Prelude.Natural)
getMonitorResponse_maxCityNetworksToMonitor = Lens.lens (\GetMonitorResponse' {maxCityNetworksToMonitor} -> maxCityNetworksToMonitor) (\s@GetMonitorResponse' {} a -> s {maxCityNetworksToMonitor = a} :: GetMonitorResponse)

-- | The health of the data processing for the monitor.
getMonitorResponse_processingStatus :: Lens.Lens' GetMonitorResponse (Prelude.Maybe MonitorProcessingStatusCode)
getMonitorResponse_processingStatus = Lens.lens (\GetMonitorResponse' {processingStatus} -> processingStatus) (\s@GetMonitorResponse' {} a -> s {processingStatus = a} :: GetMonitorResponse)

-- | Additional information about the health of the data processing for the
-- monitor.
getMonitorResponse_processingStatusInfo :: Lens.Lens' GetMonitorResponse (Prelude.Maybe Prelude.Text)
getMonitorResponse_processingStatusInfo = Lens.lens (\GetMonitorResponse' {processingStatusInfo} -> processingStatusInfo) (\s@GetMonitorResponse' {} a -> s {processingStatusInfo = a} :: GetMonitorResponse)

-- | The tags that have been added to monitor.
getMonitorResponse_tags :: Lens.Lens' GetMonitorResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getMonitorResponse_tags = Lens.lens (\GetMonitorResponse' {tags} -> tags) (\s@GetMonitorResponse' {} a -> s {tags = a} :: GetMonitorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of the internet-facing traffic for your application that
-- you want to monitor with this monitor.
getMonitorResponse_trafficPercentageToMonitor :: Lens.Lens' GetMonitorResponse (Prelude.Maybe Prelude.Natural)
getMonitorResponse_trafficPercentageToMonitor = Lens.lens (\GetMonitorResponse' {trafficPercentageToMonitor} -> trafficPercentageToMonitor) (\s@GetMonitorResponse' {} a -> s {trafficPercentageToMonitor = a} :: GetMonitorResponse)

-- | The response's http status code.
getMonitorResponse_httpStatus :: Lens.Lens' GetMonitorResponse Prelude.Int
getMonitorResponse_httpStatus = Lens.lens (\GetMonitorResponse' {httpStatus} -> httpStatus) (\s@GetMonitorResponse' {} a -> s {httpStatus = a} :: GetMonitorResponse)

-- | The name of the monitor.
getMonitorResponse_monitorName :: Lens.Lens' GetMonitorResponse Prelude.Text
getMonitorResponse_monitorName = Lens.lens (\GetMonitorResponse' {monitorName} -> monitorName) (\s@GetMonitorResponse' {} a -> s {monitorName = a} :: GetMonitorResponse)

-- | The Amazon Resource Name (ARN) of the monitor.
getMonitorResponse_monitorArn :: Lens.Lens' GetMonitorResponse Prelude.Text
getMonitorResponse_monitorArn = Lens.lens (\GetMonitorResponse' {monitorArn} -> monitorArn) (\s@GetMonitorResponse' {} a -> s {monitorArn = a} :: GetMonitorResponse)

-- | The resources that have been added for the monitor. Resources are listed
-- by their Amazon Resource Names (ARNs).
getMonitorResponse_resources :: Lens.Lens' GetMonitorResponse [Prelude.Text]
getMonitorResponse_resources = Lens.lens (\GetMonitorResponse' {resources} -> resources) (\s@GetMonitorResponse' {} a -> s {resources = a} :: GetMonitorResponse) Prelude.. Lens.coerced

-- | The status of the monitor.
getMonitorResponse_status :: Lens.Lens' GetMonitorResponse MonitorConfigState
getMonitorResponse_status = Lens.lens (\GetMonitorResponse' {status} -> status) (\s@GetMonitorResponse' {} a -> s {status = a} :: GetMonitorResponse)

-- | The time when the monitor was created.
getMonitorResponse_createdAt :: Lens.Lens' GetMonitorResponse Prelude.UTCTime
getMonitorResponse_createdAt = Lens.lens (\GetMonitorResponse' {createdAt} -> createdAt) (\s@GetMonitorResponse' {} a -> s {createdAt = a} :: GetMonitorResponse) Prelude.. Data._Time

-- | The last time that the monitor was modified.
getMonitorResponse_modifiedAt :: Lens.Lens' GetMonitorResponse Prelude.UTCTime
getMonitorResponse_modifiedAt = Lens.lens (\GetMonitorResponse' {modifiedAt} -> modifiedAt) (\s@GetMonitorResponse' {} a -> s {modifiedAt = a} :: GetMonitorResponse) Prelude.. Data._Time

instance Prelude.NFData GetMonitorResponse where
  rnf GetMonitorResponse' {..} =
    Prelude.rnf internetMeasurementsLogDelivery
      `Prelude.seq` Prelude.rnf maxCityNetworksToMonitor
      `Prelude.seq` Prelude.rnf processingStatus
      `Prelude.seq` Prelude.rnf processingStatusInfo
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trafficPercentageToMonitor
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
