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
-- Module      : Amazonka.InternetMonitor.UpdateMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a monitor. You can update a monitor to change the maximum number
-- of city-networks (locations and ASNs or internet service providers), to
-- add or remove resources, or to change the status of the monitor. Note
-- that you can\'t change the name of a monitor.
--
-- The city-network maximum that you choose is the limit, but you only pay
-- for the number of city-networks that are actually monitored. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/IMCityNetworksMaximum.html Choosing a city-network maximum value>
-- in the /Amazon CloudWatch User Guide/.
module Amazonka.InternetMonitor.UpdateMonitor
  ( -- * Creating a Request
    UpdateMonitor (..),
    newUpdateMonitor,

    -- * Request Lenses
    updateMonitor_clientToken,
    updateMonitor_internetMeasurementsLogDelivery,
    updateMonitor_maxCityNetworksToMonitor,
    updateMonitor_resourcesToAdd,
    updateMonitor_resourcesToRemove,
    updateMonitor_status,
    updateMonitor_trafficPercentageToMonitor,
    updateMonitor_monitorName,

    -- * Destructuring the Response
    UpdateMonitorResponse (..),
    newUpdateMonitorResponse,

    -- * Response Lenses
    updateMonitorResponse_httpStatus,
    updateMonitorResponse_monitorArn,
    updateMonitorResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMonitor' smart constructor.
data UpdateMonitor = UpdateMonitor'
  { -- | A unique, case-sensitive string of up to 64 ASCII characters that you
    -- specify to make an idempotent API request. You should not reuse the same
    -- client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Publish internet measurements for Internet Monitor to another location,
    -- such as an Amazon S3 bucket. The measurements are also published to
    -- Amazon CloudWatch Logs.
    internetMeasurementsLogDelivery :: Prelude.Maybe InternetMeasurementsLogDelivery,
    -- | The maximum number of city-networks to monitor for your resources. A
    -- city-network is the location (city) where clients access your
    -- application resources from and the network or ASN, such as an internet
    -- service provider, that clients access the resources through.
    maxCityNetworksToMonitor :: Prelude.Maybe Prelude.Natural,
    -- | The resources to include in a monitor, which you provide as a set of
    -- Amazon Resource Names (ARNs).
    --
    -- You can add a combination of Amazon Virtual Private Clouds (VPCs) and
    -- Amazon CloudFront distributions, or you can add Amazon WorkSpaces
    -- directories. You can\'t add all three types of resources.
    --
    -- If you add only VPC resources, at least one VPC must have an Internet
    -- Gateway attached to it, to make sure that it has internet connectivity.
    resourcesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The resources to remove from a monitor, which you provide as a set of
    -- Amazon Resource Names (ARNs).
    resourcesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The status for a monitor. The accepted values for @Status@ with the
    -- @UpdateMonitor@ API call are the following: @ACTIVE@ and @INACTIVE@. The
    -- following values are /not/ accepted: @PENDING@, and @ERROR@.
    status :: Prelude.Maybe MonitorConfigState,
    -- | The percentage of the internet-facing traffic for your application that
    -- you want to monitor with this monitor.
    trafficPercentageToMonitor :: Prelude.Maybe Prelude.Natural,
    -- | The name of the monitor.
    monitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateMonitor_clientToken' - A unique, case-sensitive string of up to 64 ASCII characters that you
-- specify to make an idempotent API request. You should not reuse the same
-- client token for other API requests.
--
-- 'internetMeasurementsLogDelivery', 'updateMonitor_internetMeasurementsLogDelivery' - Publish internet measurements for Internet Monitor to another location,
-- such as an Amazon S3 bucket. The measurements are also published to
-- Amazon CloudWatch Logs.
--
-- 'maxCityNetworksToMonitor', 'updateMonitor_maxCityNetworksToMonitor' - The maximum number of city-networks to monitor for your resources. A
-- city-network is the location (city) where clients access your
-- application resources from and the network or ASN, such as an internet
-- service provider, that clients access the resources through.
--
-- 'resourcesToAdd', 'updateMonitor_resourcesToAdd' - The resources to include in a monitor, which you provide as a set of
-- Amazon Resource Names (ARNs).
--
-- You can add a combination of Amazon Virtual Private Clouds (VPCs) and
-- Amazon CloudFront distributions, or you can add Amazon WorkSpaces
-- directories. You can\'t add all three types of resources.
--
-- If you add only VPC resources, at least one VPC must have an Internet
-- Gateway attached to it, to make sure that it has internet connectivity.
--
-- 'resourcesToRemove', 'updateMonitor_resourcesToRemove' - The resources to remove from a monitor, which you provide as a set of
-- Amazon Resource Names (ARNs).
--
-- 'status', 'updateMonitor_status' - The status for a monitor. The accepted values for @Status@ with the
-- @UpdateMonitor@ API call are the following: @ACTIVE@ and @INACTIVE@. The
-- following values are /not/ accepted: @PENDING@, and @ERROR@.
--
-- 'trafficPercentageToMonitor', 'updateMonitor_trafficPercentageToMonitor' - The percentage of the internet-facing traffic for your application that
-- you want to monitor with this monitor.
--
-- 'monitorName', 'updateMonitor_monitorName' - The name of the monitor.
newUpdateMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  UpdateMonitor
newUpdateMonitor pMonitorName_ =
  UpdateMonitor'
    { clientToken = Prelude.Nothing,
      internetMeasurementsLogDelivery = Prelude.Nothing,
      maxCityNetworksToMonitor = Prelude.Nothing,
      resourcesToAdd = Prelude.Nothing,
      resourcesToRemove = Prelude.Nothing,
      status = Prelude.Nothing,
      trafficPercentageToMonitor = Prelude.Nothing,
      monitorName = pMonitorName_
    }

-- | A unique, case-sensitive string of up to 64 ASCII characters that you
-- specify to make an idempotent API request. You should not reuse the same
-- client token for other API requests.
updateMonitor_clientToken :: Lens.Lens' UpdateMonitor (Prelude.Maybe Prelude.Text)
updateMonitor_clientToken = Lens.lens (\UpdateMonitor' {clientToken} -> clientToken) (\s@UpdateMonitor' {} a -> s {clientToken = a} :: UpdateMonitor)

-- | Publish internet measurements for Internet Monitor to another location,
-- such as an Amazon S3 bucket. The measurements are also published to
-- Amazon CloudWatch Logs.
updateMonitor_internetMeasurementsLogDelivery :: Lens.Lens' UpdateMonitor (Prelude.Maybe InternetMeasurementsLogDelivery)
updateMonitor_internetMeasurementsLogDelivery = Lens.lens (\UpdateMonitor' {internetMeasurementsLogDelivery} -> internetMeasurementsLogDelivery) (\s@UpdateMonitor' {} a -> s {internetMeasurementsLogDelivery = a} :: UpdateMonitor)

-- | The maximum number of city-networks to monitor for your resources. A
-- city-network is the location (city) where clients access your
-- application resources from and the network or ASN, such as an internet
-- service provider, that clients access the resources through.
updateMonitor_maxCityNetworksToMonitor :: Lens.Lens' UpdateMonitor (Prelude.Maybe Prelude.Natural)
updateMonitor_maxCityNetworksToMonitor = Lens.lens (\UpdateMonitor' {maxCityNetworksToMonitor} -> maxCityNetworksToMonitor) (\s@UpdateMonitor' {} a -> s {maxCityNetworksToMonitor = a} :: UpdateMonitor)

-- | The resources to include in a monitor, which you provide as a set of
-- Amazon Resource Names (ARNs).
--
-- You can add a combination of Amazon Virtual Private Clouds (VPCs) and
-- Amazon CloudFront distributions, or you can add Amazon WorkSpaces
-- directories. You can\'t add all three types of resources.
--
-- If you add only VPC resources, at least one VPC must have an Internet
-- Gateway attached to it, to make sure that it has internet connectivity.
updateMonitor_resourcesToAdd :: Lens.Lens' UpdateMonitor (Prelude.Maybe [Prelude.Text])
updateMonitor_resourcesToAdd = Lens.lens (\UpdateMonitor' {resourcesToAdd} -> resourcesToAdd) (\s@UpdateMonitor' {} a -> s {resourcesToAdd = a} :: UpdateMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The resources to remove from a monitor, which you provide as a set of
-- Amazon Resource Names (ARNs).
updateMonitor_resourcesToRemove :: Lens.Lens' UpdateMonitor (Prelude.Maybe [Prelude.Text])
updateMonitor_resourcesToRemove = Lens.lens (\UpdateMonitor' {resourcesToRemove} -> resourcesToRemove) (\s@UpdateMonitor' {} a -> s {resourcesToRemove = a} :: UpdateMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The status for a monitor. The accepted values for @Status@ with the
-- @UpdateMonitor@ API call are the following: @ACTIVE@ and @INACTIVE@. The
-- following values are /not/ accepted: @PENDING@, and @ERROR@.
updateMonitor_status :: Lens.Lens' UpdateMonitor (Prelude.Maybe MonitorConfigState)
updateMonitor_status = Lens.lens (\UpdateMonitor' {status} -> status) (\s@UpdateMonitor' {} a -> s {status = a} :: UpdateMonitor)

-- | The percentage of the internet-facing traffic for your application that
-- you want to monitor with this monitor.
updateMonitor_trafficPercentageToMonitor :: Lens.Lens' UpdateMonitor (Prelude.Maybe Prelude.Natural)
updateMonitor_trafficPercentageToMonitor = Lens.lens (\UpdateMonitor' {trafficPercentageToMonitor} -> trafficPercentageToMonitor) (\s@UpdateMonitor' {} a -> s {trafficPercentageToMonitor = a} :: UpdateMonitor)

-- | The name of the monitor.
updateMonitor_monitorName :: Lens.Lens' UpdateMonitor Prelude.Text
updateMonitor_monitorName = Lens.lens (\UpdateMonitor' {monitorName} -> monitorName) (\s@UpdateMonitor' {} a -> s {monitorName = a} :: UpdateMonitor)

instance Core.AWSRequest UpdateMonitor where
  type
    AWSResponse UpdateMonitor =
      UpdateMonitorResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MonitorArn")
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable UpdateMonitor where
  hashWithSalt _salt UpdateMonitor' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` internetMeasurementsLogDelivery
      `Prelude.hashWithSalt` maxCityNetworksToMonitor
      `Prelude.hashWithSalt` resourcesToAdd
      `Prelude.hashWithSalt` resourcesToRemove
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` trafficPercentageToMonitor
      `Prelude.hashWithSalt` monitorName

instance Prelude.NFData UpdateMonitor where
  rnf UpdateMonitor' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf internetMeasurementsLogDelivery
      `Prelude.seq` Prelude.rnf maxCityNetworksToMonitor
      `Prelude.seq` Prelude.rnf resourcesToAdd
      `Prelude.seq` Prelude.rnf resourcesToRemove
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf trafficPercentageToMonitor
      `Prelude.seq` Prelude.rnf monitorName

instance Data.ToHeaders UpdateMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMonitor where
  toJSON UpdateMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("InternetMeasurementsLogDelivery" Data..=)
              Prelude.<$> internetMeasurementsLogDelivery,
            ("MaxCityNetworksToMonitor" Data..=)
              Prelude.<$> maxCityNetworksToMonitor,
            ("ResourcesToAdd" Data..=)
              Prelude.<$> resourcesToAdd,
            ("ResourcesToRemove" Data..=)
              Prelude.<$> resourcesToRemove,
            ("Status" Data..=) Prelude.<$> status,
            ("TrafficPercentageToMonitor" Data..=)
              Prelude.<$> trafficPercentageToMonitor
          ]
      )

instance Data.ToPath UpdateMonitor where
  toPath UpdateMonitor' {..} =
    Prelude.mconcat
      ["/v20210603/Monitors/", Data.toBS monitorName]

instance Data.ToQuery UpdateMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMonitorResponse' smart constructor.
data UpdateMonitorResponse = UpdateMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitor.
    monitorArn :: Prelude.Text,
    -- | The status of a monitor.
    status :: MonitorConfigState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMonitorResponse_httpStatus' - The response's http status code.
--
-- 'monitorArn', 'updateMonitorResponse_monitorArn' - The Amazon Resource Name (ARN) of the monitor.
--
-- 'status', 'updateMonitorResponse_status' - The status of a monitor.
newUpdateMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitorArn'
  Prelude.Text ->
  -- | 'status'
  MonitorConfigState ->
  UpdateMonitorResponse
newUpdateMonitorResponse
  pHttpStatus_
  pMonitorArn_
  pStatus_ =
    UpdateMonitorResponse'
      { httpStatus = pHttpStatus_,
        monitorArn = pMonitorArn_,
        status = pStatus_
      }

-- | The response's http status code.
updateMonitorResponse_httpStatus :: Lens.Lens' UpdateMonitorResponse Prelude.Int
updateMonitorResponse_httpStatus = Lens.lens (\UpdateMonitorResponse' {httpStatus} -> httpStatus) (\s@UpdateMonitorResponse' {} a -> s {httpStatus = a} :: UpdateMonitorResponse)

-- | The Amazon Resource Name (ARN) of the monitor.
updateMonitorResponse_monitorArn :: Lens.Lens' UpdateMonitorResponse Prelude.Text
updateMonitorResponse_monitorArn = Lens.lens (\UpdateMonitorResponse' {monitorArn} -> monitorArn) (\s@UpdateMonitorResponse' {} a -> s {monitorArn = a} :: UpdateMonitorResponse)

-- | The status of a monitor.
updateMonitorResponse_status :: Lens.Lens' UpdateMonitorResponse MonitorConfigState
updateMonitorResponse_status = Lens.lens (\UpdateMonitorResponse' {status} -> status) (\s@UpdateMonitorResponse' {} a -> s {status = a} :: UpdateMonitorResponse)

instance Prelude.NFData UpdateMonitorResponse where
  rnf UpdateMonitorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf status
