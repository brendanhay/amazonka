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
-- Module      : Amazonka.InternetMonitor.CreateMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a monitor in Amazon CloudWatch Internet Monitor. A monitor is
-- built based on information from the application resources that you add:
-- Amazon Virtual Private Clouds (VPCs), Amazon CloudFront distributions,
-- and WorkSpaces directories. Internet Monitor then publishes internet
-- measurements from Amazon Web Services that are specific to the
-- /city-networks/, that is, the locations and ASNs (typically internet
-- service providers or ISPs), where clients access your application. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-InternetMonitor.html Using Amazon CloudWatch Internet Monitor>
-- in the /Amazon CloudWatch User Guide/.
--
-- When you create a monitor, you set a maximum limit for the number of
-- city-networks where client traffic is monitored. The city-network
-- maximum that you choose is the limit, but you only pay for the number of
-- city-networks that are actually monitored. You can change the maximum at
-- any time by updating your monitor. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/IMCityNetworksMaximum.html Choosing a city-network maximum value>
-- in the /Amazon CloudWatch User Guide/.
module Amazonka.InternetMonitor.CreateMonitor
  ( -- * Creating a Request
    CreateMonitor (..),
    newCreateMonitor,

    -- * Request Lenses
    createMonitor_clientToken,
    createMonitor_internetMeasurementsLogDelivery,
    createMonitor_maxCityNetworksToMonitor,
    createMonitor_resources,
    createMonitor_tags,
    createMonitor_trafficPercentageToMonitor,
    createMonitor_monitorName,

    -- * Destructuring the Response
    CreateMonitorResponse (..),
    newCreateMonitorResponse,

    -- * Response Lenses
    createMonitorResponse_httpStatus,
    createMonitorResponse_arn,
    createMonitorResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMonitor' smart constructor.
data CreateMonitor = CreateMonitor'
  { -- | A unique, case-sensitive string of up to 64 ASCII characters that you
    -- specify to make an idempotent API request. Don\'t reuse the same client
    -- token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Publish internet measurements for Internet Monitor to an Amazon S3
    -- bucket in addition to CloudWatch Logs.
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
    -- | The resources to include in a monitor, which you provide as a set of
    -- Amazon Resource Names (ARNs).
    --
    -- You can add a combination of Amazon Virtual Private Clouds (VPCs) and
    -- Amazon CloudFront distributions, or you can add Amazon WorkSpaces
    -- directories. You can\'t add all three types of resources.
    --
    -- If you add only VPC resources, at least one VPC must have an Internet
    -- Gateway attached to it, to make sure that it has internet connectivity.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The tags for a monitor. You can add a maximum of 50 tags in Internet
    -- Monitor.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The percentage of the internet-facing traffic for your application that
    -- you want to monitor with this monitor.
    trafficPercentageToMonitor :: Prelude.Maybe Prelude.Natural,
    -- | The name of the monitor.
    monitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createMonitor_clientToken' - A unique, case-sensitive string of up to 64 ASCII characters that you
-- specify to make an idempotent API request. Don\'t reuse the same client
-- token for other API requests.
--
-- 'internetMeasurementsLogDelivery', 'createMonitor_internetMeasurementsLogDelivery' - Publish internet measurements for Internet Monitor to an Amazon S3
-- bucket in addition to CloudWatch Logs.
--
-- 'maxCityNetworksToMonitor', 'createMonitor_maxCityNetworksToMonitor' - The maximum number of city-networks to monitor for your resources. A
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
-- 'resources', 'createMonitor_resources' - The resources to include in a monitor, which you provide as a set of
-- Amazon Resource Names (ARNs).
--
-- You can add a combination of Amazon Virtual Private Clouds (VPCs) and
-- Amazon CloudFront distributions, or you can add Amazon WorkSpaces
-- directories. You can\'t add all three types of resources.
--
-- If you add only VPC resources, at least one VPC must have an Internet
-- Gateway attached to it, to make sure that it has internet connectivity.
--
-- 'tags', 'createMonitor_tags' - The tags for a monitor. You can add a maximum of 50 tags in Internet
-- Monitor.
--
-- 'trafficPercentageToMonitor', 'createMonitor_trafficPercentageToMonitor' - The percentage of the internet-facing traffic for your application that
-- you want to monitor with this monitor.
--
-- 'monitorName', 'createMonitor_monitorName' - The name of the monitor.
newCreateMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  CreateMonitor
newCreateMonitor pMonitorName_ =
  CreateMonitor'
    { clientToken = Prelude.Nothing,
      internetMeasurementsLogDelivery = Prelude.Nothing,
      maxCityNetworksToMonitor = Prelude.Nothing,
      resources = Prelude.Nothing,
      tags = Prelude.Nothing,
      trafficPercentageToMonitor = Prelude.Nothing,
      monitorName = pMonitorName_
    }

-- | A unique, case-sensitive string of up to 64 ASCII characters that you
-- specify to make an idempotent API request. Don\'t reuse the same client
-- token for other API requests.
createMonitor_clientToken :: Lens.Lens' CreateMonitor (Prelude.Maybe Prelude.Text)
createMonitor_clientToken = Lens.lens (\CreateMonitor' {clientToken} -> clientToken) (\s@CreateMonitor' {} a -> s {clientToken = a} :: CreateMonitor)

-- | Publish internet measurements for Internet Monitor to an Amazon S3
-- bucket in addition to CloudWatch Logs.
createMonitor_internetMeasurementsLogDelivery :: Lens.Lens' CreateMonitor (Prelude.Maybe InternetMeasurementsLogDelivery)
createMonitor_internetMeasurementsLogDelivery = Lens.lens (\CreateMonitor' {internetMeasurementsLogDelivery} -> internetMeasurementsLogDelivery) (\s@CreateMonitor' {} a -> s {internetMeasurementsLogDelivery = a} :: CreateMonitor)

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
createMonitor_maxCityNetworksToMonitor :: Lens.Lens' CreateMonitor (Prelude.Maybe Prelude.Natural)
createMonitor_maxCityNetworksToMonitor = Lens.lens (\CreateMonitor' {maxCityNetworksToMonitor} -> maxCityNetworksToMonitor) (\s@CreateMonitor' {} a -> s {maxCityNetworksToMonitor = a} :: CreateMonitor)

-- | The resources to include in a monitor, which you provide as a set of
-- Amazon Resource Names (ARNs).
--
-- You can add a combination of Amazon Virtual Private Clouds (VPCs) and
-- Amazon CloudFront distributions, or you can add Amazon WorkSpaces
-- directories. You can\'t add all three types of resources.
--
-- If you add only VPC resources, at least one VPC must have an Internet
-- Gateway attached to it, to make sure that it has internet connectivity.
createMonitor_resources :: Lens.Lens' CreateMonitor (Prelude.Maybe [Prelude.Text])
createMonitor_resources = Lens.lens (\CreateMonitor' {resources} -> resources) (\s@CreateMonitor' {} a -> s {resources = a} :: CreateMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The tags for a monitor. You can add a maximum of 50 tags in Internet
-- Monitor.
createMonitor_tags :: Lens.Lens' CreateMonitor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMonitor_tags = Lens.lens (\CreateMonitor' {tags} -> tags) (\s@CreateMonitor' {} a -> s {tags = a} :: CreateMonitor) Prelude.. Lens.mapping Lens.coerced

-- | The percentage of the internet-facing traffic for your application that
-- you want to monitor with this monitor.
createMonitor_trafficPercentageToMonitor :: Lens.Lens' CreateMonitor (Prelude.Maybe Prelude.Natural)
createMonitor_trafficPercentageToMonitor = Lens.lens (\CreateMonitor' {trafficPercentageToMonitor} -> trafficPercentageToMonitor) (\s@CreateMonitor' {} a -> s {trafficPercentageToMonitor = a} :: CreateMonitor)

-- | The name of the monitor.
createMonitor_monitorName :: Lens.Lens' CreateMonitor Prelude.Text
createMonitor_monitorName = Lens.lens (\CreateMonitor' {monitorName} -> monitorName) (\s@CreateMonitor' {} a -> s {monitorName = a} :: CreateMonitor)

instance Core.AWSRequest CreateMonitor where
  type
    AWSResponse CreateMonitor =
      CreateMonitorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable CreateMonitor where
  hashWithSalt _salt CreateMonitor' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` internetMeasurementsLogDelivery
      `Prelude.hashWithSalt` maxCityNetworksToMonitor
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trafficPercentageToMonitor
      `Prelude.hashWithSalt` monitorName

instance Prelude.NFData CreateMonitor where
  rnf CreateMonitor' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf internetMeasurementsLogDelivery
      `Prelude.seq` Prelude.rnf maxCityNetworksToMonitor
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trafficPercentageToMonitor
      `Prelude.seq` Prelude.rnf monitorName

instance Data.ToHeaders CreateMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMonitor where
  toJSON CreateMonitor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("InternetMeasurementsLogDelivery" Data..=)
              Prelude.<$> internetMeasurementsLogDelivery,
            ("MaxCityNetworksToMonitor" Data..=)
              Prelude.<$> maxCityNetworksToMonitor,
            ("Resources" Data..=) Prelude.<$> resources,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TrafficPercentageToMonitor" Data..=)
              Prelude.<$> trafficPercentageToMonitor,
            Prelude.Just ("MonitorName" Data..= monitorName)
          ]
      )

instance Data.ToPath CreateMonitor where
  toPath = Prelude.const "/v20210603/Monitors"

instance Data.ToQuery CreateMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMonitorResponse' smart constructor.
data CreateMonitorResponse = CreateMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitor.
    arn :: Prelude.Text,
    -- | The status of a monitor.
    status :: MonitorConfigState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMonitorResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createMonitorResponse_arn' - The Amazon Resource Name (ARN) of the monitor.
--
-- 'status', 'createMonitorResponse_status' - The status of a monitor.
newCreateMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  MonitorConfigState ->
  CreateMonitorResponse
newCreateMonitorResponse pHttpStatus_ pArn_ pStatus_ =
  CreateMonitorResponse'
    { httpStatus = pHttpStatus_,
      arn = pArn_,
      status = pStatus_
    }

-- | The response's http status code.
createMonitorResponse_httpStatus :: Lens.Lens' CreateMonitorResponse Prelude.Int
createMonitorResponse_httpStatus = Lens.lens (\CreateMonitorResponse' {httpStatus} -> httpStatus) (\s@CreateMonitorResponse' {} a -> s {httpStatus = a} :: CreateMonitorResponse)

-- | The Amazon Resource Name (ARN) of the monitor.
createMonitorResponse_arn :: Lens.Lens' CreateMonitorResponse Prelude.Text
createMonitorResponse_arn = Lens.lens (\CreateMonitorResponse' {arn} -> arn) (\s@CreateMonitorResponse' {} a -> s {arn = a} :: CreateMonitorResponse)

-- | The status of a monitor.
createMonitorResponse_status :: Lens.Lens' CreateMonitorResponse MonitorConfigState
createMonitorResponse_status = Lens.lens (\CreateMonitorResponse' {status} -> status) (\s@CreateMonitorResponse' {} a -> s {status = a} :: CreateMonitorResponse)

instance Prelude.NFData CreateMonitorResponse where
  rnf CreateMonitorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
