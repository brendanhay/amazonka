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
-- Module      : Amazonka.IoT.DescribeCustomMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender detect custom metric.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeCustomMetric>
-- action.
module Amazonka.IoT.DescribeCustomMetric
  ( -- * Creating a Request
    DescribeCustomMetric (..),
    newDescribeCustomMetric,

    -- * Request Lenses
    describeCustomMetric_metricName,

    -- * Destructuring the Response
    DescribeCustomMetricResponse (..),
    newDescribeCustomMetricResponse,

    -- * Response Lenses
    describeCustomMetricResponse_metricArn,
    describeCustomMetricResponse_lastModifiedDate,
    describeCustomMetricResponse_displayName,
    describeCustomMetricResponse_creationDate,
    describeCustomMetricResponse_metricType,
    describeCustomMetricResponse_metricName,
    describeCustomMetricResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomMetric' smart constructor.
data DescribeCustomMetric = DescribeCustomMetric'
  { -- | The name of the custom metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'describeCustomMetric_metricName' - The name of the custom metric.
newDescribeCustomMetric ::
  -- | 'metricName'
  Prelude.Text ->
  DescribeCustomMetric
newDescribeCustomMetric pMetricName_ =
  DescribeCustomMetric' {metricName = pMetricName_}

-- | The name of the custom metric.
describeCustomMetric_metricName :: Lens.Lens' DescribeCustomMetric Prelude.Text
describeCustomMetric_metricName = Lens.lens (\DescribeCustomMetric' {metricName} -> metricName) (\s@DescribeCustomMetric' {} a -> s {metricName = a} :: DescribeCustomMetric)

instance Core.AWSRequest DescribeCustomMetric where
  type
    AWSResponse DescribeCustomMetric =
      DescribeCustomMetricResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomMetricResponse'
            Prelude.<$> (x Data..?> "metricArn")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "displayName")
            Prelude.<*> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "metricType")
            Prelude.<*> (x Data..?> "metricName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomMetric where
  hashWithSalt _salt DescribeCustomMetric' {..} =
    _salt `Prelude.hashWithSalt` metricName

instance Prelude.NFData DescribeCustomMetric where
  rnf DescribeCustomMetric' {..} =
    Prelude.rnf metricName

instance Data.ToHeaders DescribeCustomMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCustomMetric where
  toPath DescribeCustomMetric' {..} =
    Prelude.mconcat
      ["/custom-metric/", Data.toBS metricName]

instance Data.ToQuery DescribeCustomMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomMetricResponse' smart constructor.
data DescribeCustomMetricResponse = DescribeCustomMetricResponse'
  { -- | The Amazon Resource Number (ARN) of the custom metric.
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The time the custom metric was last modified in milliseconds since
    -- epoch.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Field represents a friendly name in the console for the custom metric;
    -- doesn\'t have to be unique. Don\'t use this name as the metric
    -- identifier in the device metric report. Can be updated.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The creation date of the custom metric in milliseconds since epoch.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The type of the custom metric.
    --
    -- The type @number@ only takes a single metric value as an input, but
    -- while submitting the metrics value in the DeviceMetrics report, it must
    -- be passed as an array with a single value.
    metricType :: Prelude.Maybe CustomMetricType,
    -- | The name of the custom metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricArn', 'describeCustomMetricResponse_metricArn' - The Amazon Resource Number (ARN) of the custom metric.
--
-- 'lastModifiedDate', 'describeCustomMetricResponse_lastModifiedDate' - The time the custom metric was last modified in milliseconds since
-- epoch.
--
-- 'displayName', 'describeCustomMetricResponse_displayName' - Field represents a friendly name in the console for the custom metric;
-- doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
--
-- 'creationDate', 'describeCustomMetricResponse_creationDate' - The creation date of the custom metric in milliseconds since epoch.
--
-- 'metricType', 'describeCustomMetricResponse_metricType' - The type of the custom metric.
--
-- The type @number@ only takes a single metric value as an input, but
-- while submitting the metrics value in the DeviceMetrics report, it must
-- be passed as an array with a single value.
--
-- 'metricName', 'describeCustomMetricResponse_metricName' - The name of the custom metric.
--
-- 'httpStatus', 'describeCustomMetricResponse_httpStatus' - The response's http status code.
newDescribeCustomMetricResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomMetricResponse
newDescribeCustomMetricResponse pHttpStatus_ =
  DescribeCustomMetricResponse'
    { metricArn =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      displayName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      metricType = Prelude.Nothing,
      metricName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the custom metric.
describeCustomMetricResponse_metricArn :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.Text)
describeCustomMetricResponse_metricArn = Lens.lens (\DescribeCustomMetricResponse' {metricArn} -> metricArn) (\s@DescribeCustomMetricResponse' {} a -> s {metricArn = a} :: DescribeCustomMetricResponse)

-- | The time the custom metric was last modified in milliseconds since
-- epoch.
describeCustomMetricResponse_lastModifiedDate :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomMetricResponse_lastModifiedDate = Lens.lens (\DescribeCustomMetricResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeCustomMetricResponse' {} a -> s {lastModifiedDate = a} :: DescribeCustomMetricResponse) Prelude.. Lens.mapping Data._Time

-- | Field represents a friendly name in the console for the custom metric;
-- doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
describeCustomMetricResponse_displayName :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.Text)
describeCustomMetricResponse_displayName = Lens.lens (\DescribeCustomMetricResponse' {displayName} -> displayName) (\s@DescribeCustomMetricResponse' {} a -> s {displayName = a} :: DescribeCustomMetricResponse)

-- | The creation date of the custom metric in milliseconds since epoch.
describeCustomMetricResponse_creationDate :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomMetricResponse_creationDate = Lens.lens (\DescribeCustomMetricResponse' {creationDate} -> creationDate) (\s@DescribeCustomMetricResponse' {} a -> s {creationDate = a} :: DescribeCustomMetricResponse) Prelude.. Lens.mapping Data._Time

-- | The type of the custom metric.
--
-- The type @number@ only takes a single metric value as an input, but
-- while submitting the metrics value in the DeviceMetrics report, it must
-- be passed as an array with a single value.
describeCustomMetricResponse_metricType :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe CustomMetricType)
describeCustomMetricResponse_metricType = Lens.lens (\DescribeCustomMetricResponse' {metricType} -> metricType) (\s@DescribeCustomMetricResponse' {} a -> s {metricType = a} :: DescribeCustomMetricResponse)

-- | The name of the custom metric.
describeCustomMetricResponse_metricName :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.Text)
describeCustomMetricResponse_metricName = Lens.lens (\DescribeCustomMetricResponse' {metricName} -> metricName) (\s@DescribeCustomMetricResponse' {} a -> s {metricName = a} :: DescribeCustomMetricResponse)

-- | The response's http status code.
describeCustomMetricResponse_httpStatus :: Lens.Lens' DescribeCustomMetricResponse Prelude.Int
describeCustomMetricResponse_httpStatus = Lens.lens (\DescribeCustomMetricResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomMetricResponse' {} a -> s {httpStatus = a} :: DescribeCustomMetricResponse)

instance Prelude.NFData DescribeCustomMetricResponse where
  rnf DescribeCustomMetricResponse' {..} =
    Prelude.rnf metricArn
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf metricType
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf httpStatus
