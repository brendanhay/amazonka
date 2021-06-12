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
-- Module      : Network.AWS.IoT.DescribeCustomMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender detect custom metric.
module Network.AWS.IoT.DescribeCustomMetric
  ( -- * Creating a Request
    DescribeCustomMetric (..),
    newDescribeCustomMetric,

    -- * Request Lenses
    describeCustomMetric_metricName,

    -- * Destructuring the Response
    DescribeCustomMetricResponse (..),
    newDescribeCustomMetricResponse,

    -- * Response Lenses
    describeCustomMetricResponse_lastModifiedDate,
    describeCustomMetricResponse_metricType,
    describeCustomMetricResponse_metricArn,
    describeCustomMetricResponse_metricName,
    describeCustomMetricResponse_creationDate,
    describeCustomMetricResponse_displayName,
    describeCustomMetricResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCustomMetric' smart constructor.
data DescribeCustomMetric = DescribeCustomMetric'
  { -- | The name of the custom metric.
    metricName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeCustomMetric
newDescribeCustomMetric pMetricName_ =
  DescribeCustomMetric' {metricName = pMetricName_}

-- | The name of the custom metric.
describeCustomMetric_metricName :: Lens.Lens' DescribeCustomMetric Core.Text
describeCustomMetric_metricName = Lens.lens (\DescribeCustomMetric' {metricName} -> metricName) (\s@DescribeCustomMetric' {} a -> s {metricName = a} :: DescribeCustomMetric)

instance Core.AWSRequest DescribeCustomMetric where
  type
    AWSResponse DescribeCustomMetric =
      DescribeCustomMetricResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomMetricResponse'
            Core.<$> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "metricType")
            Core.<*> (x Core..?> "metricArn")
            Core.<*> (x Core..?> "metricName")
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "displayName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCustomMetric

instance Core.NFData DescribeCustomMetric

instance Core.ToHeaders DescribeCustomMetric where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCustomMetric where
  toPath DescribeCustomMetric' {..} =
    Core.mconcat
      ["/custom-metric/", Core.toBS metricName]

instance Core.ToQuery DescribeCustomMetric where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCustomMetricResponse' smart constructor.
data DescribeCustomMetricResponse = DescribeCustomMetricResponse'
  { -- | The time the custom metric was last modified in milliseconds since
    -- epoch.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The type of the custom metric. Types include @string-list@,
    -- @ip-address-list@, @number-list@, and @number@.
    metricType :: Core.Maybe CustomMetricType,
    -- | The Amazon Resource Number (ARN) of the custom metric.
    metricArn :: Core.Maybe Core.Text,
    -- | The name of the custom metric.
    metricName :: Core.Maybe Core.Text,
    -- | The creation date of the custom metric in milliseconds since epoch.
    creationDate :: Core.Maybe Core.POSIX,
    -- | Field represents a friendly name in the console for the custom metric;
    -- doesn\'t have to be unique. Don\'t use this name as the metric
    -- identifier in the device metric report. Can be updated.
    displayName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCustomMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'describeCustomMetricResponse_lastModifiedDate' - The time the custom metric was last modified in milliseconds since
-- epoch.
--
-- 'metricType', 'describeCustomMetricResponse_metricType' - The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
--
-- 'metricArn', 'describeCustomMetricResponse_metricArn' - The Amazon Resource Number (ARN) of the custom metric.
--
-- 'metricName', 'describeCustomMetricResponse_metricName' - The name of the custom metric.
--
-- 'creationDate', 'describeCustomMetricResponse_creationDate' - The creation date of the custom metric in milliseconds since epoch.
--
-- 'displayName', 'describeCustomMetricResponse_displayName' - Field represents a friendly name in the console for the custom metric;
-- doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
--
-- 'httpStatus', 'describeCustomMetricResponse_httpStatus' - The response's http status code.
newDescribeCustomMetricResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCustomMetricResponse
newDescribeCustomMetricResponse pHttpStatus_ =
  DescribeCustomMetricResponse'
    { lastModifiedDate =
        Core.Nothing,
      metricType = Core.Nothing,
      metricArn = Core.Nothing,
      metricName = Core.Nothing,
      creationDate = Core.Nothing,
      displayName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the custom metric was last modified in milliseconds since
-- epoch.
describeCustomMetricResponse_lastModifiedDate :: Lens.Lens' DescribeCustomMetricResponse (Core.Maybe Core.UTCTime)
describeCustomMetricResponse_lastModifiedDate = Lens.lens (\DescribeCustomMetricResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeCustomMetricResponse' {} a -> s {lastModifiedDate = a} :: DescribeCustomMetricResponse) Core.. Lens.mapping Core._Time

-- | The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
describeCustomMetricResponse_metricType :: Lens.Lens' DescribeCustomMetricResponse (Core.Maybe CustomMetricType)
describeCustomMetricResponse_metricType = Lens.lens (\DescribeCustomMetricResponse' {metricType} -> metricType) (\s@DescribeCustomMetricResponse' {} a -> s {metricType = a} :: DescribeCustomMetricResponse)

-- | The Amazon Resource Number (ARN) of the custom metric.
describeCustomMetricResponse_metricArn :: Lens.Lens' DescribeCustomMetricResponse (Core.Maybe Core.Text)
describeCustomMetricResponse_metricArn = Lens.lens (\DescribeCustomMetricResponse' {metricArn} -> metricArn) (\s@DescribeCustomMetricResponse' {} a -> s {metricArn = a} :: DescribeCustomMetricResponse)

-- | The name of the custom metric.
describeCustomMetricResponse_metricName :: Lens.Lens' DescribeCustomMetricResponse (Core.Maybe Core.Text)
describeCustomMetricResponse_metricName = Lens.lens (\DescribeCustomMetricResponse' {metricName} -> metricName) (\s@DescribeCustomMetricResponse' {} a -> s {metricName = a} :: DescribeCustomMetricResponse)

-- | The creation date of the custom metric in milliseconds since epoch.
describeCustomMetricResponse_creationDate :: Lens.Lens' DescribeCustomMetricResponse (Core.Maybe Core.UTCTime)
describeCustomMetricResponse_creationDate = Lens.lens (\DescribeCustomMetricResponse' {creationDate} -> creationDate) (\s@DescribeCustomMetricResponse' {} a -> s {creationDate = a} :: DescribeCustomMetricResponse) Core.. Lens.mapping Core._Time

-- | Field represents a friendly name in the console for the custom metric;
-- doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
describeCustomMetricResponse_displayName :: Lens.Lens' DescribeCustomMetricResponse (Core.Maybe Core.Text)
describeCustomMetricResponse_displayName = Lens.lens (\DescribeCustomMetricResponse' {displayName} -> displayName) (\s@DescribeCustomMetricResponse' {} a -> s {displayName = a} :: DescribeCustomMetricResponse)

-- | The response's http status code.
describeCustomMetricResponse_httpStatus :: Lens.Lens' DescribeCustomMetricResponse Core.Int
describeCustomMetricResponse_httpStatus = Lens.lens (\DescribeCustomMetricResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomMetricResponse' {} a -> s {httpStatus = a} :: DescribeCustomMetricResponse)

instance Core.NFData DescribeCustomMetricResponse
