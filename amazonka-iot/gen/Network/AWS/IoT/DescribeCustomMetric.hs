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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCustomMetric' smart constructor.
data DescribeCustomMetric = DescribeCustomMetric'
  { -- | The name of the custom metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeCustomMetric where
  type
    Rs DescribeCustomMetric =
      DescribeCustomMetricResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomMetricResponse'
            Prelude.<$> (x Prelude..?> "lastModifiedDate")
            Prelude.<*> (x Prelude..?> "metricType")
            Prelude.<*> (x Prelude..?> "metricArn")
            Prelude.<*> (x Prelude..?> "metricName")
            Prelude.<*> (x Prelude..?> "creationDate")
            Prelude.<*> (x Prelude..?> "displayName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCustomMetric

instance Prelude.NFData DescribeCustomMetric

instance Prelude.ToHeaders DescribeCustomMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeCustomMetric where
  toPath DescribeCustomMetric' {..} =
    Prelude.mconcat
      ["/custom-metric/", Prelude.toBS metricName]

instance Prelude.ToQuery DescribeCustomMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomMetricResponse' smart constructor.
data DescribeCustomMetricResponse = DescribeCustomMetricResponse'
  { -- | The time the custom metric was last modified in milliseconds since
    -- epoch.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The type of the custom metric. Types include @string-list@,
    -- @ip-address-list@, @number-list@, and @number@.
    metricType :: Prelude.Maybe CustomMetricType,
    -- | The Amazon Resource Number (ARN) of the custom metric.
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The creation date of the custom metric in milliseconds since epoch.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | Field represents a friendly name in the console for the custom metric;
    -- doesn\'t have to be unique. Don\'t use this name as the metric
    -- identifier in the device metric report. Can be updated.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeCustomMetricResponse
newDescribeCustomMetricResponse pHttpStatus_ =
  DescribeCustomMetricResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      metricType = Prelude.Nothing,
      metricArn = Prelude.Nothing,
      metricName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      displayName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time the custom metric was last modified in milliseconds since
-- epoch.
describeCustomMetricResponse_lastModifiedDate :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomMetricResponse_lastModifiedDate = Lens.lens (\DescribeCustomMetricResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeCustomMetricResponse' {} a -> s {lastModifiedDate = a} :: DescribeCustomMetricResponse) Prelude.. Lens.mapping Prelude._Time

-- | The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
describeCustomMetricResponse_metricType :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe CustomMetricType)
describeCustomMetricResponse_metricType = Lens.lens (\DescribeCustomMetricResponse' {metricType} -> metricType) (\s@DescribeCustomMetricResponse' {} a -> s {metricType = a} :: DescribeCustomMetricResponse)

-- | The Amazon Resource Number (ARN) of the custom metric.
describeCustomMetricResponse_metricArn :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.Text)
describeCustomMetricResponse_metricArn = Lens.lens (\DescribeCustomMetricResponse' {metricArn} -> metricArn) (\s@DescribeCustomMetricResponse' {} a -> s {metricArn = a} :: DescribeCustomMetricResponse)

-- | The name of the custom metric.
describeCustomMetricResponse_metricName :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.Text)
describeCustomMetricResponse_metricName = Lens.lens (\DescribeCustomMetricResponse' {metricName} -> metricName) (\s@DescribeCustomMetricResponse' {} a -> s {metricName = a} :: DescribeCustomMetricResponse)

-- | The creation date of the custom metric in milliseconds since epoch.
describeCustomMetricResponse_creationDate :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.UTCTime)
describeCustomMetricResponse_creationDate = Lens.lens (\DescribeCustomMetricResponse' {creationDate} -> creationDate) (\s@DescribeCustomMetricResponse' {} a -> s {creationDate = a} :: DescribeCustomMetricResponse) Prelude.. Lens.mapping Prelude._Time

-- | Field represents a friendly name in the console for the custom metric;
-- doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
describeCustomMetricResponse_displayName :: Lens.Lens' DescribeCustomMetricResponse (Prelude.Maybe Prelude.Text)
describeCustomMetricResponse_displayName = Lens.lens (\DescribeCustomMetricResponse' {displayName} -> displayName) (\s@DescribeCustomMetricResponse' {} a -> s {displayName = a} :: DescribeCustomMetricResponse)

-- | The response's http status code.
describeCustomMetricResponse_httpStatus :: Lens.Lens' DescribeCustomMetricResponse Prelude.Int
describeCustomMetricResponse_httpStatus = Lens.lens (\DescribeCustomMetricResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomMetricResponse' {} a -> s {httpStatus = a} :: DescribeCustomMetricResponse)

instance Prelude.NFData DescribeCustomMetricResponse
