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
-- Module      : Network.AWS.IoT.UpdateCustomMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Device Defender detect custom metric.
module Network.AWS.IoT.UpdateCustomMetric
  ( -- * Creating a Request
    UpdateCustomMetric (..),
    newUpdateCustomMetric,

    -- * Request Lenses
    updateCustomMetric_metricName,
    updateCustomMetric_displayName,

    -- * Destructuring the Response
    UpdateCustomMetricResponse (..),
    newUpdateCustomMetricResponse,

    -- * Response Lenses
    updateCustomMetricResponse_lastModifiedDate,
    updateCustomMetricResponse_metricType,
    updateCustomMetricResponse_metricArn,
    updateCustomMetricResponse_metricName,
    updateCustomMetricResponse_creationDate,
    updateCustomMetricResponse_displayName,
    updateCustomMetricResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCustomMetric' smart constructor.
data UpdateCustomMetric = UpdateCustomMetric'
  { -- | The name of the custom metric. Cannot be updated.
    metricName :: Core.Text,
    -- | Field represents a friendly name in the console for the custom metric,
    -- it doesn\'t have to be unique. Don\'t use this name as the metric
    -- identifier in the device metric report. Can be updated.
    displayName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCustomMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'updateCustomMetric_metricName' - The name of the custom metric. Cannot be updated.
--
-- 'displayName', 'updateCustomMetric_displayName' - Field represents a friendly name in the console for the custom metric,
-- it doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
newUpdateCustomMetric ::
  -- | 'metricName'
  Core.Text ->
  -- | 'displayName'
  Core.Text ->
  UpdateCustomMetric
newUpdateCustomMetric pMetricName_ pDisplayName_ =
  UpdateCustomMetric'
    { metricName = pMetricName_,
      displayName = pDisplayName_
    }

-- | The name of the custom metric. Cannot be updated.
updateCustomMetric_metricName :: Lens.Lens' UpdateCustomMetric Core.Text
updateCustomMetric_metricName = Lens.lens (\UpdateCustomMetric' {metricName} -> metricName) (\s@UpdateCustomMetric' {} a -> s {metricName = a} :: UpdateCustomMetric)

-- | Field represents a friendly name in the console for the custom metric,
-- it doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
updateCustomMetric_displayName :: Lens.Lens' UpdateCustomMetric Core.Text
updateCustomMetric_displayName = Lens.lens (\UpdateCustomMetric' {displayName} -> displayName) (\s@UpdateCustomMetric' {} a -> s {displayName = a} :: UpdateCustomMetric)

instance Core.AWSRequest UpdateCustomMetric where
  type
    AWSResponse UpdateCustomMetric =
      UpdateCustomMetricResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCustomMetricResponse'
            Core.<$> (x Core..?> "lastModifiedDate")
            Core.<*> (x Core..?> "metricType")
            Core.<*> (x Core..?> "metricArn")
            Core.<*> (x Core..?> "metricName")
            Core.<*> (x Core..?> "creationDate")
            Core.<*> (x Core..?> "displayName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCustomMetric

instance Core.NFData UpdateCustomMetric

instance Core.ToHeaders UpdateCustomMetric where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateCustomMetric where
  toJSON UpdateCustomMetric' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("displayName" Core..= displayName)]
      )

instance Core.ToPath UpdateCustomMetric where
  toPath UpdateCustomMetric' {..} =
    Core.mconcat
      ["/custom-metric/", Core.toBS metricName]

instance Core.ToQuery UpdateCustomMetric where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCustomMetricResponse' smart constructor.
data UpdateCustomMetricResponse = UpdateCustomMetricResponse'
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
    -- | A friendly name in the console for the custom metric
    displayName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCustomMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'updateCustomMetricResponse_lastModifiedDate' - The time the custom metric was last modified in milliseconds since
-- epoch.
--
-- 'metricType', 'updateCustomMetricResponse_metricType' - The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
--
-- 'metricArn', 'updateCustomMetricResponse_metricArn' - The Amazon Resource Number (ARN) of the custom metric.
--
-- 'metricName', 'updateCustomMetricResponse_metricName' - The name of the custom metric.
--
-- 'creationDate', 'updateCustomMetricResponse_creationDate' - The creation date of the custom metric in milliseconds since epoch.
--
-- 'displayName', 'updateCustomMetricResponse_displayName' - A friendly name in the console for the custom metric
--
-- 'httpStatus', 'updateCustomMetricResponse_httpStatus' - The response's http status code.
newUpdateCustomMetricResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateCustomMetricResponse
newUpdateCustomMetricResponse pHttpStatus_ =
  UpdateCustomMetricResponse'
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
updateCustomMetricResponse_lastModifiedDate :: Lens.Lens' UpdateCustomMetricResponse (Core.Maybe Core.UTCTime)
updateCustomMetricResponse_lastModifiedDate = Lens.lens (\UpdateCustomMetricResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateCustomMetricResponse' {} a -> s {lastModifiedDate = a} :: UpdateCustomMetricResponse) Core.. Lens.mapping Core._Time

-- | The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
updateCustomMetricResponse_metricType :: Lens.Lens' UpdateCustomMetricResponse (Core.Maybe CustomMetricType)
updateCustomMetricResponse_metricType = Lens.lens (\UpdateCustomMetricResponse' {metricType} -> metricType) (\s@UpdateCustomMetricResponse' {} a -> s {metricType = a} :: UpdateCustomMetricResponse)

-- | The Amazon Resource Number (ARN) of the custom metric.
updateCustomMetricResponse_metricArn :: Lens.Lens' UpdateCustomMetricResponse (Core.Maybe Core.Text)
updateCustomMetricResponse_metricArn = Lens.lens (\UpdateCustomMetricResponse' {metricArn} -> metricArn) (\s@UpdateCustomMetricResponse' {} a -> s {metricArn = a} :: UpdateCustomMetricResponse)

-- | The name of the custom metric.
updateCustomMetricResponse_metricName :: Lens.Lens' UpdateCustomMetricResponse (Core.Maybe Core.Text)
updateCustomMetricResponse_metricName = Lens.lens (\UpdateCustomMetricResponse' {metricName} -> metricName) (\s@UpdateCustomMetricResponse' {} a -> s {metricName = a} :: UpdateCustomMetricResponse)

-- | The creation date of the custom metric in milliseconds since epoch.
updateCustomMetricResponse_creationDate :: Lens.Lens' UpdateCustomMetricResponse (Core.Maybe Core.UTCTime)
updateCustomMetricResponse_creationDate = Lens.lens (\UpdateCustomMetricResponse' {creationDate} -> creationDate) (\s@UpdateCustomMetricResponse' {} a -> s {creationDate = a} :: UpdateCustomMetricResponse) Core.. Lens.mapping Core._Time

-- | A friendly name in the console for the custom metric
updateCustomMetricResponse_displayName :: Lens.Lens' UpdateCustomMetricResponse (Core.Maybe Core.Text)
updateCustomMetricResponse_displayName = Lens.lens (\UpdateCustomMetricResponse' {displayName} -> displayName) (\s@UpdateCustomMetricResponse' {} a -> s {displayName = a} :: UpdateCustomMetricResponse)

-- | The response's http status code.
updateCustomMetricResponse_httpStatus :: Lens.Lens' UpdateCustomMetricResponse Core.Int
updateCustomMetricResponse_httpStatus = Lens.lens (\UpdateCustomMetricResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomMetricResponse' {} a -> s {httpStatus = a} :: UpdateCustomMetricResponse)

instance Core.NFData UpdateCustomMetricResponse
