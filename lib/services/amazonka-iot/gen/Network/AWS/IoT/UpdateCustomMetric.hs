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
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateCustomMetric>
-- action.
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
    updateCustomMetricResponse_metricType,
    updateCustomMetricResponse_lastModifiedDate,
    updateCustomMetricResponse_metricName,
    updateCustomMetricResponse_displayName,
    updateCustomMetricResponse_creationDate,
    updateCustomMetricResponse_metricArn,
    updateCustomMetricResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCustomMetric' smart constructor.
data UpdateCustomMetric = UpdateCustomMetric'
  { -- | The name of the custom metric. Cannot be updated.
    metricName :: Prelude.Text,
    -- | Field represents a friendly name in the console for the custom metric,
    -- it doesn\'t have to be unique. Don\'t use this name as the metric
    -- identifier in the device metric report. Can be updated.
    displayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  UpdateCustomMetric
newUpdateCustomMetric pMetricName_ pDisplayName_ =
  UpdateCustomMetric'
    { metricName = pMetricName_,
      displayName = pDisplayName_
    }

-- | The name of the custom metric. Cannot be updated.
updateCustomMetric_metricName :: Lens.Lens' UpdateCustomMetric Prelude.Text
updateCustomMetric_metricName = Lens.lens (\UpdateCustomMetric' {metricName} -> metricName) (\s@UpdateCustomMetric' {} a -> s {metricName = a} :: UpdateCustomMetric)

-- | Field represents a friendly name in the console for the custom metric,
-- it doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated.
updateCustomMetric_displayName :: Lens.Lens' UpdateCustomMetric Prelude.Text
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
            Prelude.<$> (x Core..?> "metricType")
            Prelude.<*> (x Core..?> "lastModifiedDate")
            Prelude.<*> (x Core..?> "metricName")
            Prelude.<*> (x Core..?> "displayName")
            Prelude.<*> (x Core..?> "creationDate")
            Prelude.<*> (x Core..?> "metricArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCustomMetric

instance Prelude.NFData UpdateCustomMetric

instance Core.ToHeaders UpdateCustomMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateCustomMetric where
  toJSON UpdateCustomMetric' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("displayName" Core..= displayName)]
      )

instance Core.ToPath UpdateCustomMetric where
  toPath UpdateCustomMetric' {..} =
    Prelude.mconcat
      ["/custom-metric/", Core.toBS metricName]

instance Core.ToQuery UpdateCustomMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCustomMetricResponse' smart constructor.
data UpdateCustomMetricResponse = UpdateCustomMetricResponse'
  { -- | The type of the custom metric. Types include @string-list@,
    -- @ip-address-list@, @number-list@, and @number@.
    metricType :: Prelude.Maybe CustomMetricType,
    -- | The time the custom metric was last modified in milliseconds since
    -- epoch.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the custom metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | A friendly name in the console for the custom metric
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The creation date of the custom metric in milliseconds since epoch.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Number (ARN) of the custom metric.
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricType', 'updateCustomMetricResponse_metricType' - The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
--
-- 'lastModifiedDate', 'updateCustomMetricResponse_lastModifiedDate' - The time the custom metric was last modified in milliseconds since
-- epoch.
--
-- 'metricName', 'updateCustomMetricResponse_metricName' - The name of the custom metric.
--
-- 'displayName', 'updateCustomMetricResponse_displayName' - A friendly name in the console for the custom metric
--
-- 'creationDate', 'updateCustomMetricResponse_creationDate' - The creation date of the custom metric in milliseconds since epoch.
--
-- 'metricArn', 'updateCustomMetricResponse_metricArn' - The Amazon Resource Number (ARN) of the custom metric.
--
-- 'httpStatus', 'updateCustomMetricResponse_httpStatus' - The response's http status code.
newUpdateCustomMetricResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomMetricResponse
newUpdateCustomMetricResponse pHttpStatus_ =
  UpdateCustomMetricResponse'
    { metricType =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      metricName = Prelude.Nothing,
      displayName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      metricArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
updateCustomMetricResponse_metricType :: Lens.Lens' UpdateCustomMetricResponse (Prelude.Maybe CustomMetricType)
updateCustomMetricResponse_metricType = Lens.lens (\UpdateCustomMetricResponse' {metricType} -> metricType) (\s@UpdateCustomMetricResponse' {} a -> s {metricType = a} :: UpdateCustomMetricResponse)

-- | The time the custom metric was last modified in milliseconds since
-- epoch.
updateCustomMetricResponse_lastModifiedDate :: Lens.Lens' UpdateCustomMetricResponse (Prelude.Maybe Prelude.UTCTime)
updateCustomMetricResponse_lastModifiedDate = Lens.lens (\UpdateCustomMetricResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateCustomMetricResponse' {} a -> s {lastModifiedDate = a} :: UpdateCustomMetricResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the custom metric.
updateCustomMetricResponse_metricName :: Lens.Lens' UpdateCustomMetricResponse (Prelude.Maybe Prelude.Text)
updateCustomMetricResponse_metricName = Lens.lens (\UpdateCustomMetricResponse' {metricName} -> metricName) (\s@UpdateCustomMetricResponse' {} a -> s {metricName = a} :: UpdateCustomMetricResponse)

-- | A friendly name in the console for the custom metric
updateCustomMetricResponse_displayName :: Lens.Lens' UpdateCustomMetricResponse (Prelude.Maybe Prelude.Text)
updateCustomMetricResponse_displayName = Lens.lens (\UpdateCustomMetricResponse' {displayName} -> displayName) (\s@UpdateCustomMetricResponse' {} a -> s {displayName = a} :: UpdateCustomMetricResponse)

-- | The creation date of the custom metric in milliseconds since epoch.
updateCustomMetricResponse_creationDate :: Lens.Lens' UpdateCustomMetricResponse (Prelude.Maybe Prelude.UTCTime)
updateCustomMetricResponse_creationDate = Lens.lens (\UpdateCustomMetricResponse' {creationDate} -> creationDate) (\s@UpdateCustomMetricResponse' {} a -> s {creationDate = a} :: UpdateCustomMetricResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Number (ARN) of the custom metric.
updateCustomMetricResponse_metricArn :: Lens.Lens' UpdateCustomMetricResponse (Prelude.Maybe Prelude.Text)
updateCustomMetricResponse_metricArn = Lens.lens (\UpdateCustomMetricResponse' {metricArn} -> metricArn) (\s@UpdateCustomMetricResponse' {} a -> s {metricArn = a} :: UpdateCustomMetricResponse)

-- | The response's http status code.
updateCustomMetricResponse_httpStatus :: Lens.Lens' UpdateCustomMetricResponse Prelude.Int
updateCustomMetricResponse_httpStatus = Lens.lens (\UpdateCustomMetricResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomMetricResponse' {} a -> s {httpStatus = a} :: UpdateCustomMetricResponse)

instance Prelude.NFData UpdateCustomMetricResponse
