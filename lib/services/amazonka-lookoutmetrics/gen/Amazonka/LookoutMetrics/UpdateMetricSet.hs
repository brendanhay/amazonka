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
-- Module      : Amazonka.LookoutMetrics.UpdateMetricSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dataset.
module Amazonka.LookoutMetrics.UpdateMetricSet
  ( -- * Creating a Request
    UpdateMetricSet (..),
    newUpdateMetricSet,

    -- * Request Lenses
    updateMetricSet_dimensionList,
    updateMetricSet_offset,
    updateMetricSet_timestampColumn,
    updateMetricSet_metricList,
    updateMetricSet_metricSource,
    updateMetricSet_metricSetFrequency,
    updateMetricSet_metricSetDescription,
    updateMetricSet_metricSetArn,

    -- * Destructuring the Response
    UpdateMetricSetResponse (..),
    newUpdateMetricSetResponse,

    -- * Response Lenses
    updateMetricSetResponse_metricSetArn,
    updateMetricSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMetricSet' smart constructor.
data UpdateMetricSet = UpdateMetricSet'
  { -- | The dimension list.
    dimensionList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | After an interval ends, the amount of seconds that the detector waits
    -- before importing data. Offset is only supported for S3 and Redshift
    -- datasources.
    offset :: Prelude.Maybe Prelude.Natural,
    -- | The timestamp column.
    timestampColumn :: Prelude.Maybe TimestampColumn,
    -- | The metric list.
    metricList :: Prelude.Maybe (Prelude.NonEmpty Metric),
    metricSource :: Prelude.Maybe MetricSource,
    -- | The dataset\'s interval.
    metricSetFrequency :: Prelude.Maybe Frequency,
    -- | The dataset\'s description.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dataset to update.
    metricSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMetricSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionList', 'updateMetricSet_dimensionList' - The dimension list.
--
-- 'offset', 'updateMetricSet_offset' - After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3 and Redshift
-- datasources.
--
-- 'timestampColumn', 'updateMetricSet_timestampColumn' - The timestamp column.
--
-- 'metricList', 'updateMetricSet_metricList' - The metric list.
--
-- 'metricSource', 'updateMetricSet_metricSource' - Undocumented member.
--
-- 'metricSetFrequency', 'updateMetricSet_metricSetFrequency' - The dataset\'s interval.
--
-- 'metricSetDescription', 'updateMetricSet_metricSetDescription' - The dataset\'s description.
--
-- 'metricSetArn', 'updateMetricSet_metricSetArn' - The ARN of the dataset to update.
newUpdateMetricSet ::
  -- | 'metricSetArn'
  Prelude.Text ->
  UpdateMetricSet
newUpdateMetricSet pMetricSetArn_ =
  UpdateMetricSet'
    { dimensionList = Prelude.Nothing,
      offset = Prelude.Nothing,
      timestampColumn = Prelude.Nothing,
      metricList = Prelude.Nothing,
      metricSource = Prelude.Nothing,
      metricSetFrequency = Prelude.Nothing,
      metricSetDescription = Prelude.Nothing,
      metricSetArn = pMetricSetArn_
    }

-- | The dimension list.
updateMetricSet_dimensionList :: Lens.Lens' UpdateMetricSet (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateMetricSet_dimensionList = Lens.lens (\UpdateMetricSet' {dimensionList} -> dimensionList) (\s@UpdateMetricSet' {} a -> s {dimensionList = a} :: UpdateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3 and Redshift
-- datasources.
updateMetricSet_offset :: Lens.Lens' UpdateMetricSet (Prelude.Maybe Prelude.Natural)
updateMetricSet_offset = Lens.lens (\UpdateMetricSet' {offset} -> offset) (\s@UpdateMetricSet' {} a -> s {offset = a} :: UpdateMetricSet)

-- | The timestamp column.
updateMetricSet_timestampColumn :: Lens.Lens' UpdateMetricSet (Prelude.Maybe TimestampColumn)
updateMetricSet_timestampColumn = Lens.lens (\UpdateMetricSet' {timestampColumn} -> timestampColumn) (\s@UpdateMetricSet' {} a -> s {timestampColumn = a} :: UpdateMetricSet)

-- | The metric list.
updateMetricSet_metricList :: Lens.Lens' UpdateMetricSet (Prelude.Maybe (Prelude.NonEmpty Metric))
updateMetricSet_metricList = Lens.lens (\UpdateMetricSet' {metricList} -> metricList) (\s@UpdateMetricSet' {} a -> s {metricList = a} :: UpdateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateMetricSet_metricSource :: Lens.Lens' UpdateMetricSet (Prelude.Maybe MetricSource)
updateMetricSet_metricSource = Lens.lens (\UpdateMetricSet' {metricSource} -> metricSource) (\s@UpdateMetricSet' {} a -> s {metricSource = a} :: UpdateMetricSet)

-- | The dataset\'s interval.
updateMetricSet_metricSetFrequency :: Lens.Lens' UpdateMetricSet (Prelude.Maybe Frequency)
updateMetricSet_metricSetFrequency = Lens.lens (\UpdateMetricSet' {metricSetFrequency} -> metricSetFrequency) (\s@UpdateMetricSet' {} a -> s {metricSetFrequency = a} :: UpdateMetricSet)

-- | The dataset\'s description.
updateMetricSet_metricSetDescription :: Lens.Lens' UpdateMetricSet (Prelude.Maybe Prelude.Text)
updateMetricSet_metricSetDescription = Lens.lens (\UpdateMetricSet' {metricSetDescription} -> metricSetDescription) (\s@UpdateMetricSet' {} a -> s {metricSetDescription = a} :: UpdateMetricSet)

-- | The ARN of the dataset to update.
updateMetricSet_metricSetArn :: Lens.Lens' UpdateMetricSet Prelude.Text
updateMetricSet_metricSetArn = Lens.lens (\UpdateMetricSet' {metricSetArn} -> metricSetArn) (\s@UpdateMetricSet' {} a -> s {metricSetArn = a} :: UpdateMetricSet)

instance Core.AWSRequest UpdateMetricSet where
  type
    AWSResponse UpdateMetricSet =
      UpdateMetricSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMetricSetResponse'
            Prelude.<$> (x Core..?> "MetricSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMetricSet

instance Prelude.NFData UpdateMetricSet

instance Core.ToHeaders UpdateMetricSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMetricSet where
  toJSON UpdateMetricSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DimensionList" Core..=) Prelude.<$> dimensionList,
            ("Offset" Core..=) Prelude.<$> offset,
            ("TimestampColumn" Core..=)
              Prelude.<$> timestampColumn,
            ("MetricList" Core..=) Prelude.<$> metricList,
            ("MetricSource" Core..=) Prelude.<$> metricSource,
            ("MetricSetFrequency" Core..=)
              Prelude.<$> metricSetFrequency,
            ("MetricSetDescription" Core..=)
              Prelude.<$> metricSetDescription,
            Prelude.Just ("MetricSetArn" Core..= metricSetArn)
          ]
      )

instance Core.ToPath UpdateMetricSet where
  toPath = Prelude.const "/UpdateMetricSet"

instance Core.ToQuery UpdateMetricSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMetricSetResponse' smart constructor.
data UpdateMetricSetResponse = UpdateMetricSetResponse'
  { -- | The ARN of the dataset.
    metricSetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMetricSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricSetArn', 'updateMetricSetResponse_metricSetArn' - The ARN of the dataset.
--
-- 'httpStatus', 'updateMetricSetResponse_httpStatus' - The response's http status code.
newUpdateMetricSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMetricSetResponse
newUpdateMetricSetResponse pHttpStatus_ =
  UpdateMetricSetResponse'
    { metricSetArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the dataset.
updateMetricSetResponse_metricSetArn :: Lens.Lens' UpdateMetricSetResponse (Prelude.Maybe Prelude.Text)
updateMetricSetResponse_metricSetArn = Lens.lens (\UpdateMetricSetResponse' {metricSetArn} -> metricSetArn) (\s@UpdateMetricSetResponse' {} a -> s {metricSetArn = a} :: UpdateMetricSetResponse)

-- | The response's http status code.
updateMetricSetResponse_httpStatus :: Lens.Lens' UpdateMetricSetResponse Prelude.Int
updateMetricSetResponse_httpStatus = Lens.lens (\UpdateMetricSetResponse' {httpStatus} -> httpStatus) (\s@UpdateMetricSetResponse' {} a -> s {httpStatus = a} :: UpdateMetricSetResponse)

instance Prelude.NFData UpdateMetricSetResponse
