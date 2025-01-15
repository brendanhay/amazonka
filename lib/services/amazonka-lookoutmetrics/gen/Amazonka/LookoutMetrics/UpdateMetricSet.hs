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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dataset.
module Amazonka.LookoutMetrics.UpdateMetricSet
  ( -- * Creating a Request
    UpdateMetricSet (..),
    newUpdateMetricSet,

    -- * Request Lenses
    updateMetricSet_dimensionFilterList,
    updateMetricSet_dimensionList,
    updateMetricSet_metricList,
    updateMetricSet_metricSetDescription,
    updateMetricSet_metricSetFrequency,
    updateMetricSet_metricSource,
    updateMetricSet_offset,
    updateMetricSet_timestampColumn,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMetricSet' smart constructor.
data UpdateMetricSet = UpdateMetricSet'
  { -- | Describes a list of filters for choosing specific dimensions and
    -- specific values. Each filter consists of the dimension and one of its
    -- values that you want to include. When multiple dimensions or values are
    -- specified, the dimensions are joined with an AND operation and the
    -- values are joined with an OR operation.
    dimensionFilterList :: Prelude.Maybe [MetricSetDimensionFilter],
    -- | The dimension list.
    dimensionList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The metric list.
    metricList :: Prelude.Maybe (Prelude.NonEmpty Metric),
    -- | The dataset\'s description.
    metricSetDescription :: Prelude.Maybe Prelude.Text,
    -- | The dataset\'s interval.
    metricSetFrequency :: Prelude.Maybe Frequency,
    metricSource :: Prelude.Maybe MetricSource,
    -- | After an interval ends, the amount of seconds that the detector waits
    -- before importing data. Offset is only supported for S3, Redshift, Athena
    -- and datasources.
    offset :: Prelude.Maybe Prelude.Natural,
    -- | The timestamp column.
    timestampColumn :: Prelude.Maybe TimestampColumn,
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
-- 'dimensionFilterList', 'updateMetricSet_dimensionFilterList' - Describes a list of filters for choosing specific dimensions and
-- specific values. Each filter consists of the dimension and one of its
-- values that you want to include. When multiple dimensions or values are
-- specified, the dimensions are joined with an AND operation and the
-- values are joined with an OR operation.
--
-- 'dimensionList', 'updateMetricSet_dimensionList' - The dimension list.
--
-- 'metricList', 'updateMetricSet_metricList' - The metric list.
--
-- 'metricSetDescription', 'updateMetricSet_metricSetDescription' - The dataset\'s description.
--
-- 'metricSetFrequency', 'updateMetricSet_metricSetFrequency' - The dataset\'s interval.
--
-- 'metricSource', 'updateMetricSet_metricSource' - Undocumented member.
--
-- 'offset', 'updateMetricSet_offset' - After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3, Redshift, Athena
-- and datasources.
--
-- 'timestampColumn', 'updateMetricSet_timestampColumn' - The timestamp column.
--
-- 'metricSetArn', 'updateMetricSet_metricSetArn' - The ARN of the dataset to update.
newUpdateMetricSet ::
  -- | 'metricSetArn'
  Prelude.Text ->
  UpdateMetricSet
newUpdateMetricSet pMetricSetArn_ =
  UpdateMetricSet'
    { dimensionFilterList =
        Prelude.Nothing,
      dimensionList = Prelude.Nothing,
      metricList = Prelude.Nothing,
      metricSetDescription = Prelude.Nothing,
      metricSetFrequency = Prelude.Nothing,
      metricSource = Prelude.Nothing,
      offset = Prelude.Nothing,
      timestampColumn = Prelude.Nothing,
      metricSetArn = pMetricSetArn_
    }

-- | Describes a list of filters for choosing specific dimensions and
-- specific values. Each filter consists of the dimension and one of its
-- values that you want to include. When multiple dimensions or values are
-- specified, the dimensions are joined with an AND operation and the
-- values are joined with an OR operation.
updateMetricSet_dimensionFilterList :: Lens.Lens' UpdateMetricSet (Prelude.Maybe [MetricSetDimensionFilter])
updateMetricSet_dimensionFilterList = Lens.lens (\UpdateMetricSet' {dimensionFilterList} -> dimensionFilterList) (\s@UpdateMetricSet' {} a -> s {dimensionFilterList = a} :: UpdateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | The dimension list.
updateMetricSet_dimensionList :: Lens.Lens' UpdateMetricSet (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateMetricSet_dimensionList = Lens.lens (\UpdateMetricSet' {dimensionList} -> dimensionList) (\s@UpdateMetricSet' {} a -> s {dimensionList = a} :: UpdateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | The metric list.
updateMetricSet_metricList :: Lens.Lens' UpdateMetricSet (Prelude.Maybe (Prelude.NonEmpty Metric))
updateMetricSet_metricList = Lens.lens (\UpdateMetricSet' {metricList} -> metricList) (\s@UpdateMetricSet' {} a -> s {metricList = a} :: UpdateMetricSet) Prelude.. Lens.mapping Lens.coerced

-- | The dataset\'s description.
updateMetricSet_metricSetDescription :: Lens.Lens' UpdateMetricSet (Prelude.Maybe Prelude.Text)
updateMetricSet_metricSetDescription = Lens.lens (\UpdateMetricSet' {metricSetDescription} -> metricSetDescription) (\s@UpdateMetricSet' {} a -> s {metricSetDescription = a} :: UpdateMetricSet)

-- | The dataset\'s interval.
updateMetricSet_metricSetFrequency :: Lens.Lens' UpdateMetricSet (Prelude.Maybe Frequency)
updateMetricSet_metricSetFrequency = Lens.lens (\UpdateMetricSet' {metricSetFrequency} -> metricSetFrequency) (\s@UpdateMetricSet' {} a -> s {metricSetFrequency = a} :: UpdateMetricSet)

-- | Undocumented member.
updateMetricSet_metricSource :: Lens.Lens' UpdateMetricSet (Prelude.Maybe MetricSource)
updateMetricSet_metricSource = Lens.lens (\UpdateMetricSet' {metricSource} -> metricSource) (\s@UpdateMetricSet' {} a -> s {metricSource = a} :: UpdateMetricSet)

-- | After an interval ends, the amount of seconds that the detector waits
-- before importing data. Offset is only supported for S3, Redshift, Athena
-- and datasources.
updateMetricSet_offset :: Lens.Lens' UpdateMetricSet (Prelude.Maybe Prelude.Natural)
updateMetricSet_offset = Lens.lens (\UpdateMetricSet' {offset} -> offset) (\s@UpdateMetricSet' {} a -> s {offset = a} :: UpdateMetricSet)

-- | The timestamp column.
updateMetricSet_timestampColumn :: Lens.Lens' UpdateMetricSet (Prelude.Maybe TimestampColumn)
updateMetricSet_timestampColumn = Lens.lens (\UpdateMetricSet' {timestampColumn} -> timestampColumn) (\s@UpdateMetricSet' {} a -> s {timestampColumn = a} :: UpdateMetricSet)

-- | The ARN of the dataset to update.
updateMetricSet_metricSetArn :: Lens.Lens' UpdateMetricSet Prelude.Text
updateMetricSet_metricSetArn = Lens.lens (\UpdateMetricSet' {metricSetArn} -> metricSetArn) (\s@UpdateMetricSet' {} a -> s {metricSetArn = a} :: UpdateMetricSet)

instance Core.AWSRequest UpdateMetricSet where
  type
    AWSResponse UpdateMetricSet =
      UpdateMetricSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMetricSetResponse'
            Prelude.<$> (x Data..?> "MetricSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMetricSet where
  hashWithSalt _salt UpdateMetricSet' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionFilterList
      `Prelude.hashWithSalt` dimensionList
      `Prelude.hashWithSalt` metricList
      `Prelude.hashWithSalt` metricSetDescription
      `Prelude.hashWithSalt` metricSetFrequency
      `Prelude.hashWithSalt` metricSource
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` timestampColumn
      `Prelude.hashWithSalt` metricSetArn

instance Prelude.NFData UpdateMetricSet where
  rnf UpdateMetricSet' {..} =
    Prelude.rnf dimensionFilterList `Prelude.seq`
      Prelude.rnf dimensionList `Prelude.seq`
        Prelude.rnf metricList `Prelude.seq`
          Prelude.rnf metricSetDescription `Prelude.seq`
            Prelude.rnf metricSetFrequency `Prelude.seq`
              Prelude.rnf metricSource `Prelude.seq`
                Prelude.rnf offset `Prelude.seq`
                  Prelude.rnf timestampColumn `Prelude.seq`
                    Prelude.rnf metricSetArn

instance Data.ToHeaders UpdateMetricSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMetricSet where
  toJSON UpdateMetricSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionFilterList" Data..=)
              Prelude.<$> dimensionFilterList,
            ("DimensionList" Data..=) Prelude.<$> dimensionList,
            ("MetricList" Data..=) Prelude.<$> metricList,
            ("MetricSetDescription" Data..=)
              Prelude.<$> metricSetDescription,
            ("MetricSetFrequency" Data..=)
              Prelude.<$> metricSetFrequency,
            ("MetricSource" Data..=) Prelude.<$> metricSource,
            ("Offset" Data..=) Prelude.<$> offset,
            ("TimestampColumn" Data..=)
              Prelude.<$> timestampColumn,
            Prelude.Just ("MetricSetArn" Data..= metricSetArn)
          ]
      )

instance Data.ToPath UpdateMetricSet where
  toPath = Prelude.const "/UpdateMetricSet"

instance Data.ToQuery UpdateMetricSet where
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

instance Prelude.NFData UpdateMetricSetResponse where
  rnf UpdateMetricSetResponse' {..} =
    Prelude.rnf metricSetArn `Prelude.seq`
      Prelude.rnf httpStatus
