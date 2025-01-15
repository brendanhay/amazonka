{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerMetrics.Types.BatchPutMetricsError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerMetrics.Types.BatchPutMetricsError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerMetrics.Types.PutMetricsErrorCode

-- | An error that occured when putting the metric data.
--
-- /See:/ 'newBatchPutMetricsError' smart constructor.
data BatchPutMetricsError = BatchPutMetricsError'
  { -- | The error code of an error that occured when attempting to put metrics.
    --
    -- -   @METRIC_LIMIT_EXCEEDED@: The maximum amount of metrics per resource
    --     is exceeded.
    --
    -- -   @INTERNAL_ERROR@: An internal error occured.
    --
    -- -   @VALIDATION_ERROR@: The metric data failed validation.
    --
    -- -   @CONFLICT_ERROR@: Multiple requests attempted to modify the same
    --     data simultaneously.
    code :: Prelude.Maybe PutMetricsErrorCode,
    -- | An index that corresponds to the metric in the request.
    metricIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutMetricsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'batchPutMetricsError_code' - The error code of an error that occured when attempting to put metrics.
--
-- -   @METRIC_LIMIT_EXCEEDED@: The maximum amount of metrics per resource
--     is exceeded.
--
-- -   @INTERNAL_ERROR@: An internal error occured.
--
-- -   @VALIDATION_ERROR@: The metric data failed validation.
--
-- -   @CONFLICT_ERROR@: Multiple requests attempted to modify the same
--     data simultaneously.
--
-- 'metricIndex', 'batchPutMetricsError_metricIndex' - An index that corresponds to the metric in the request.
newBatchPutMetricsError ::
  BatchPutMetricsError
newBatchPutMetricsError =
  BatchPutMetricsError'
    { code = Prelude.Nothing,
      metricIndex = Prelude.Nothing
    }

-- | The error code of an error that occured when attempting to put metrics.
--
-- -   @METRIC_LIMIT_EXCEEDED@: The maximum amount of metrics per resource
--     is exceeded.
--
-- -   @INTERNAL_ERROR@: An internal error occured.
--
-- -   @VALIDATION_ERROR@: The metric data failed validation.
--
-- -   @CONFLICT_ERROR@: Multiple requests attempted to modify the same
--     data simultaneously.
batchPutMetricsError_code :: Lens.Lens' BatchPutMetricsError (Prelude.Maybe PutMetricsErrorCode)
batchPutMetricsError_code = Lens.lens (\BatchPutMetricsError' {code} -> code) (\s@BatchPutMetricsError' {} a -> s {code = a} :: BatchPutMetricsError)

-- | An index that corresponds to the metric in the request.
batchPutMetricsError_metricIndex :: Lens.Lens' BatchPutMetricsError (Prelude.Maybe Prelude.Int)
batchPutMetricsError_metricIndex = Lens.lens (\BatchPutMetricsError' {metricIndex} -> metricIndex) (\s@BatchPutMetricsError' {} a -> s {metricIndex = a} :: BatchPutMetricsError)

instance Data.FromJSON BatchPutMetricsError where
  parseJSON =
    Data.withObject
      "BatchPutMetricsError"
      ( \x ->
          BatchPutMetricsError'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "MetricIndex")
      )

instance Prelude.Hashable BatchPutMetricsError where
  hashWithSalt _salt BatchPutMetricsError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` metricIndex

instance Prelude.NFData BatchPutMetricsError where
  rnf BatchPutMetricsError' {..} =
    Prelude.rnf code `Prelude.seq`
      Prelude.rnf metricIndex
