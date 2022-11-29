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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceComparisonValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceComparisonValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceMetric
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceScalar
import qualified Amazonka.Prelude as Prelude

-- | Reference scalar values and other metrics that DevOps Guru displays on a
-- graph in its console along with the actual metrics it analyzed. Compare
-- these reference values to your actual metrics to help you understand
-- anomalous behavior that DevOps Guru detected.
--
-- /See:/ 'newPerformanceInsightsReferenceComparisonValues' smart constructor.
data PerformanceInsightsReferenceComparisonValues = PerformanceInsightsReferenceComparisonValues'
  { -- | A scalar value DevOps Guru for a metric that DevOps Guru compares to
    -- actual metric values. This reference value is used to determine if an
    -- actual metric value should be considered anomalous.
    referenceScalar :: Prelude.Maybe PerformanceInsightsReferenceScalar,
    -- | A metric that DevOps Guru compares to actual metric values. This
    -- reference metric is used to determine if an actual metric should be
    -- considered anomalous.
    referenceMetric :: Prelude.Maybe PerformanceInsightsReferenceMetric
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsReferenceComparisonValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceScalar', 'performanceInsightsReferenceComparisonValues_referenceScalar' - A scalar value DevOps Guru for a metric that DevOps Guru compares to
-- actual metric values. This reference value is used to determine if an
-- actual metric value should be considered anomalous.
--
-- 'referenceMetric', 'performanceInsightsReferenceComparisonValues_referenceMetric' - A metric that DevOps Guru compares to actual metric values. This
-- reference metric is used to determine if an actual metric should be
-- considered anomalous.
newPerformanceInsightsReferenceComparisonValues ::
  PerformanceInsightsReferenceComparisonValues
newPerformanceInsightsReferenceComparisonValues =
  PerformanceInsightsReferenceComparisonValues'
    { referenceScalar =
        Prelude.Nothing,
      referenceMetric =
        Prelude.Nothing
    }

-- | A scalar value DevOps Guru for a metric that DevOps Guru compares to
-- actual metric values. This reference value is used to determine if an
-- actual metric value should be considered anomalous.
performanceInsightsReferenceComparisonValues_referenceScalar :: Lens.Lens' PerformanceInsightsReferenceComparisonValues (Prelude.Maybe PerformanceInsightsReferenceScalar)
performanceInsightsReferenceComparisonValues_referenceScalar = Lens.lens (\PerformanceInsightsReferenceComparisonValues' {referenceScalar} -> referenceScalar) (\s@PerformanceInsightsReferenceComparisonValues' {} a -> s {referenceScalar = a} :: PerformanceInsightsReferenceComparisonValues)

-- | A metric that DevOps Guru compares to actual metric values. This
-- reference metric is used to determine if an actual metric should be
-- considered anomalous.
performanceInsightsReferenceComparisonValues_referenceMetric :: Lens.Lens' PerformanceInsightsReferenceComparisonValues (Prelude.Maybe PerformanceInsightsReferenceMetric)
performanceInsightsReferenceComparisonValues_referenceMetric = Lens.lens (\PerformanceInsightsReferenceComparisonValues' {referenceMetric} -> referenceMetric) (\s@PerformanceInsightsReferenceComparisonValues' {} a -> s {referenceMetric = a} :: PerformanceInsightsReferenceComparisonValues)

instance
  Core.FromJSON
    PerformanceInsightsReferenceComparisonValues
  where
  parseJSON =
    Core.withObject
      "PerformanceInsightsReferenceComparisonValues"
      ( \x ->
          PerformanceInsightsReferenceComparisonValues'
            Prelude.<$> (x Core..:? "ReferenceScalar")
              Prelude.<*> (x Core..:? "ReferenceMetric")
      )

instance
  Prelude.Hashable
    PerformanceInsightsReferenceComparisonValues
  where
  hashWithSalt
    _salt
    PerformanceInsightsReferenceComparisonValues' {..} =
      _salt `Prelude.hashWithSalt` referenceScalar
        `Prelude.hashWithSalt` referenceMetric

instance
  Prelude.NFData
    PerformanceInsightsReferenceComparisonValues
  where
  rnf PerformanceInsightsReferenceComparisonValues' {..} =
    Prelude.rnf referenceScalar
      `Prelude.seq` Prelude.rnf referenceMetric
