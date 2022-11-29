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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceComparisonValues
import qualified Amazonka.Prelude as Prelude

-- | Reference data used to evaluate Performance Insights to determine if its
-- performance is anomalous or not.
--
-- /See:/ 'newPerformanceInsightsReferenceData' smart constructor.
data PerformanceInsightsReferenceData = PerformanceInsightsReferenceData'
  { -- | The name of the reference data.
    name :: Prelude.Maybe Prelude.Text,
    -- | The specific reference values used to evaluate the Performance Insights.
    -- For more information, see
    -- @ PerformanceInsightsReferenceComparisonValues @.
    comparisonValues :: Prelude.Maybe PerformanceInsightsReferenceComparisonValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsReferenceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'performanceInsightsReferenceData_name' - The name of the reference data.
--
-- 'comparisonValues', 'performanceInsightsReferenceData_comparisonValues' - The specific reference values used to evaluate the Performance Insights.
-- For more information, see
-- @ PerformanceInsightsReferenceComparisonValues @.
newPerformanceInsightsReferenceData ::
  PerformanceInsightsReferenceData
newPerformanceInsightsReferenceData =
  PerformanceInsightsReferenceData'
    { name =
        Prelude.Nothing,
      comparisonValues = Prelude.Nothing
    }

-- | The name of the reference data.
performanceInsightsReferenceData_name :: Lens.Lens' PerformanceInsightsReferenceData (Prelude.Maybe Prelude.Text)
performanceInsightsReferenceData_name = Lens.lens (\PerformanceInsightsReferenceData' {name} -> name) (\s@PerformanceInsightsReferenceData' {} a -> s {name = a} :: PerformanceInsightsReferenceData)

-- | The specific reference values used to evaluate the Performance Insights.
-- For more information, see
-- @ PerformanceInsightsReferenceComparisonValues @.
performanceInsightsReferenceData_comparisonValues :: Lens.Lens' PerformanceInsightsReferenceData (Prelude.Maybe PerformanceInsightsReferenceComparisonValues)
performanceInsightsReferenceData_comparisonValues = Lens.lens (\PerformanceInsightsReferenceData' {comparisonValues} -> comparisonValues) (\s@PerformanceInsightsReferenceData' {} a -> s {comparisonValues = a} :: PerformanceInsightsReferenceData)

instance
  Core.FromJSON
    PerformanceInsightsReferenceData
  where
  parseJSON =
    Core.withObject
      "PerformanceInsightsReferenceData"
      ( \x ->
          PerformanceInsightsReferenceData'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ComparisonValues")
      )

instance
  Prelude.Hashable
    PerformanceInsightsReferenceData
  where
  hashWithSalt
    _salt
    PerformanceInsightsReferenceData' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` comparisonValues

instance
  Prelude.NFData
    PerformanceInsightsReferenceData
  where
  rnf PerformanceInsightsReferenceData' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf comparisonValues
