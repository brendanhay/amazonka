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
-- Module      : Amazonka.Evidently.Types.ExperimentResultsData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentResultsData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ExperimentResultResponseType
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains experiment results for one metric that is
-- monitored in the experiment.
--
-- /See:/ 'newExperimentResultsData' smart constructor.
data ExperimentResultsData = ExperimentResultsData'
  { -- | The name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The experiment statistic that these results pertain to.
    resultStat :: Prelude.Maybe ExperimentResultResponseType,
    -- | The treatment, or variation, that returned the @values@ in this
    -- structure.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The values for the @metricName@ that were recorded in the experiment.
    values :: Prelude.Maybe [Prelude.Double]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentResultsData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'experimentResultsData_metricName' - The name of the metric.
--
-- 'resultStat', 'experimentResultsData_resultStat' - The experiment statistic that these results pertain to.
--
-- 'treatmentName', 'experimentResultsData_treatmentName' - The treatment, or variation, that returned the @values@ in this
-- structure.
--
-- 'values', 'experimentResultsData_values' - The values for the @metricName@ that were recorded in the experiment.
newExperimentResultsData ::
  ExperimentResultsData
newExperimentResultsData =
  ExperimentResultsData'
    { metricName =
        Prelude.Nothing,
      resultStat = Prelude.Nothing,
      treatmentName = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the metric.
experimentResultsData_metricName :: Lens.Lens' ExperimentResultsData (Prelude.Maybe Prelude.Text)
experimentResultsData_metricName = Lens.lens (\ExperimentResultsData' {metricName} -> metricName) (\s@ExperimentResultsData' {} a -> s {metricName = a} :: ExperimentResultsData)

-- | The experiment statistic that these results pertain to.
experimentResultsData_resultStat :: Lens.Lens' ExperimentResultsData (Prelude.Maybe ExperimentResultResponseType)
experimentResultsData_resultStat = Lens.lens (\ExperimentResultsData' {resultStat} -> resultStat) (\s@ExperimentResultsData' {} a -> s {resultStat = a} :: ExperimentResultsData)

-- | The treatment, or variation, that returned the @values@ in this
-- structure.
experimentResultsData_treatmentName :: Lens.Lens' ExperimentResultsData (Prelude.Maybe Prelude.Text)
experimentResultsData_treatmentName = Lens.lens (\ExperimentResultsData' {treatmentName} -> treatmentName) (\s@ExperimentResultsData' {} a -> s {treatmentName = a} :: ExperimentResultsData)

-- | The values for the @metricName@ that were recorded in the experiment.
experimentResultsData_values :: Lens.Lens' ExperimentResultsData (Prelude.Maybe [Prelude.Double])
experimentResultsData_values = Lens.lens (\ExperimentResultsData' {values} -> values) (\s@ExperimentResultsData' {} a -> s {values = a} :: ExperimentResultsData) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExperimentResultsData where
  parseJSON =
    Data.withObject
      "ExperimentResultsData"
      ( \x ->
          ExperimentResultsData'
            Prelude.<$> (x Data..:? "metricName")
            Prelude.<*> (x Data..:? "resultStat")
            Prelude.<*> (x Data..:? "treatmentName")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentResultsData where
  hashWithSalt _salt ExperimentResultsData' {..} =
    _salt `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` resultStat
      `Prelude.hashWithSalt` treatmentName
      `Prelude.hashWithSalt` values

instance Prelude.NFData ExperimentResultsData where
  rnf ExperimentResultsData' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf resultStat
      `Prelude.seq` Prelude.rnf treatmentName
      `Prelude.seq` Prelude.rnf values
