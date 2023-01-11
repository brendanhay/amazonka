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
-- Module      : Amazonka.FraudDetector.Types.TrainingResultV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TrainingResultV2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.AggregatedVariablesImportanceMetrics
import Amazonka.FraudDetector.Types.DataValidationMetrics
import Amazonka.FraudDetector.Types.TrainingMetricsV2
import Amazonka.FraudDetector.Types.VariableImportanceMetrics
import qualified Amazonka.Prelude as Prelude

-- | The training result details.
--
-- /See:/ 'newTrainingResultV2' smart constructor.
data TrainingResultV2 = TrainingResultV2'
  { -- | The variable importance metrics of the aggregated variables.
    --
    -- Account Takeover Insights (ATI) model uses event variables from the
    -- login data you provide to continuously calculate a set of variables
    -- (aggregated variables) based on historical events. For example, your ATI
    -- model might calculate the number of times an user has logged in using
    -- the same IP address. In this case, event variables used to derive the
    -- aggregated variables are @IP address@ and @user@.
    aggregatedVariablesImportanceMetrics :: Prelude.Maybe AggregatedVariablesImportanceMetrics,
    dataValidationMetrics :: Prelude.Maybe DataValidationMetrics,
    -- | The training metric details.
    trainingMetricsV2 :: Prelude.Maybe TrainingMetricsV2,
    variableImportanceMetrics :: Prelude.Maybe VariableImportanceMetrics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingResultV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregatedVariablesImportanceMetrics', 'trainingResultV2_aggregatedVariablesImportanceMetrics' - The variable importance metrics of the aggregated variables.
--
-- Account Takeover Insights (ATI) model uses event variables from the
-- login data you provide to continuously calculate a set of variables
-- (aggregated variables) based on historical events. For example, your ATI
-- model might calculate the number of times an user has logged in using
-- the same IP address. In this case, event variables used to derive the
-- aggregated variables are @IP address@ and @user@.
--
-- 'dataValidationMetrics', 'trainingResultV2_dataValidationMetrics' - Undocumented member.
--
-- 'trainingMetricsV2', 'trainingResultV2_trainingMetricsV2' - The training metric details.
--
-- 'variableImportanceMetrics', 'trainingResultV2_variableImportanceMetrics' - Undocumented member.
newTrainingResultV2 ::
  TrainingResultV2
newTrainingResultV2 =
  TrainingResultV2'
    { aggregatedVariablesImportanceMetrics =
        Prelude.Nothing,
      dataValidationMetrics = Prelude.Nothing,
      trainingMetricsV2 = Prelude.Nothing,
      variableImportanceMetrics = Prelude.Nothing
    }

-- | The variable importance metrics of the aggregated variables.
--
-- Account Takeover Insights (ATI) model uses event variables from the
-- login data you provide to continuously calculate a set of variables
-- (aggregated variables) based on historical events. For example, your ATI
-- model might calculate the number of times an user has logged in using
-- the same IP address. In this case, event variables used to derive the
-- aggregated variables are @IP address@ and @user@.
trainingResultV2_aggregatedVariablesImportanceMetrics :: Lens.Lens' TrainingResultV2 (Prelude.Maybe AggregatedVariablesImportanceMetrics)
trainingResultV2_aggregatedVariablesImportanceMetrics = Lens.lens (\TrainingResultV2' {aggregatedVariablesImportanceMetrics} -> aggregatedVariablesImportanceMetrics) (\s@TrainingResultV2' {} a -> s {aggregatedVariablesImportanceMetrics = a} :: TrainingResultV2)

-- | Undocumented member.
trainingResultV2_dataValidationMetrics :: Lens.Lens' TrainingResultV2 (Prelude.Maybe DataValidationMetrics)
trainingResultV2_dataValidationMetrics = Lens.lens (\TrainingResultV2' {dataValidationMetrics} -> dataValidationMetrics) (\s@TrainingResultV2' {} a -> s {dataValidationMetrics = a} :: TrainingResultV2)

-- | The training metric details.
trainingResultV2_trainingMetricsV2 :: Lens.Lens' TrainingResultV2 (Prelude.Maybe TrainingMetricsV2)
trainingResultV2_trainingMetricsV2 = Lens.lens (\TrainingResultV2' {trainingMetricsV2} -> trainingMetricsV2) (\s@TrainingResultV2' {} a -> s {trainingMetricsV2 = a} :: TrainingResultV2)

-- | Undocumented member.
trainingResultV2_variableImportanceMetrics :: Lens.Lens' TrainingResultV2 (Prelude.Maybe VariableImportanceMetrics)
trainingResultV2_variableImportanceMetrics = Lens.lens (\TrainingResultV2' {variableImportanceMetrics} -> variableImportanceMetrics) (\s@TrainingResultV2' {} a -> s {variableImportanceMetrics = a} :: TrainingResultV2)

instance Data.FromJSON TrainingResultV2 where
  parseJSON =
    Data.withObject
      "TrainingResultV2"
      ( \x ->
          TrainingResultV2'
            Prelude.<$> (x Data..:? "aggregatedVariablesImportanceMetrics")
            Prelude.<*> (x Data..:? "dataValidationMetrics")
            Prelude.<*> (x Data..:? "trainingMetricsV2")
            Prelude.<*> (x Data..:? "variableImportanceMetrics")
      )

instance Prelude.Hashable TrainingResultV2 where
  hashWithSalt _salt TrainingResultV2' {..} =
    _salt
      `Prelude.hashWithSalt` aggregatedVariablesImportanceMetrics
      `Prelude.hashWithSalt` dataValidationMetrics
      `Prelude.hashWithSalt` trainingMetricsV2
      `Prelude.hashWithSalt` variableImportanceMetrics

instance Prelude.NFData TrainingResultV2 where
  rnf TrainingResultV2' {..} =
    Prelude.rnf aggregatedVariablesImportanceMetrics
      `Prelude.seq` Prelude.rnf dataValidationMetrics
      `Prelude.seq` Prelude.rnf trainingMetricsV2
      `Prelude.seq` Prelude.rnf variableImportanceMetrics
