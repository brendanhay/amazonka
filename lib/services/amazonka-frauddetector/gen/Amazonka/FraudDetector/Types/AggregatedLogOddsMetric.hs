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
-- Module      : Amazonka.FraudDetector.Types.AggregatedLogOddsMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.AggregatedLogOddsMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The log odds metric details.
--
-- Account Takeover Insights (ATI) model uses event variables from the
-- login data you provide to continuously calculate a set of variables
-- (aggregated variables) based on historical events. For example, your ATI
-- model might calculate the number of times an user has logged in using
-- the same IP address. In this case, event variables used to derive the
-- aggregated variables are @IP address@ and @user@.
--
-- /See:/ 'newAggregatedLogOddsMetric' smart constructor.
data AggregatedLogOddsMetric = AggregatedLogOddsMetric'
  { -- | The names of all the variables.
    variableNames :: [Prelude.Text],
    -- | The relative importance of the variables in the list to the other event
    -- variable.
    aggregatedVariablesImportance :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregatedLogOddsMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variableNames', 'aggregatedLogOddsMetric_variableNames' - The names of all the variables.
--
-- 'aggregatedVariablesImportance', 'aggregatedLogOddsMetric_aggregatedVariablesImportance' - The relative importance of the variables in the list to the other event
-- variable.
newAggregatedLogOddsMetric ::
  -- | 'aggregatedVariablesImportance'
  Prelude.Double ->
  AggregatedLogOddsMetric
newAggregatedLogOddsMetric
  pAggregatedVariablesImportance_ =
    AggregatedLogOddsMetric'
      { variableNames =
          Prelude.mempty,
        aggregatedVariablesImportance =
          pAggregatedVariablesImportance_
      }

-- | The names of all the variables.
aggregatedLogOddsMetric_variableNames :: Lens.Lens' AggregatedLogOddsMetric [Prelude.Text]
aggregatedLogOddsMetric_variableNames = Lens.lens (\AggregatedLogOddsMetric' {variableNames} -> variableNames) (\s@AggregatedLogOddsMetric' {} a -> s {variableNames = a} :: AggregatedLogOddsMetric) Prelude.. Lens.coerced

-- | The relative importance of the variables in the list to the other event
-- variable.
aggregatedLogOddsMetric_aggregatedVariablesImportance :: Lens.Lens' AggregatedLogOddsMetric Prelude.Double
aggregatedLogOddsMetric_aggregatedVariablesImportance = Lens.lens (\AggregatedLogOddsMetric' {aggregatedVariablesImportance} -> aggregatedVariablesImportance) (\s@AggregatedLogOddsMetric' {} a -> s {aggregatedVariablesImportance = a} :: AggregatedLogOddsMetric)

instance Data.FromJSON AggregatedLogOddsMetric where
  parseJSON =
    Data.withObject
      "AggregatedLogOddsMetric"
      ( \x ->
          AggregatedLogOddsMetric'
            Prelude.<$> (x Data..:? "variableNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "aggregatedVariablesImportance")
      )

instance Prelude.Hashable AggregatedLogOddsMetric where
  hashWithSalt _salt AggregatedLogOddsMetric' {..} =
    _salt
      `Prelude.hashWithSalt` variableNames
      `Prelude.hashWithSalt` aggregatedVariablesImportance

instance Prelude.NFData AggregatedLogOddsMetric where
  rnf AggregatedLogOddsMetric' {..} =
    Prelude.rnf variableNames
      `Prelude.seq` Prelude.rnf aggregatedVariablesImportance
