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
-- Module      : Amazonka.FraudDetector.Types.AggregatedVariablesImpactExplanation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.AggregatedVariablesImpactExplanation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the impact of aggregated variables on the prediction
-- score.
--
-- Account Takeover Insights (ATI) model uses the login data you provide to
-- continuously calculate a set of variables (aggregated variables) based
-- on historical events. For example, the model might calculate the number
-- of times an user has logged in using the same IP address. In this case,
-- event variables used to derive the aggregated variables are @IP address@
-- and @user@.
--
-- /See:/ 'newAggregatedVariablesImpactExplanation' smart constructor.
data AggregatedVariablesImpactExplanation = AggregatedVariablesImpactExplanation'
  { -- | The raw, uninterpreted value represented as log-odds of the fraud. These
    -- values are usually between -10 to +10, but range from -infinity to
    -- +infinity.
    --
    -- -   A positive value indicates that the variables drove the risk score
    --     up.
    --
    -- -   A negative value indicates that the variables drove the risk score
    --     down.
    logOddsImpact :: Prelude.Maybe Prelude.Double,
    -- | The relative impact of the aggregated variables in terms of magnitude on
    -- the prediction scores.
    relativeImpact :: Prelude.Maybe Prelude.Text,
    -- | The names of all the event variables that were used to derive the
    -- aggregated variables.
    eventVariableNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregatedVariablesImpactExplanation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logOddsImpact', 'aggregatedVariablesImpactExplanation_logOddsImpact' - The raw, uninterpreted value represented as log-odds of the fraud. These
-- values are usually between -10 to +10, but range from -infinity to
-- +infinity.
--
-- -   A positive value indicates that the variables drove the risk score
--     up.
--
-- -   A negative value indicates that the variables drove the risk score
--     down.
--
-- 'relativeImpact', 'aggregatedVariablesImpactExplanation_relativeImpact' - The relative impact of the aggregated variables in terms of magnitude on
-- the prediction scores.
--
-- 'eventVariableNames', 'aggregatedVariablesImpactExplanation_eventVariableNames' - The names of all the event variables that were used to derive the
-- aggregated variables.
newAggregatedVariablesImpactExplanation ::
  AggregatedVariablesImpactExplanation
newAggregatedVariablesImpactExplanation =
  AggregatedVariablesImpactExplanation'
    { logOddsImpact =
        Prelude.Nothing,
      relativeImpact = Prelude.Nothing,
      eventVariableNames = Prelude.Nothing
    }

-- | The raw, uninterpreted value represented as log-odds of the fraud. These
-- values are usually between -10 to +10, but range from -infinity to
-- +infinity.
--
-- -   A positive value indicates that the variables drove the risk score
--     up.
--
-- -   A negative value indicates that the variables drove the risk score
--     down.
aggregatedVariablesImpactExplanation_logOddsImpact :: Lens.Lens' AggregatedVariablesImpactExplanation (Prelude.Maybe Prelude.Double)
aggregatedVariablesImpactExplanation_logOddsImpact = Lens.lens (\AggregatedVariablesImpactExplanation' {logOddsImpact} -> logOddsImpact) (\s@AggregatedVariablesImpactExplanation' {} a -> s {logOddsImpact = a} :: AggregatedVariablesImpactExplanation)

-- | The relative impact of the aggregated variables in terms of magnitude on
-- the prediction scores.
aggregatedVariablesImpactExplanation_relativeImpact :: Lens.Lens' AggregatedVariablesImpactExplanation (Prelude.Maybe Prelude.Text)
aggregatedVariablesImpactExplanation_relativeImpact = Lens.lens (\AggregatedVariablesImpactExplanation' {relativeImpact} -> relativeImpact) (\s@AggregatedVariablesImpactExplanation' {} a -> s {relativeImpact = a} :: AggregatedVariablesImpactExplanation)

-- | The names of all the event variables that were used to derive the
-- aggregated variables.
aggregatedVariablesImpactExplanation_eventVariableNames :: Lens.Lens' AggregatedVariablesImpactExplanation (Prelude.Maybe [Prelude.Text])
aggregatedVariablesImpactExplanation_eventVariableNames = Lens.lens (\AggregatedVariablesImpactExplanation' {eventVariableNames} -> eventVariableNames) (\s@AggregatedVariablesImpactExplanation' {} a -> s {eventVariableNames = a} :: AggregatedVariablesImpactExplanation) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AggregatedVariablesImpactExplanation
  where
  parseJSON =
    Data.withObject
      "AggregatedVariablesImpactExplanation"
      ( \x ->
          AggregatedVariablesImpactExplanation'
            Prelude.<$> (x Data..:? "logOddsImpact")
            Prelude.<*> (x Data..:? "relativeImpact")
            Prelude.<*> ( x Data..:? "eventVariableNames"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AggregatedVariablesImpactExplanation
  where
  hashWithSalt
    _salt
    AggregatedVariablesImpactExplanation' {..} =
      _salt `Prelude.hashWithSalt` logOddsImpact
        `Prelude.hashWithSalt` relativeImpact
        `Prelude.hashWithSalt` eventVariableNames

instance
  Prelude.NFData
    AggregatedVariablesImpactExplanation
  where
  rnf AggregatedVariablesImpactExplanation' {..} =
    Prelude.rnf logOddsImpact
      `Prelude.seq` Prelude.rnf relativeImpact
      `Prelude.seq` Prelude.rnf eventVariableNames
