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
-- Module      : Amazonka.FraudDetector.Types.PredictionExplanations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.PredictionExplanations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types.AggregatedVariablesImpactExplanation
import Amazonka.FraudDetector.Types.VariableImpactExplanation
import qualified Amazonka.Prelude as Prelude

-- | The prediction explanations that provide insight into how each event
-- variable impacted the model version\'s fraud prediction score.
--
-- /See:/ 'newPredictionExplanations' smart constructor.
data PredictionExplanations = PredictionExplanations'
  { -- | The details of the event variable\'s impact on the prediction score.
    variableImpactExplanations :: Prelude.Maybe [VariableImpactExplanation],
    -- | The details of the aggregated variables impact on the prediction score.
    --
    -- Account Takeover Insights (ATI) model uses event variables from the
    -- login data you provide to continuously calculate a set of variables
    -- (aggregated variables) based on historical events. For example, your ATI
    -- model might calculate the number of times an user has logged in using
    -- the same IP address. In this case, event variables used to derive the
    -- aggregated variables are @IP address@ and @user@.
    aggregatedVariablesImpactExplanations :: Prelude.Maybe [AggregatedVariablesImpactExplanation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictionExplanations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variableImpactExplanations', 'predictionExplanations_variableImpactExplanations' - The details of the event variable\'s impact on the prediction score.
--
-- 'aggregatedVariablesImpactExplanations', 'predictionExplanations_aggregatedVariablesImpactExplanations' - The details of the aggregated variables impact on the prediction score.
--
-- Account Takeover Insights (ATI) model uses event variables from the
-- login data you provide to continuously calculate a set of variables
-- (aggregated variables) based on historical events. For example, your ATI
-- model might calculate the number of times an user has logged in using
-- the same IP address. In this case, event variables used to derive the
-- aggregated variables are @IP address@ and @user@.
newPredictionExplanations ::
  PredictionExplanations
newPredictionExplanations =
  PredictionExplanations'
    { variableImpactExplanations =
        Prelude.Nothing,
      aggregatedVariablesImpactExplanations =
        Prelude.Nothing
    }

-- | The details of the event variable\'s impact on the prediction score.
predictionExplanations_variableImpactExplanations :: Lens.Lens' PredictionExplanations (Prelude.Maybe [VariableImpactExplanation])
predictionExplanations_variableImpactExplanations = Lens.lens (\PredictionExplanations' {variableImpactExplanations} -> variableImpactExplanations) (\s@PredictionExplanations' {} a -> s {variableImpactExplanations = a} :: PredictionExplanations) Prelude.. Lens.mapping Lens.coerced

-- | The details of the aggregated variables impact on the prediction score.
--
-- Account Takeover Insights (ATI) model uses event variables from the
-- login data you provide to continuously calculate a set of variables
-- (aggregated variables) based on historical events. For example, your ATI
-- model might calculate the number of times an user has logged in using
-- the same IP address. In this case, event variables used to derive the
-- aggregated variables are @IP address@ and @user@.
predictionExplanations_aggregatedVariablesImpactExplanations :: Lens.Lens' PredictionExplanations (Prelude.Maybe [AggregatedVariablesImpactExplanation])
predictionExplanations_aggregatedVariablesImpactExplanations = Lens.lens (\PredictionExplanations' {aggregatedVariablesImpactExplanations} -> aggregatedVariablesImpactExplanations) (\s@PredictionExplanations' {} a -> s {aggregatedVariablesImpactExplanations = a} :: PredictionExplanations) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PredictionExplanations where
  parseJSON =
    Core.withObject
      "PredictionExplanations"
      ( \x ->
          PredictionExplanations'
            Prelude.<$> ( x Core..:? "variableImpactExplanations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "aggregatedVariablesImpactExplanations"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PredictionExplanations where
  hashWithSalt _salt PredictionExplanations' {..} =
    _salt
      `Prelude.hashWithSalt` variableImpactExplanations
      `Prelude.hashWithSalt` aggregatedVariablesImpactExplanations

instance Prelude.NFData PredictionExplanations where
  rnf PredictionExplanations' {..} =
    Prelude.rnf variableImpactExplanations
      `Prelude.seq` Prelude.rnf aggregatedVariablesImpactExplanations
