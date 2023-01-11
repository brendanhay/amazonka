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
-- Module      : Amazonka.FraudDetector.Types.VariableImpactExplanation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.VariableImpactExplanation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the event variable\'s impact on the prediction score.
--
-- /See:/ 'newVariableImpactExplanation' smart constructor.
data VariableImpactExplanation = VariableImpactExplanation'
  { -- | The event variable name.
    eventVariableName :: Prelude.Maybe Prelude.Text,
    -- | The raw, uninterpreted value represented as log-odds of the fraud. These
    -- values are usually between -10 to +10, but range from - infinity to +
    -- infinity.
    --
    -- -   A positive value indicates that the variable drove the risk score
    --     up.
    --
    -- -   A negative value indicates that the variable drove the risk score
    --     down.
    logOddsImpact :: Prelude.Maybe Prelude.Double,
    -- | The event variable\'s relative impact in terms of magnitude on the
    -- prediction scores. The relative impact values consist of a numerical
    -- rating (0-5, 5 being the highest) and direction (increased\/decreased)
    -- impact of the fraud risk.
    relativeImpact :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableImpactExplanation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventVariableName', 'variableImpactExplanation_eventVariableName' - The event variable name.
--
-- 'logOddsImpact', 'variableImpactExplanation_logOddsImpact' - The raw, uninterpreted value represented as log-odds of the fraud. These
-- values are usually between -10 to +10, but range from - infinity to +
-- infinity.
--
-- -   A positive value indicates that the variable drove the risk score
--     up.
--
-- -   A negative value indicates that the variable drove the risk score
--     down.
--
-- 'relativeImpact', 'variableImpactExplanation_relativeImpact' - The event variable\'s relative impact in terms of magnitude on the
-- prediction scores. The relative impact values consist of a numerical
-- rating (0-5, 5 being the highest) and direction (increased\/decreased)
-- impact of the fraud risk.
newVariableImpactExplanation ::
  VariableImpactExplanation
newVariableImpactExplanation =
  VariableImpactExplanation'
    { eventVariableName =
        Prelude.Nothing,
      logOddsImpact = Prelude.Nothing,
      relativeImpact = Prelude.Nothing
    }

-- | The event variable name.
variableImpactExplanation_eventVariableName :: Lens.Lens' VariableImpactExplanation (Prelude.Maybe Prelude.Text)
variableImpactExplanation_eventVariableName = Lens.lens (\VariableImpactExplanation' {eventVariableName} -> eventVariableName) (\s@VariableImpactExplanation' {} a -> s {eventVariableName = a} :: VariableImpactExplanation)

-- | The raw, uninterpreted value represented as log-odds of the fraud. These
-- values are usually between -10 to +10, but range from - infinity to +
-- infinity.
--
-- -   A positive value indicates that the variable drove the risk score
--     up.
--
-- -   A negative value indicates that the variable drove the risk score
--     down.
variableImpactExplanation_logOddsImpact :: Lens.Lens' VariableImpactExplanation (Prelude.Maybe Prelude.Double)
variableImpactExplanation_logOddsImpact = Lens.lens (\VariableImpactExplanation' {logOddsImpact} -> logOddsImpact) (\s@VariableImpactExplanation' {} a -> s {logOddsImpact = a} :: VariableImpactExplanation)

-- | The event variable\'s relative impact in terms of magnitude on the
-- prediction scores. The relative impact values consist of a numerical
-- rating (0-5, 5 being the highest) and direction (increased\/decreased)
-- impact of the fraud risk.
variableImpactExplanation_relativeImpact :: Lens.Lens' VariableImpactExplanation (Prelude.Maybe Prelude.Text)
variableImpactExplanation_relativeImpact = Lens.lens (\VariableImpactExplanation' {relativeImpact} -> relativeImpact) (\s@VariableImpactExplanation' {} a -> s {relativeImpact = a} :: VariableImpactExplanation)

instance Data.FromJSON VariableImpactExplanation where
  parseJSON =
    Data.withObject
      "VariableImpactExplanation"
      ( \x ->
          VariableImpactExplanation'
            Prelude.<$> (x Data..:? "eventVariableName")
            Prelude.<*> (x Data..:? "logOddsImpact")
            Prelude.<*> (x Data..:? "relativeImpact")
      )

instance Prelude.Hashable VariableImpactExplanation where
  hashWithSalt _salt VariableImpactExplanation' {..} =
    _salt `Prelude.hashWithSalt` eventVariableName
      `Prelude.hashWithSalt` logOddsImpact
      `Prelude.hashWithSalt` relativeImpact

instance Prelude.NFData VariableImpactExplanation where
  rnf VariableImpactExplanation' {..} =
    Prelude.rnf eventVariableName
      `Prelude.seq` Prelude.rnf logOddsImpact
      `Prelude.seq` Prelude.rnf relativeImpact
