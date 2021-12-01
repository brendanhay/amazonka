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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansCoverage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansCoverage where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.DateInterval
import Amazonka.CostExplorer.Types.SavingsPlansCoverageData
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The amount of Savings Plans eligible usage that is covered by Savings
-- Plans. All calculations consider the On-Demand equivalent of your
-- Savings Plans usage.
--
-- /See:/ 'newSavingsPlansCoverage' smart constructor.
data SavingsPlansCoverage = SavingsPlansCoverage'
  { timePeriod :: Prelude.Maybe DateInterval,
    -- | The amount of Savings Plans eligible usage that the Savings Plans
    -- covered.
    coverage :: Prelude.Maybe SavingsPlansCoverageData,
    -- | The attribute that applies to a specific @Dimension@.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansCoverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timePeriod', 'savingsPlansCoverage_timePeriod' - Undocumented member.
--
-- 'coverage', 'savingsPlansCoverage_coverage' - The amount of Savings Plans eligible usage that the Savings Plans
-- covered.
--
-- 'attributes', 'savingsPlansCoverage_attributes' - The attribute that applies to a specific @Dimension@.
newSavingsPlansCoverage ::
  SavingsPlansCoverage
newSavingsPlansCoverage =
  SavingsPlansCoverage'
    { timePeriod = Prelude.Nothing,
      coverage = Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | Undocumented member.
savingsPlansCoverage_timePeriod :: Lens.Lens' SavingsPlansCoverage (Prelude.Maybe DateInterval)
savingsPlansCoverage_timePeriod = Lens.lens (\SavingsPlansCoverage' {timePeriod} -> timePeriod) (\s@SavingsPlansCoverage' {} a -> s {timePeriod = a} :: SavingsPlansCoverage)

-- | The amount of Savings Plans eligible usage that the Savings Plans
-- covered.
savingsPlansCoverage_coverage :: Lens.Lens' SavingsPlansCoverage (Prelude.Maybe SavingsPlansCoverageData)
savingsPlansCoverage_coverage = Lens.lens (\SavingsPlansCoverage' {coverage} -> coverage) (\s@SavingsPlansCoverage' {} a -> s {coverage = a} :: SavingsPlansCoverage)

-- | The attribute that applies to a specific @Dimension@.
savingsPlansCoverage_attributes :: Lens.Lens' SavingsPlansCoverage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
savingsPlansCoverage_attributes = Lens.lens (\SavingsPlansCoverage' {attributes} -> attributes) (\s@SavingsPlansCoverage' {} a -> s {attributes = a} :: SavingsPlansCoverage) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SavingsPlansCoverage where
  parseJSON =
    Core.withObject
      "SavingsPlansCoverage"
      ( \x ->
          SavingsPlansCoverage'
            Prelude.<$> (x Core..:? "TimePeriod")
            Prelude.<*> (x Core..:? "Coverage")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SavingsPlansCoverage where
  hashWithSalt salt' SavingsPlansCoverage' {..} =
    salt' `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` coverage
      `Prelude.hashWithSalt` timePeriod

instance Prelude.NFData SavingsPlansCoverage where
  rnf SavingsPlansCoverage' {..} =
    Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf coverage
