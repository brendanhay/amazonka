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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverage where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The amount of Savings Plans eligible usage that is covered by Savings
-- Plans. All calculations consider the On-Demand equivalent of your
-- Savings Plans usage.
--
-- /See:/ 'newSavingsPlansCoverage' smart constructor.
data SavingsPlansCoverage = SavingsPlansCoverage'
  { timePeriod :: Prelude.Maybe DateInterval,
    -- | The attribute that applies to a specific @Dimension@.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The amount of Savings Plans eligible usage that the Savings Plans
    -- covered.
    coverage :: Prelude.Maybe SavingsPlansCoverageData
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
-- 'attributes', 'savingsPlansCoverage_attributes' - The attribute that applies to a specific @Dimension@.
--
-- 'coverage', 'savingsPlansCoverage_coverage' - The amount of Savings Plans eligible usage that the Savings Plans
-- covered.
newSavingsPlansCoverage ::
  SavingsPlansCoverage
newSavingsPlansCoverage =
  SavingsPlansCoverage'
    { timePeriod = Prelude.Nothing,
      attributes = Prelude.Nothing,
      coverage = Prelude.Nothing
    }

-- | Undocumented member.
savingsPlansCoverage_timePeriod :: Lens.Lens' SavingsPlansCoverage (Prelude.Maybe DateInterval)
savingsPlansCoverage_timePeriod = Lens.lens (\SavingsPlansCoverage' {timePeriod} -> timePeriod) (\s@SavingsPlansCoverage' {} a -> s {timePeriod = a} :: SavingsPlansCoverage)

-- | The attribute that applies to a specific @Dimension@.
savingsPlansCoverage_attributes :: Lens.Lens' SavingsPlansCoverage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
savingsPlansCoverage_attributes = Lens.lens (\SavingsPlansCoverage' {attributes} -> attributes) (\s@SavingsPlansCoverage' {} a -> s {attributes = a} :: SavingsPlansCoverage) Prelude.. Lens.mapping Lens._Coerce

-- | The amount of Savings Plans eligible usage that the Savings Plans
-- covered.
savingsPlansCoverage_coverage :: Lens.Lens' SavingsPlansCoverage (Prelude.Maybe SavingsPlansCoverageData)
savingsPlansCoverage_coverage = Lens.lens (\SavingsPlansCoverage' {coverage} -> coverage) (\s@SavingsPlansCoverage' {} a -> s {coverage = a} :: SavingsPlansCoverage)

instance Core.FromJSON SavingsPlansCoverage where
  parseJSON =
    Core.withObject
      "SavingsPlansCoverage"
      ( \x ->
          SavingsPlansCoverage'
            Prelude.<$> (x Core..:? "TimePeriod")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Coverage")
      )

instance Prelude.Hashable SavingsPlansCoverage

instance Prelude.NFData SavingsPlansCoverage
