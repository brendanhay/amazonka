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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansSavings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansSavings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The amount of savings that you\'re accumulating, against the public
-- On-Demand rate of the usage accrued in an account.
--
-- /See:/ 'newSavingsPlansSavings' smart constructor.
data SavingsPlansSavings = SavingsPlansSavings'
  { -- | The savings amount that you\'re accumulating for the usage that\'s
    -- covered by a Savings Plans, when compared to the On-Demand equivalent of
    -- the same usage.
    netSavings :: Prelude.Maybe Prelude.Text,
    -- | How much the amount that the usage would have cost if it was accrued at
    -- the On-Demand rate.
    onDemandCostEquivalent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansSavings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'netSavings', 'savingsPlansSavings_netSavings' - The savings amount that you\'re accumulating for the usage that\'s
-- covered by a Savings Plans, when compared to the On-Demand equivalent of
-- the same usage.
--
-- 'onDemandCostEquivalent', 'savingsPlansSavings_onDemandCostEquivalent' - How much the amount that the usage would have cost if it was accrued at
-- the On-Demand rate.
newSavingsPlansSavings ::
  SavingsPlansSavings
newSavingsPlansSavings =
  SavingsPlansSavings'
    { netSavings = Prelude.Nothing,
      onDemandCostEquivalent = Prelude.Nothing
    }

-- | The savings amount that you\'re accumulating for the usage that\'s
-- covered by a Savings Plans, when compared to the On-Demand equivalent of
-- the same usage.
savingsPlansSavings_netSavings :: Lens.Lens' SavingsPlansSavings (Prelude.Maybe Prelude.Text)
savingsPlansSavings_netSavings = Lens.lens (\SavingsPlansSavings' {netSavings} -> netSavings) (\s@SavingsPlansSavings' {} a -> s {netSavings = a} :: SavingsPlansSavings)

-- | How much the amount that the usage would have cost if it was accrued at
-- the On-Demand rate.
savingsPlansSavings_onDemandCostEquivalent :: Lens.Lens' SavingsPlansSavings (Prelude.Maybe Prelude.Text)
savingsPlansSavings_onDemandCostEquivalent = Lens.lens (\SavingsPlansSavings' {onDemandCostEquivalent} -> onDemandCostEquivalent) (\s@SavingsPlansSavings' {} a -> s {onDemandCostEquivalent = a} :: SavingsPlansSavings)

instance Core.FromJSON SavingsPlansSavings where
  parseJSON =
    Core.withObject
      "SavingsPlansSavings"
      ( \x ->
          SavingsPlansSavings'
            Prelude.<$> (x Core..:? "NetSavings")
            Prelude.<*> (x Core..:? "OnDemandCostEquivalent")
      )

instance Prelude.Hashable SavingsPlansSavings where
  hashWithSalt _salt SavingsPlansSavings' {..} =
    _salt `Prelude.hashWithSalt` netSavings
      `Prelude.hashWithSalt` onDemandCostEquivalent

instance Prelude.NFData SavingsPlansSavings where
  rnf SavingsPlansSavings' {..} =
    Prelude.rnf netSavings
      `Prelude.seq` Prelude.rnf onDemandCostEquivalent
