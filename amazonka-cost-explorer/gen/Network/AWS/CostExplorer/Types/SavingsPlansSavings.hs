{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansSavings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansSavings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The amount of savings you\'re accumulating, against the public On-Demand
-- rate of the usage accrued in an account.
--
-- /See:/ 'newSavingsPlansSavings' smart constructor.
data SavingsPlansSavings = SavingsPlansSavings'
  { -- | How much the amount that the usage would have cost if it was accrued at
    -- the On-Demand rate.
    onDemandCostEquivalent :: Prelude.Maybe Prelude.Text,
    -- | The savings amount that you are accumulating for the usage that is
    -- covered by a Savings Plans, when compared to the On-Demand equivalent of
    -- the same usage.
    netSavings :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansSavings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandCostEquivalent', 'savingsPlansSavings_onDemandCostEquivalent' - How much the amount that the usage would have cost if it was accrued at
-- the On-Demand rate.
--
-- 'netSavings', 'savingsPlansSavings_netSavings' - The savings amount that you are accumulating for the usage that is
-- covered by a Savings Plans, when compared to the On-Demand equivalent of
-- the same usage.
newSavingsPlansSavings ::
  SavingsPlansSavings
newSavingsPlansSavings =
  SavingsPlansSavings'
    { onDemandCostEquivalent =
        Prelude.Nothing,
      netSavings = Prelude.Nothing
    }

-- | How much the amount that the usage would have cost if it was accrued at
-- the On-Demand rate.
savingsPlansSavings_onDemandCostEquivalent :: Lens.Lens' SavingsPlansSavings (Prelude.Maybe Prelude.Text)
savingsPlansSavings_onDemandCostEquivalent = Lens.lens (\SavingsPlansSavings' {onDemandCostEquivalent} -> onDemandCostEquivalent) (\s@SavingsPlansSavings' {} a -> s {onDemandCostEquivalent = a} :: SavingsPlansSavings)

-- | The savings amount that you are accumulating for the usage that is
-- covered by a Savings Plans, when compared to the On-Demand equivalent of
-- the same usage.
savingsPlansSavings_netSavings :: Lens.Lens' SavingsPlansSavings (Prelude.Maybe Prelude.Text)
savingsPlansSavings_netSavings = Lens.lens (\SavingsPlansSavings' {netSavings} -> netSavings) (\s@SavingsPlansSavings' {} a -> s {netSavings = a} :: SavingsPlansSavings)

instance Prelude.FromJSON SavingsPlansSavings where
  parseJSON =
    Prelude.withObject
      "SavingsPlansSavings"
      ( \x ->
          SavingsPlansSavings'
            Prelude.<$> (x Prelude..:? "OnDemandCostEquivalent")
            Prelude.<*> (x Prelude..:? "NetSavings")
      )

instance Prelude.Hashable SavingsPlansSavings

instance Prelude.NFData SavingsPlansSavings
