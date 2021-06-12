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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The amortized amount of Savings Plans purchased in a specific account
-- during a specific time interval.
--
-- /See:/ 'newSavingsPlansAmortizedCommitment' smart constructor.
data SavingsPlansAmortizedCommitment = SavingsPlansAmortizedCommitment'
  { -- | The amortized amount of your Savings Plans commitment that was purchased
    -- with an @Upfront@ or @PartialUpfront@ Savings Plans.
    amortizedUpfrontCommitment :: Core.Maybe Core.Text,
    -- | The amortized amount of your Savings Plans commitment that was purchased
    -- with either a @Partial@ or a @NoUpfront@.
    amortizedRecurringCommitment :: Core.Maybe Core.Text,
    -- | The total amortized amount of your Savings Plans commitment, regardless
    -- of your Savings Plans purchase method.
    totalAmortizedCommitment :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SavingsPlansAmortizedCommitment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amortizedUpfrontCommitment', 'savingsPlansAmortizedCommitment_amortizedUpfrontCommitment' - The amortized amount of your Savings Plans commitment that was purchased
-- with an @Upfront@ or @PartialUpfront@ Savings Plans.
--
-- 'amortizedRecurringCommitment', 'savingsPlansAmortizedCommitment_amortizedRecurringCommitment' - The amortized amount of your Savings Plans commitment that was purchased
-- with either a @Partial@ or a @NoUpfront@.
--
-- 'totalAmortizedCommitment', 'savingsPlansAmortizedCommitment_totalAmortizedCommitment' - The total amortized amount of your Savings Plans commitment, regardless
-- of your Savings Plans purchase method.
newSavingsPlansAmortizedCommitment ::
  SavingsPlansAmortizedCommitment
newSavingsPlansAmortizedCommitment =
  SavingsPlansAmortizedCommitment'
    { amortizedUpfrontCommitment =
        Core.Nothing,
      amortizedRecurringCommitment =
        Core.Nothing,
      totalAmortizedCommitment = Core.Nothing
    }

-- | The amortized amount of your Savings Plans commitment that was purchased
-- with an @Upfront@ or @PartialUpfront@ Savings Plans.
savingsPlansAmortizedCommitment_amortizedUpfrontCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Core.Maybe Core.Text)
savingsPlansAmortizedCommitment_amortizedUpfrontCommitment = Lens.lens (\SavingsPlansAmortizedCommitment' {amortizedUpfrontCommitment} -> amortizedUpfrontCommitment) (\s@SavingsPlansAmortizedCommitment' {} a -> s {amortizedUpfrontCommitment = a} :: SavingsPlansAmortizedCommitment)

-- | The amortized amount of your Savings Plans commitment that was purchased
-- with either a @Partial@ or a @NoUpfront@.
savingsPlansAmortizedCommitment_amortizedRecurringCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Core.Maybe Core.Text)
savingsPlansAmortizedCommitment_amortizedRecurringCommitment = Lens.lens (\SavingsPlansAmortizedCommitment' {amortizedRecurringCommitment} -> amortizedRecurringCommitment) (\s@SavingsPlansAmortizedCommitment' {} a -> s {amortizedRecurringCommitment = a} :: SavingsPlansAmortizedCommitment)

-- | The total amortized amount of your Savings Plans commitment, regardless
-- of your Savings Plans purchase method.
savingsPlansAmortizedCommitment_totalAmortizedCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Core.Maybe Core.Text)
savingsPlansAmortizedCommitment_totalAmortizedCommitment = Lens.lens (\SavingsPlansAmortizedCommitment' {totalAmortizedCommitment} -> totalAmortizedCommitment) (\s@SavingsPlansAmortizedCommitment' {} a -> s {totalAmortizedCommitment = a} :: SavingsPlansAmortizedCommitment)

instance
  Core.FromJSON
    SavingsPlansAmortizedCommitment
  where
  parseJSON =
    Core.withObject
      "SavingsPlansAmortizedCommitment"
      ( \x ->
          SavingsPlansAmortizedCommitment'
            Core.<$> (x Core..:? "AmortizedUpfrontCommitment")
            Core.<*> (x Core..:? "AmortizedRecurringCommitment")
            Core.<*> (x Core..:? "TotalAmortizedCommitment")
      )

instance
  Core.Hashable
    SavingsPlansAmortizedCommitment

instance Core.NFData SavingsPlansAmortizedCommitment
