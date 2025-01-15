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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansAmortizedCommitment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amortized amount of Savings Plans purchased in a specific account
-- during a specific time interval.
--
-- /See:/ 'newSavingsPlansAmortizedCommitment' smart constructor.
data SavingsPlansAmortizedCommitment = SavingsPlansAmortizedCommitment'
  { -- | The amortized amount of your Savings Plans commitment that was purchased
    -- with either a @Partial@ or a @NoUpfront@.
    amortizedRecurringCommitment :: Prelude.Maybe Prelude.Text,
    -- | The amortized amount of your Savings Plans commitment that was purchased
    -- with an @Upfront@ or @PartialUpfront@ Savings Plans.
    amortizedUpfrontCommitment :: Prelude.Maybe Prelude.Text,
    -- | The total amortized amount of your Savings Plans commitment, regardless
    -- of your Savings Plans purchase method.
    totalAmortizedCommitment :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansAmortizedCommitment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amortizedRecurringCommitment', 'savingsPlansAmortizedCommitment_amortizedRecurringCommitment' - The amortized amount of your Savings Plans commitment that was purchased
-- with either a @Partial@ or a @NoUpfront@.
--
-- 'amortizedUpfrontCommitment', 'savingsPlansAmortizedCommitment_amortizedUpfrontCommitment' - The amortized amount of your Savings Plans commitment that was purchased
-- with an @Upfront@ or @PartialUpfront@ Savings Plans.
--
-- 'totalAmortizedCommitment', 'savingsPlansAmortizedCommitment_totalAmortizedCommitment' - The total amortized amount of your Savings Plans commitment, regardless
-- of your Savings Plans purchase method.
newSavingsPlansAmortizedCommitment ::
  SavingsPlansAmortizedCommitment
newSavingsPlansAmortizedCommitment =
  SavingsPlansAmortizedCommitment'
    { amortizedRecurringCommitment =
        Prelude.Nothing,
      amortizedUpfrontCommitment =
        Prelude.Nothing,
      totalAmortizedCommitment = Prelude.Nothing
    }

-- | The amortized amount of your Savings Plans commitment that was purchased
-- with either a @Partial@ or a @NoUpfront@.
savingsPlansAmortizedCommitment_amortizedRecurringCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Prelude.Maybe Prelude.Text)
savingsPlansAmortizedCommitment_amortizedRecurringCommitment = Lens.lens (\SavingsPlansAmortizedCommitment' {amortizedRecurringCommitment} -> amortizedRecurringCommitment) (\s@SavingsPlansAmortizedCommitment' {} a -> s {amortizedRecurringCommitment = a} :: SavingsPlansAmortizedCommitment)

-- | The amortized amount of your Savings Plans commitment that was purchased
-- with an @Upfront@ or @PartialUpfront@ Savings Plans.
savingsPlansAmortizedCommitment_amortizedUpfrontCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Prelude.Maybe Prelude.Text)
savingsPlansAmortizedCommitment_amortizedUpfrontCommitment = Lens.lens (\SavingsPlansAmortizedCommitment' {amortizedUpfrontCommitment} -> amortizedUpfrontCommitment) (\s@SavingsPlansAmortizedCommitment' {} a -> s {amortizedUpfrontCommitment = a} :: SavingsPlansAmortizedCommitment)

-- | The total amortized amount of your Savings Plans commitment, regardless
-- of your Savings Plans purchase method.
savingsPlansAmortizedCommitment_totalAmortizedCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Prelude.Maybe Prelude.Text)
savingsPlansAmortizedCommitment_totalAmortizedCommitment = Lens.lens (\SavingsPlansAmortizedCommitment' {totalAmortizedCommitment} -> totalAmortizedCommitment) (\s@SavingsPlansAmortizedCommitment' {} a -> s {totalAmortizedCommitment = a} :: SavingsPlansAmortizedCommitment)

instance
  Data.FromJSON
    SavingsPlansAmortizedCommitment
  where
  parseJSON =
    Data.withObject
      "SavingsPlansAmortizedCommitment"
      ( \x ->
          SavingsPlansAmortizedCommitment'
            Prelude.<$> (x Data..:? "AmortizedRecurringCommitment")
            Prelude.<*> (x Data..:? "AmortizedUpfrontCommitment")
            Prelude.<*> (x Data..:? "TotalAmortizedCommitment")
      )

instance
  Prelude.Hashable
    SavingsPlansAmortizedCommitment
  where
  hashWithSalt
    _salt
    SavingsPlansAmortizedCommitment' {..} =
      _salt
        `Prelude.hashWithSalt` amortizedRecurringCommitment
        `Prelude.hashWithSalt` amortizedUpfrontCommitment
        `Prelude.hashWithSalt` totalAmortizedCommitment

instance
  Prelude.NFData
    SavingsPlansAmortizedCommitment
  where
  rnf SavingsPlansAmortizedCommitment' {..} =
    Prelude.rnf amortizedRecurringCommitment `Prelude.seq`
      Prelude.rnf amortizedUpfrontCommitment `Prelude.seq`
        Prelude.rnf totalAmortizedCommitment
