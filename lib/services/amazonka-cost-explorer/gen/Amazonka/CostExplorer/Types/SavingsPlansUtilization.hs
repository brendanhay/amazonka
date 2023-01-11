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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansUtilization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The measurement of how well you\'re using your existing Savings Plans.
--
-- /See:/ 'newSavingsPlansUtilization' smart constructor.
data SavingsPlansUtilization = SavingsPlansUtilization'
  { -- | The total amount of Savings Plans commitment that\'s been purchased in
    -- an account (or set of accounts).
    totalCommitment :: Prelude.Maybe Prelude.Text,
    -- | The amount of your Savings Plans commitment that wasn\'t consumed from
    -- Savings Plans eligible usage in a specific period.
    unusedCommitment :: Prelude.Maybe Prelude.Text,
    -- | The amount of your Savings Plans commitment that was consumed from
    -- Savings Plans eligible usage in a specific period.
    usedCommitment :: Prelude.Maybe Prelude.Text,
    -- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
    -- Savings Plans.
    utilizationPercentage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalCommitment', 'savingsPlansUtilization_totalCommitment' - The total amount of Savings Plans commitment that\'s been purchased in
-- an account (or set of accounts).
--
-- 'unusedCommitment', 'savingsPlansUtilization_unusedCommitment' - The amount of your Savings Plans commitment that wasn\'t consumed from
-- Savings Plans eligible usage in a specific period.
--
-- 'usedCommitment', 'savingsPlansUtilization_usedCommitment' - The amount of your Savings Plans commitment that was consumed from
-- Savings Plans eligible usage in a specific period.
--
-- 'utilizationPercentage', 'savingsPlansUtilization_utilizationPercentage' - The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
-- Savings Plans.
newSavingsPlansUtilization ::
  SavingsPlansUtilization
newSavingsPlansUtilization =
  SavingsPlansUtilization'
    { totalCommitment =
        Prelude.Nothing,
      unusedCommitment = Prelude.Nothing,
      usedCommitment = Prelude.Nothing,
      utilizationPercentage = Prelude.Nothing
    }

-- | The total amount of Savings Plans commitment that\'s been purchased in
-- an account (or set of accounts).
savingsPlansUtilization_totalCommitment :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_totalCommitment = Lens.lens (\SavingsPlansUtilization' {totalCommitment} -> totalCommitment) (\s@SavingsPlansUtilization' {} a -> s {totalCommitment = a} :: SavingsPlansUtilization)

-- | The amount of your Savings Plans commitment that wasn\'t consumed from
-- Savings Plans eligible usage in a specific period.
savingsPlansUtilization_unusedCommitment :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_unusedCommitment = Lens.lens (\SavingsPlansUtilization' {unusedCommitment} -> unusedCommitment) (\s@SavingsPlansUtilization' {} a -> s {unusedCommitment = a} :: SavingsPlansUtilization)

-- | The amount of your Savings Plans commitment that was consumed from
-- Savings Plans eligible usage in a specific period.
savingsPlansUtilization_usedCommitment :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_usedCommitment = Lens.lens (\SavingsPlansUtilization' {usedCommitment} -> usedCommitment) (\s@SavingsPlansUtilization' {} a -> s {usedCommitment = a} :: SavingsPlansUtilization)

-- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
-- Savings Plans.
savingsPlansUtilization_utilizationPercentage :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_utilizationPercentage = Lens.lens (\SavingsPlansUtilization' {utilizationPercentage} -> utilizationPercentage) (\s@SavingsPlansUtilization' {} a -> s {utilizationPercentage = a} :: SavingsPlansUtilization)

instance Data.FromJSON SavingsPlansUtilization where
  parseJSON =
    Data.withObject
      "SavingsPlansUtilization"
      ( \x ->
          SavingsPlansUtilization'
            Prelude.<$> (x Data..:? "TotalCommitment")
            Prelude.<*> (x Data..:? "UnusedCommitment")
            Prelude.<*> (x Data..:? "UsedCommitment")
            Prelude.<*> (x Data..:? "UtilizationPercentage")
      )

instance Prelude.Hashable SavingsPlansUtilization where
  hashWithSalt _salt SavingsPlansUtilization' {..} =
    _salt `Prelude.hashWithSalt` totalCommitment
      `Prelude.hashWithSalt` unusedCommitment
      `Prelude.hashWithSalt` usedCommitment
      `Prelude.hashWithSalt` utilizationPercentage

instance Prelude.NFData SavingsPlansUtilization where
  rnf SavingsPlansUtilization' {..} =
    Prelude.rnf totalCommitment
      `Prelude.seq` Prelude.rnf unusedCommitment
      `Prelude.seq` Prelude.rnf usedCommitment
      `Prelude.seq` Prelude.rnf utilizationPercentage
