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
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilization where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The measurement of how well you are using your existing Savings Plans.
--
-- /See:/ 'newSavingsPlansUtilization' smart constructor.
data SavingsPlansUtilization = SavingsPlansUtilization'
  { -- | The amount of your Savings Plans commitment that was not consumed from
    -- Savings Plans eligible usage in a specific period.
    unusedCommitment :: Prelude.Maybe Prelude.Text,
    -- | The amount of your Savings Plans commitment that was consumed from
    -- Savings Plans eligible usage in a specific period.
    usedCommitment :: Prelude.Maybe Prelude.Text,
    -- | The total amount of Savings Plans commitment that\'s been purchased in
    -- an account (or set of accounts).
    totalCommitment :: Prelude.Maybe Prelude.Text,
    -- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
    -- Savings Plans.
    utilizationPercentage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unusedCommitment', 'savingsPlansUtilization_unusedCommitment' - The amount of your Savings Plans commitment that was not consumed from
-- Savings Plans eligible usage in a specific period.
--
-- 'usedCommitment', 'savingsPlansUtilization_usedCommitment' - The amount of your Savings Plans commitment that was consumed from
-- Savings Plans eligible usage in a specific period.
--
-- 'totalCommitment', 'savingsPlansUtilization_totalCommitment' - The total amount of Savings Plans commitment that\'s been purchased in
-- an account (or set of accounts).
--
-- 'utilizationPercentage', 'savingsPlansUtilization_utilizationPercentage' - The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
-- Savings Plans.
newSavingsPlansUtilization ::
  SavingsPlansUtilization
newSavingsPlansUtilization =
  SavingsPlansUtilization'
    { unusedCommitment =
        Prelude.Nothing,
      usedCommitment = Prelude.Nothing,
      totalCommitment = Prelude.Nothing,
      utilizationPercentage = Prelude.Nothing
    }

-- | The amount of your Savings Plans commitment that was not consumed from
-- Savings Plans eligible usage in a specific period.
savingsPlansUtilization_unusedCommitment :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_unusedCommitment = Lens.lens (\SavingsPlansUtilization' {unusedCommitment} -> unusedCommitment) (\s@SavingsPlansUtilization' {} a -> s {unusedCommitment = a} :: SavingsPlansUtilization)

-- | The amount of your Savings Plans commitment that was consumed from
-- Savings Plans eligible usage in a specific period.
savingsPlansUtilization_usedCommitment :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_usedCommitment = Lens.lens (\SavingsPlansUtilization' {usedCommitment} -> usedCommitment) (\s@SavingsPlansUtilization' {} a -> s {usedCommitment = a} :: SavingsPlansUtilization)

-- | The total amount of Savings Plans commitment that\'s been purchased in
-- an account (or set of accounts).
savingsPlansUtilization_totalCommitment :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_totalCommitment = Lens.lens (\SavingsPlansUtilization' {totalCommitment} -> totalCommitment) (\s@SavingsPlansUtilization' {} a -> s {totalCommitment = a} :: SavingsPlansUtilization)

-- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
-- Savings Plans.
savingsPlansUtilization_utilizationPercentage :: Lens.Lens' SavingsPlansUtilization (Prelude.Maybe Prelude.Text)
savingsPlansUtilization_utilizationPercentage = Lens.lens (\SavingsPlansUtilization' {utilizationPercentage} -> utilizationPercentage) (\s@SavingsPlansUtilization' {} a -> s {utilizationPercentage = a} :: SavingsPlansUtilization)

instance Prelude.FromJSON SavingsPlansUtilization where
  parseJSON =
    Prelude.withObject
      "SavingsPlansUtilization"
      ( \x ->
          SavingsPlansUtilization'
            Prelude.<$> (x Prelude..:? "UnusedCommitment")
            Prelude.<*> (x Prelude..:? "UsedCommitment")
            Prelude.<*> (x Prelude..:? "TotalCommitment")
            Prelude.<*> (x Prelude..:? "UtilizationPercentage")
      )

instance Prelude.Hashable SavingsPlansUtilization

instance Prelude.NFData SavingsPlansUtilization
