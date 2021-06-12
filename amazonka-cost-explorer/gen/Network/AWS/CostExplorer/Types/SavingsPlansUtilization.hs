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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The measurement of how well you are using your existing Savings Plans.
--
-- /See:/ 'newSavingsPlansUtilization' smart constructor.
data SavingsPlansUtilization = SavingsPlansUtilization'
  { -- | The amount of your Savings Plans commitment that was not consumed from
    -- Savings Plans eligible usage in a specific period.
    unusedCommitment :: Core.Maybe Core.Text,
    -- | The amount of your Savings Plans commitment that was consumed from
    -- Savings Plans eligible usage in a specific period.
    usedCommitment :: Core.Maybe Core.Text,
    -- | The total amount of Savings Plans commitment that\'s been purchased in
    -- an account (or set of accounts).
    totalCommitment :: Core.Maybe Core.Text,
    -- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
    -- Savings Plans.
    utilizationPercentage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      usedCommitment = Core.Nothing,
      totalCommitment = Core.Nothing,
      utilizationPercentage = Core.Nothing
    }

-- | The amount of your Savings Plans commitment that was not consumed from
-- Savings Plans eligible usage in a specific period.
savingsPlansUtilization_unusedCommitment :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Core.Text)
savingsPlansUtilization_unusedCommitment = Lens.lens (\SavingsPlansUtilization' {unusedCommitment} -> unusedCommitment) (\s@SavingsPlansUtilization' {} a -> s {unusedCommitment = a} :: SavingsPlansUtilization)

-- | The amount of your Savings Plans commitment that was consumed from
-- Savings Plans eligible usage in a specific period.
savingsPlansUtilization_usedCommitment :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Core.Text)
savingsPlansUtilization_usedCommitment = Lens.lens (\SavingsPlansUtilization' {usedCommitment} -> usedCommitment) (\s@SavingsPlansUtilization' {} a -> s {usedCommitment = a} :: SavingsPlansUtilization)

-- | The total amount of Savings Plans commitment that\'s been purchased in
-- an account (or set of accounts).
savingsPlansUtilization_totalCommitment :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Core.Text)
savingsPlansUtilization_totalCommitment = Lens.lens (\SavingsPlansUtilization' {totalCommitment} -> totalCommitment) (\s@SavingsPlansUtilization' {} a -> s {totalCommitment = a} :: SavingsPlansUtilization)

-- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your
-- Savings Plans.
savingsPlansUtilization_utilizationPercentage :: Lens.Lens' SavingsPlansUtilization (Core.Maybe Core.Text)
savingsPlansUtilization_utilizationPercentage = Lens.lens (\SavingsPlansUtilization' {utilizationPercentage} -> utilizationPercentage) (\s@SavingsPlansUtilization' {} a -> s {utilizationPercentage = a} :: SavingsPlansUtilization)

instance Core.FromJSON SavingsPlansUtilization where
  parseJSON =
    Core.withObject
      "SavingsPlansUtilization"
      ( \x ->
          SavingsPlansUtilization'
            Core.<$> (x Core..:? "UnusedCommitment")
            Core.<*> (x Core..:? "UsedCommitment")
            Core.<*> (x Core..:? "TotalCommitment")
            Core.<*> (x Core..:? "UtilizationPercentage")
      )

instance Core.Hashable SavingsPlansUtilization

instance Core.NFData SavingsPlansUtilization
