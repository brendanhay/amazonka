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
-- Module      : Amazonka.Support.Types.TrustedAdvisorCostOptimizingSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorCostOptimizingSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The estimated cost savings that might be realized if the recommended
-- operations are taken.
--
-- /See:/ 'newTrustedAdvisorCostOptimizingSummary' smart constructor.
data TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary'
  { -- | The estimated monthly savings that might be realized if the recommended
    -- operations are taken.
    estimatedMonthlySavings :: Prelude.Double,
    -- | The estimated percentage of savings that might be realized if the
    -- recommended operations are taken.
    estimatedPercentMonthlySavings :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorCostOptimizingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavings', 'trustedAdvisorCostOptimizingSummary_estimatedMonthlySavings' - The estimated monthly savings that might be realized if the recommended
-- operations are taken.
--
-- 'estimatedPercentMonthlySavings', 'trustedAdvisorCostOptimizingSummary_estimatedPercentMonthlySavings' - The estimated percentage of savings that might be realized if the
-- recommended operations are taken.
newTrustedAdvisorCostOptimizingSummary ::
  -- | 'estimatedMonthlySavings'
  Prelude.Double ->
  -- | 'estimatedPercentMonthlySavings'
  Prelude.Double ->
  TrustedAdvisorCostOptimizingSummary
newTrustedAdvisorCostOptimizingSummary
  pEstimatedMonthlySavings_
  pEstimatedPercentMonthlySavings_ =
    TrustedAdvisorCostOptimizingSummary'
      { estimatedMonthlySavings =
          pEstimatedMonthlySavings_,
        estimatedPercentMonthlySavings =
          pEstimatedPercentMonthlySavings_
      }

-- | The estimated monthly savings that might be realized if the recommended
-- operations are taken.
trustedAdvisorCostOptimizingSummary_estimatedMonthlySavings :: Lens.Lens' TrustedAdvisorCostOptimizingSummary Prelude.Double
trustedAdvisorCostOptimizingSummary_estimatedMonthlySavings = Lens.lens (\TrustedAdvisorCostOptimizingSummary' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TrustedAdvisorCostOptimizingSummary' {} a -> s {estimatedMonthlySavings = a} :: TrustedAdvisorCostOptimizingSummary)

-- | The estimated percentage of savings that might be realized if the
-- recommended operations are taken.
trustedAdvisorCostOptimizingSummary_estimatedPercentMonthlySavings :: Lens.Lens' TrustedAdvisorCostOptimizingSummary Prelude.Double
trustedAdvisorCostOptimizingSummary_estimatedPercentMonthlySavings = Lens.lens (\TrustedAdvisorCostOptimizingSummary' {estimatedPercentMonthlySavings} -> estimatedPercentMonthlySavings) (\s@TrustedAdvisorCostOptimizingSummary' {} a -> s {estimatedPercentMonthlySavings = a} :: TrustedAdvisorCostOptimizingSummary)

instance
  Core.FromJSON
    TrustedAdvisorCostOptimizingSummary
  where
  parseJSON =
    Core.withObject
      "TrustedAdvisorCostOptimizingSummary"
      ( \x ->
          TrustedAdvisorCostOptimizingSummary'
            Prelude.<$> (x Core..: "estimatedMonthlySavings")
            Prelude.<*> (x Core..: "estimatedPercentMonthlySavings")
      )

instance
  Prelude.Hashable
    TrustedAdvisorCostOptimizingSummary
  where
  hashWithSalt
    _salt
    TrustedAdvisorCostOptimizingSummary' {..} =
      _salt
        `Prelude.hashWithSalt` estimatedMonthlySavings
        `Prelude.hashWithSalt` estimatedPercentMonthlySavings

instance
  Prelude.NFData
    TrustedAdvisorCostOptimizingSummary
  where
  rnf TrustedAdvisorCostOptimizingSummary' {..} =
    Prelude.rnf estimatedMonthlySavings
      `Prelude.seq` Prelude.rnf estimatedPercentMonthlySavings
