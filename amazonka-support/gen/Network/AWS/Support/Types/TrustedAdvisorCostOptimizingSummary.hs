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
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    TrustedAdvisorCostOptimizingSummary
  where
  parseJSON =
    Prelude.withObject
      "TrustedAdvisorCostOptimizingSummary"
      ( \x ->
          TrustedAdvisorCostOptimizingSummary'
            Prelude.<$> (x Prelude..: "estimatedMonthlySavings")
            Prelude.<*> (x Prelude..: "estimatedPercentMonthlySavings")
      )

instance
  Prelude.Hashable
    TrustedAdvisorCostOptimizingSummary

instance
  Prelude.NFData
    TrustedAdvisorCostOptimizingSummary
