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
-- Module      : Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TerminateRecommendationDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details on termination recommendation.
--
-- /See:/ 'newTerminateRecommendationDetail' smart constructor.
data TerminateRecommendationDetail = TerminateRecommendationDetail'
  { -- | Estimated savings resulting from modification, on a monthly basis.
    estimatedMonthlySavings :: Core.Maybe Core.Text,
    -- | The currency code that AWS used to calculate the costs for this
    -- instance.
    currencyCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavings', 'terminateRecommendationDetail_estimatedMonthlySavings' - Estimated savings resulting from modification, on a monthly basis.
--
-- 'currencyCode', 'terminateRecommendationDetail_currencyCode' - The currency code that AWS used to calculate the costs for this
-- instance.
newTerminateRecommendationDetail ::
  TerminateRecommendationDetail
newTerminateRecommendationDetail =
  TerminateRecommendationDetail'
    { estimatedMonthlySavings =
        Core.Nothing,
      currencyCode = Core.Nothing
    }

-- | Estimated savings resulting from modification, on a monthly basis.
terminateRecommendationDetail_estimatedMonthlySavings :: Lens.Lens' TerminateRecommendationDetail (Core.Maybe Core.Text)
terminateRecommendationDetail_estimatedMonthlySavings = Lens.lens (\TerminateRecommendationDetail' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TerminateRecommendationDetail' {} a -> s {estimatedMonthlySavings = a} :: TerminateRecommendationDetail)

-- | The currency code that AWS used to calculate the costs for this
-- instance.
terminateRecommendationDetail_currencyCode :: Lens.Lens' TerminateRecommendationDetail (Core.Maybe Core.Text)
terminateRecommendationDetail_currencyCode = Lens.lens (\TerminateRecommendationDetail' {currencyCode} -> currencyCode) (\s@TerminateRecommendationDetail' {} a -> s {currencyCode = a} :: TerminateRecommendationDetail)

instance Core.FromJSON TerminateRecommendationDetail where
  parseJSON =
    Core.withObject
      "TerminateRecommendationDetail"
      ( \x ->
          TerminateRecommendationDetail'
            Core.<$> (x Core..:? "EstimatedMonthlySavings")
            Core.<*> (x Core..:? "CurrencyCode")
      )

instance Core.Hashable TerminateRecommendationDetail

instance Core.NFData TerminateRecommendationDetail
