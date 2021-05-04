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
-- Module      : Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TerminateRecommendationDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on termination recommendation.
--
-- /See:/ 'newTerminateRecommendationDetail' smart constructor.
data TerminateRecommendationDetail = TerminateRecommendationDetail'
  { -- | Estimated savings resulting from modification, on a monthly basis.
    estimatedMonthlySavings :: Prelude.Maybe Prelude.Text,
    -- | The currency code that AWS used to calculate the costs for this
    -- instance.
    currencyCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      currencyCode = Prelude.Nothing
    }

-- | Estimated savings resulting from modification, on a monthly basis.
terminateRecommendationDetail_estimatedMonthlySavings :: Lens.Lens' TerminateRecommendationDetail (Prelude.Maybe Prelude.Text)
terminateRecommendationDetail_estimatedMonthlySavings = Lens.lens (\TerminateRecommendationDetail' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TerminateRecommendationDetail' {} a -> s {estimatedMonthlySavings = a} :: TerminateRecommendationDetail)

-- | The currency code that AWS used to calculate the costs for this
-- instance.
terminateRecommendationDetail_currencyCode :: Lens.Lens' TerminateRecommendationDetail (Prelude.Maybe Prelude.Text)
terminateRecommendationDetail_currencyCode = Lens.lens (\TerminateRecommendationDetail' {currencyCode} -> currencyCode) (\s@TerminateRecommendationDetail' {} a -> s {currencyCode = a} :: TerminateRecommendationDetail)

instance
  Prelude.FromJSON
    TerminateRecommendationDetail
  where
  parseJSON =
    Prelude.withObject
      "TerminateRecommendationDetail"
      ( \x ->
          TerminateRecommendationDetail'
            Prelude.<$> (x Prelude..:? "EstimatedMonthlySavings")
            Prelude.<*> (x Prelude..:? "CurrencyCode")
      )

instance
  Prelude.Hashable
    TerminateRecommendationDetail

instance Prelude.NFData TerminateRecommendationDetail
