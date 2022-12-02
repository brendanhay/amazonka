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
-- Module      : Amazonka.CostExplorer.Types.TerminateRecommendationDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.TerminateRecommendationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on termination recommendation.
--
-- /See:/ 'newTerminateRecommendationDetail' smart constructor.
data TerminateRecommendationDetail = TerminateRecommendationDetail'
  { -- | The estimated savings that result from modification, on a monthly basis.
    estimatedMonthlySavings :: Prelude.Maybe Prelude.Text,
    -- | The currency code that Amazon Web Services used to calculate the costs
    -- for this instance.
    currencyCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavings', 'terminateRecommendationDetail_estimatedMonthlySavings' - The estimated savings that result from modification, on a monthly basis.
--
-- 'currencyCode', 'terminateRecommendationDetail_currencyCode' - The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
newTerminateRecommendationDetail ::
  TerminateRecommendationDetail
newTerminateRecommendationDetail =
  TerminateRecommendationDetail'
    { estimatedMonthlySavings =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing
    }

-- | The estimated savings that result from modification, on a monthly basis.
terminateRecommendationDetail_estimatedMonthlySavings :: Lens.Lens' TerminateRecommendationDetail (Prelude.Maybe Prelude.Text)
terminateRecommendationDetail_estimatedMonthlySavings = Lens.lens (\TerminateRecommendationDetail' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TerminateRecommendationDetail' {} a -> s {estimatedMonthlySavings = a} :: TerminateRecommendationDetail)

-- | The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
terminateRecommendationDetail_currencyCode :: Lens.Lens' TerminateRecommendationDetail (Prelude.Maybe Prelude.Text)
terminateRecommendationDetail_currencyCode = Lens.lens (\TerminateRecommendationDetail' {currencyCode} -> currencyCode) (\s@TerminateRecommendationDetail' {} a -> s {currencyCode = a} :: TerminateRecommendationDetail)

instance Data.FromJSON TerminateRecommendationDetail where
  parseJSON =
    Data.withObject
      "TerminateRecommendationDetail"
      ( \x ->
          TerminateRecommendationDetail'
            Prelude.<$> (x Data..:? "EstimatedMonthlySavings")
            Prelude.<*> (x Data..:? "CurrencyCode")
      )

instance
  Prelude.Hashable
    TerminateRecommendationDetail
  where
  hashWithSalt _salt TerminateRecommendationDetail' {..} =
    _salt
      `Prelude.hashWithSalt` estimatedMonthlySavings
      `Prelude.hashWithSalt` currencyCode

instance Prelude.NFData TerminateRecommendationDetail where
  rnf TerminateRecommendationDetail' {..} =
    Prelude.rnf estimatedMonthlySavings
      `Prelude.seq` Prelude.rnf currencyCode
