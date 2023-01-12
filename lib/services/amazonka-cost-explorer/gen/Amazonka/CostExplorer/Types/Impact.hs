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
-- Module      : Amazonka.CostExplorer.Types.Impact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Impact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The dollar value of the anomaly.
--
-- /See:/ 'newImpact' smart constructor.
data Impact = Impact'
  { -- | The cumulative dollar amount that was actually spent during the anomaly.
    totalActualSpend :: Prelude.Maybe Prelude.Double,
    -- | The cumulative dollar amount that was expected to be spent during the
    -- anomaly. It is calculated using advanced machine learning models to
    -- determine the typical spending pattern based on historical data for a
    -- customer.
    totalExpectedSpend :: Prelude.Maybe Prelude.Double,
    -- | The cumulative dollar difference between the total actual spend and
    -- total expected spend. It is calculated as
    -- @TotalActualSpend - TotalExpectedSpend@.
    totalImpact :: Prelude.Maybe Prelude.Double,
    -- | The cumulative percentage difference between the total actual spend and
    -- total expected spend. It is calculated as
    -- @(TotalImpact \/ TotalExpectedSpend) * 100@. When @TotalExpectedSpend@
    -- is zero, this field is omitted. Expected spend can be zero in situations
    -- such as when you start to use a service for the first time.
    totalImpactPercentage :: Prelude.Maybe Prelude.Double,
    -- | The maximum dollar value that\'s observed for an anomaly.
    maxImpact :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Impact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalActualSpend', 'impact_totalActualSpend' - The cumulative dollar amount that was actually spent during the anomaly.
--
-- 'totalExpectedSpend', 'impact_totalExpectedSpend' - The cumulative dollar amount that was expected to be spent during the
-- anomaly. It is calculated using advanced machine learning models to
-- determine the typical spending pattern based on historical data for a
-- customer.
--
-- 'totalImpact', 'impact_totalImpact' - The cumulative dollar difference between the total actual spend and
-- total expected spend. It is calculated as
-- @TotalActualSpend - TotalExpectedSpend@.
--
-- 'totalImpactPercentage', 'impact_totalImpactPercentage' - The cumulative percentage difference between the total actual spend and
-- total expected spend. It is calculated as
-- @(TotalImpact \/ TotalExpectedSpend) * 100@. When @TotalExpectedSpend@
-- is zero, this field is omitted. Expected spend can be zero in situations
-- such as when you start to use a service for the first time.
--
-- 'maxImpact', 'impact_maxImpact' - The maximum dollar value that\'s observed for an anomaly.
newImpact ::
  -- | 'maxImpact'
  Prelude.Double ->
  Impact
newImpact pMaxImpact_ =
  Impact'
    { totalActualSpend = Prelude.Nothing,
      totalExpectedSpend = Prelude.Nothing,
      totalImpact = Prelude.Nothing,
      totalImpactPercentage = Prelude.Nothing,
      maxImpact = pMaxImpact_
    }

-- | The cumulative dollar amount that was actually spent during the anomaly.
impact_totalActualSpend :: Lens.Lens' Impact (Prelude.Maybe Prelude.Double)
impact_totalActualSpend = Lens.lens (\Impact' {totalActualSpend} -> totalActualSpend) (\s@Impact' {} a -> s {totalActualSpend = a} :: Impact)

-- | The cumulative dollar amount that was expected to be spent during the
-- anomaly. It is calculated using advanced machine learning models to
-- determine the typical spending pattern based on historical data for a
-- customer.
impact_totalExpectedSpend :: Lens.Lens' Impact (Prelude.Maybe Prelude.Double)
impact_totalExpectedSpend = Lens.lens (\Impact' {totalExpectedSpend} -> totalExpectedSpend) (\s@Impact' {} a -> s {totalExpectedSpend = a} :: Impact)

-- | The cumulative dollar difference between the total actual spend and
-- total expected spend. It is calculated as
-- @TotalActualSpend - TotalExpectedSpend@.
impact_totalImpact :: Lens.Lens' Impact (Prelude.Maybe Prelude.Double)
impact_totalImpact = Lens.lens (\Impact' {totalImpact} -> totalImpact) (\s@Impact' {} a -> s {totalImpact = a} :: Impact)

-- | The cumulative percentage difference between the total actual spend and
-- total expected spend. It is calculated as
-- @(TotalImpact \/ TotalExpectedSpend) * 100@. When @TotalExpectedSpend@
-- is zero, this field is omitted. Expected spend can be zero in situations
-- such as when you start to use a service for the first time.
impact_totalImpactPercentage :: Lens.Lens' Impact (Prelude.Maybe Prelude.Double)
impact_totalImpactPercentage = Lens.lens (\Impact' {totalImpactPercentage} -> totalImpactPercentage) (\s@Impact' {} a -> s {totalImpactPercentage = a} :: Impact)

-- | The maximum dollar value that\'s observed for an anomaly.
impact_maxImpact :: Lens.Lens' Impact Prelude.Double
impact_maxImpact = Lens.lens (\Impact' {maxImpact} -> maxImpact) (\s@Impact' {} a -> s {maxImpact = a} :: Impact)

instance Data.FromJSON Impact where
  parseJSON =
    Data.withObject
      "Impact"
      ( \x ->
          Impact'
            Prelude.<$> (x Data..:? "TotalActualSpend")
            Prelude.<*> (x Data..:? "TotalExpectedSpend")
            Prelude.<*> (x Data..:? "TotalImpact")
            Prelude.<*> (x Data..:? "TotalImpactPercentage")
            Prelude.<*> (x Data..: "MaxImpact")
      )

instance Prelude.Hashable Impact where
  hashWithSalt _salt Impact' {..} =
    _salt `Prelude.hashWithSalt` totalActualSpend
      `Prelude.hashWithSalt` totalExpectedSpend
      `Prelude.hashWithSalt` totalImpact
      `Prelude.hashWithSalt` totalImpactPercentage
      `Prelude.hashWithSalt` maxImpact

instance Prelude.NFData Impact where
  rnf Impact' {..} =
    Prelude.rnf totalActualSpend
      `Prelude.seq` Prelude.rnf totalExpectedSpend
      `Prelude.seq` Prelude.rnf totalImpact
      `Prelude.seq` Prelude.rnf totalImpactPercentage
      `Prelude.seq` Prelude.rnf maxImpact
