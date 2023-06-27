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
-- Module      : Amazonka.Lightsail.Types.EstimateByTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.EstimateByTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.Currency
import Amazonka.Lightsail.Types.PricingUnit
import Amazonka.Lightsail.Types.TimePeriod
import qualified Amazonka.Prelude as Prelude

-- | An estimate that\'s associated with a time period.
--
-- /See:/ 'newEstimateByTime' smart constructor.
data EstimateByTime = EstimateByTime'
  { -- | The currency of the estimate in USD.
    currency :: Prelude.Maybe Currency,
    -- | The unit of measurement that\'s used for the cost estimate.
    pricingUnit :: Prelude.Maybe PricingUnit,
    -- | The period of time, in days, that an estimate covers. The period has a
    -- start date and an end date. The start date must come before the end
    -- date.
    timePeriod :: Prelude.Maybe TimePeriod,
    -- | The number of pricing units used to calculate the total number of hours.
    -- For example, 1 unit equals 1 hour.
    unit :: Prelude.Maybe Prelude.Double,
    -- | The amount of cost or usage that\'s measured for the cost estimate.
    usageCost :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EstimateByTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currency', 'estimateByTime_currency' - The currency of the estimate in USD.
--
-- 'pricingUnit', 'estimateByTime_pricingUnit' - The unit of measurement that\'s used for the cost estimate.
--
-- 'timePeriod', 'estimateByTime_timePeriod' - The period of time, in days, that an estimate covers. The period has a
-- start date and an end date. The start date must come before the end
-- date.
--
-- 'unit', 'estimateByTime_unit' - The number of pricing units used to calculate the total number of hours.
-- For example, 1 unit equals 1 hour.
--
-- 'usageCost', 'estimateByTime_usageCost' - The amount of cost or usage that\'s measured for the cost estimate.
newEstimateByTime ::
  EstimateByTime
newEstimateByTime =
  EstimateByTime'
    { currency = Prelude.Nothing,
      pricingUnit = Prelude.Nothing,
      timePeriod = Prelude.Nothing,
      unit = Prelude.Nothing,
      usageCost = Prelude.Nothing
    }

-- | The currency of the estimate in USD.
estimateByTime_currency :: Lens.Lens' EstimateByTime (Prelude.Maybe Currency)
estimateByTime_currency = Lens.lens (\EstimateByTime' {currency} -> currency) (\s@EstimateByTime' {} a -> s {currency = a} :: EstimateByTime)

-- | The unit of measurement that\'s used for the cost estimate.
estimateByTime_pricingUnit :: Lens.Lens' EstimateByTime (Prelude.Maybe PricingUnit)
estimateByTime_pricingUnit = Lens.lens (\EstimateByTime' {pricingUnit} -> pricingUnit) (\s@EstimateByTime' {} a -> s {pricingUnit = a} :: EstimateByTime)

-- | The period of time, in days, that an estimate covers. The period has a
-- start date and an end date. The start date must come before the end
-- date.
estimateByTime_timePeriod :: Lens.Lens' EstimateByTime (Prelude.Maybe TimePeriod)
estimateByTime_timePeriod = Lens.lens (\EstimateByTime' {timePeriod} -> timePeriod) (\s@EstimateByTime' {} a -> s {timePeriod = a} :: EstimateByTime)

-- | The number of pricing units used to calculate the total number of hours.
-- For example, 1 unit equals 1 hour.
estimateByTime_unit :: Lens.Lens' EstimateByTime (Prelude.Maybe Prelude.Double)
estimateByTime_unit = Lens.lens (\EstimateByTime' {unit} -> unit) (\s@EstimateByTime' {} a -> s {unit = a} :: EstimateByTime)

-- | The amount of cost or usage that\'s measured for the cost estimate.
estimateByTime_usageCost :: Lens.Lens' EstimateByTime (Prelude.Maybe Prelude.Double)
estimateByTime_usageCost = Lens.lens (\EstimateByTime' {usageCost} -> usageCost) (\s@EstimateByTime' {} a -> s {usageCost = a} :: EstimateByTime)

instance Data.FromJSON EstimateByTime where
  parseJSON =
    Data.withObject
      "EstimateByTime"
      ( \x ->
          EstimateByTime'
            Prelude.<$> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "pricingUnit")
            Prelude.<*> (x Data..:? "timePeriod")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..:? "usageCost")
      )

instance Prelude.Hashable EstimateByTime where
  hashWithSalt _salt EstimateByTime' {..} =
    _salt
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` pricingUnit
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` usageCost

instance Prelude.NFData EstimateByTime where
  rnf EstimateByTime' {..} =
    Prelude.rnf currency
      `Prelude.seq` Prelude.rnf pricingUnit
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf usageCost
