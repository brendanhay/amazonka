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
-- Module      : Network.AWS.Budgets.Types.CalculatedSpend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.CalculatedSpend where

import Network.AWS.Budgets.Types.Spend
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The spend objects that are associated with this budget. The
-- @actualSpend@ tracks how much you\'ve used, cost, usage, RI units, or
-- Savings Plans units and the @forecastedSpend@ tracks how much you are
-- predicted to spend based on your historical usage profile.
--
-- For example, if it is the 20th of the month and you have spent @50@
-- dollars on Amazon EC2, your @actualSpend@ is @50 USD@, and your
-- @forecastedSpend@ is @75 USD@.
--
-- /See:/ 'newCalculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { -- | The amount of cost, usage, RI units, or Savings Plans units that you are
    -- forecasted to use.
    forecastedSpend :: Prelude.Maybe Spend,
    -- | The amount of cost, usage, RI units, or Savings Plans units that you
    -- have used.
    actualSpend :: Spend
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CalculatedSpend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastedSpend', 'calculatedSpend_forecastedSpend' - The amount of cost, usage, RI units, or Savings Plans units that you are
-- forecasted to use.
--
-- 'actualSpend', 'calculatedSpend_actualSpend' - The amount of cost, usage, RI units, or Savings Plans units that you
-- have used.
newCalculatedSpend ::
  -- | 'actualSpend'
  Spend ->
  CalculatedSpend
newCalculatedSpend pActualSpend_ =
  CalculatedSpend'
    { forecastedSpend = Prelude.Nothing,
      actualSpend = pActualSpend_
    }

-- | The amount of cost, usage, RI units, or Savings Plans units that you are
-- forecasted to use.
calculatedSpend_forecastedSpend :: Lens.Lens' CalculatedSpend (Prelude.Maybe Spend)
calculatedSpend_forecastedSpend = Lens.lens (\CalculatedSpend' {forecastedSpend} -> forecastedSpend) (\s@CalculatedSpend' {} a -> s {forecastedSpend = a} :: CalculatedSpend)

-- | The amount of cost, usage, RI units, or Savings Plans units that you
-- have used.
calculatedSpend_actualSpend :: Lens.Lens' CalculatedSpend Spend
calculatedSpend_actualSpend = Lens.lens (\CalculatedSpend' {actualSpend} -> actualSpend) (\s@CalculatedSpend' {} a -> s {actualSpend = a} :: CalculatedSpend)

instance Prelude.FromJSON CalculatedSpend where
  parseJSON =
    Prelude.withObject
      "CalculatedSpend"
      ( \x ->
          CalculatedSpend'
            Prelude.<$> (x Prelude..:? "ForecastedSpend")
            Prelude.<*> (x Prelude..: "ActualSpend")
      )

instance Prelude.Hashable CalculatedSpend

instance Prelude.NFData CalculatedSpend

instance Prelude.ToJSON CalculatedSpend where
  toJSON CalculatedSpend' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ForecastedSpend" Prelude..=)
              Prelude.<$> forecastedSpend,
            Prelude.Just ("ActualSpend" Prelude..= actualSpend)
          ]
      )
