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
-- Module      : Amazonka.Budgets.Types.CalculatedSpend
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.CalculatedSpend where

import Amazonka.Budgets.Types.Spend
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The spend objects that are associated with this budget. The
-- @actualSpend@ tracks how much you\'ve used, cost, usage, RI units, or
-- Savings Plans units and the @forecastedSpend@ tracks how much that
-- you\'re predicted to spend based on your historical usage profile.
--
-- For example, if it\'s the 20th of the month and you have spent @50@
-- dollars on Amazon EC2, your @actualSpend@ is @50 USD@, and your
-- @forecastedSpend@ is @75 USD@.
--
-- /See:/ 'newCalculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { -- | The amount of cost, usage, RI units, or Savings Plans units that you\'re
    -- forecasted to use.
    forecastedSpend :: Prelude.Maybe Spend,
    -- | The amount of cost, usage, RI units, or Savings Plans units that you
    -- used.
    actualSpend :: Spend
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculatedSpend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastedSpend', 'calculatedSpend_forecastedSpend' - The amount of cost, usage, RI units, or Savings Plans units that you\'re
-- forecasted to use.
--
-- 'actualSpend', 'calculatedSpend_actualSpend' - The amount of cost, usage, RI units, or Savings Plans units that you
-- used.
newCalculatedSpend ::
  -- | 'actualSpend'
  Spend ->
  CalculatedSpend
newCalculatedSpend pActualSpend_ =
  CalculatedSpend'
    { forecastedSpend = Prelude.Nothing,
      actualSpend = pActualSpend_
    }

-- | The amount of cost, usage, RI units, or Savings Plans units that you\'re
-- forecasted to use.
calculatedSpend_forecastedSpend :: Lens.Lens' CalculatedSpend (Prelude.Maybe Spend)
calculatedSpend_forecastedSpend = Lens.lens (\CalculatedSpend' {forecastedSpend} -> forecastedSpend) (\s@CalculatedSpend' {} a -> s {forecastedSpend = a} :: CalculatedSpend)

-- | The amount of cost, usage, RI units, or Savings Plans units that you
-- used.
calculatedSpend_actualSpend :: Lens.Lens' CalculatedSpend Spend
calculatedSpend_actualSpend = Lens.lens (\CalculatedSpend' {actualSpend} -> actualSpend) (\s@CalculatedSpend' {} a -> s {actualSpend = a} :: CalculatedSpend)

instance Data.FromJSON CalculatedSpend where
  parseJSON =
    Data.withObject
      "CalculatedSpend"
      ( \x ->
          CalculatedSpend'
            Prelude.<$> (x Data..:? "ForecastedSpend")
            Prelude.<*> (x Data..: "ActualSpend")
      )

instance Prelude.Hashable CalculatedSpend where
  hashWithSalt _salt CalculatedSpend' {..} =
    _salt `Prelude.hashWithSalt` forecastedSpend
      `Prelude.hashWithSalt` actualSpend

instance Prelude.NFData CalculatedSpend where
  rnf CalculatedSpend' {..} =
    Prelude.rnf forecastedSpend
      `Prelude.seq` Prelude.rnf actualSpend

instance Data.ToJSON CalculatedSpend where
  toJSON CalculatedSpend' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForecastedSpend" Data..=)
              Prelude.<$> forecastedSpend,
            Prelude.Just ("ActualSpend" Data..= actualSpend)
          ]
      )
