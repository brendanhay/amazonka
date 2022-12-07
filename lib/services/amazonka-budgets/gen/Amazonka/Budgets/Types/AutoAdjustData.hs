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
-- Module      : Amazonka.Budgets.Types.AutoAdjustData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.AutoAdjustData where

import Amazonka.Budgets.Types.AutoAdjustType
import Amazonka.Budgets.Types.HistoricalOptions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters that determine the budget amount for an auto-adjusting
-- budget.
--
-- /See:/ 'newAutoAdjustData' smart constructor.
data AutoAdjustData = AutoAdjustData'
  { -- | The last time that your budget was auto-adjusted.
    lastAutoAdjustTime :: Prelude.Maybe Data.POSIX,
    -- | The parameters that define or describe the historical data that your
    -- auto-adjusting budget is based on.
    historicalOptions :: Prelude.Maybe HistoricalOptions,
    -- | The string that defines whether your budget auto-adjusts based on
    -- historical or forecasted data.
    autoAdjustType :: AutoAdjustType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoAdjustData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastAutoAdjustTime', 'autoAdjustData_lastAutoAdjustTime' - The last time that your budget was auto-adjusted.
--
-- 'historicalOptions', 'autoAdjustData_historicalOptions' - The parameters that define or describe the historical data that your
-- auto-adjusting budget is based on.
--
-- 'autoAdjustType', 'autoAdjustData_autoAdjustType' - The string that defines whether your budget auto-adjusts based on
-- historical or forecasted data.
newAutoAdjustData ::
  -- | 'autoAdjustType'
  AutoAdjustType ->
  AutoAdjustData
newAutoAdjustData pAutoAdjustType_ =
  AutoAdjustData'
    { lastAutoAdjustTime =
        Prelude.Nothing,
      historicalOptions = Prelude.Nothing,
      autoAdjustType = pAutoAdjustType_
    }

-- | The last time that your budget was auto-adjusted.
autoAdjustData_lastAutoAdjustTime :: Lens.Lens' AutoAdjustData (Prelude.Maybe Prelude.UTCTime)
autoAdjustData_lastAutoAdjustTime = Lens.lens (\AutoAdjustData' {lastAutoAdjustTime} -> lastAutoAdjustTime) (\s@AutoAdjustData' {} a -> s {lastAutoAdjustTime = a} :: AutoAdjustData) Prelude.. Lens.mapping Data._Time

-- | The parameters that define or describe the historical data that your
-- auto-adjusting budget is based on.
autoAdjustData_historicalOptions :: Lens.Lens' AutoAdjustData (Prelude.Maybe HistoricalOptions)
autoAdjustData_historicalOptions = Lens.lens (\AutoAdjustData' {historicalOptions} -> historicalOptions) (\s@AutoAdjustData' {} a -> s {historicalOptions = a} :: AutoAdjustData)

-- | The string that defines whether your budget auto-adjusts based on
-- historical or forecasted data.
autoAdjustData_autoAdjustType :: Lens.Lens' AutoAdjustData AutoAdjustType
autoAdjustData_autoAdjustType = Lens.lens (\AutoAdjustData' {autoAdjustType} -> autoAdjustType) (\s@AutoAdjustData' {} a -> s {autoAdjustType = a} :: AutoAdjustData)

instance Data.FromJSON AutoAdjustData where
  parseJSON =
    Data.withObject
      "AutoAdjustData"
      ( \x ->
          AutoAdjustData'
            Prelude.<$> (x Data..:? "LastAutoAdjustTime")
            Prelude.<*> (x Data..:? "HistoricalOptions")
            Prelude.<*> (x Data..: "AutoAdjustType")
      )

instance Prelude.Hashable AutoAdjustData where
  hashWithSalt _salt AutoAdjustData' {..} =
    _salt `Prelude.hashWithSalt` lastAutoAdjustTime
      `Prelude.hashWithSalt` historicalOptions
      `Prelude.hashWithSalt` autoAdjustType

instance Prelude.NFData AutoAdjustData where
  rnf AutoAdjustData' {..} =
    Prelude.rnf lastAutoAdjustTime
      `Prelude.seq` Prelude.rnf historicalOptions
      `Prelude.seq` Prelude.rnf autoAdjustType

instance Data.ToJSON AutoAdjustData where
  toJSON AutoAdjustData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LastAutoAdjustTime" Data..=)
              Prelude.<$> lastAutoAdjustTime,
            ("HistoricalOptions" Data..=)
              Prelude.<$> historicalOptions,
            Prelude.Just
              ("AutoAdjustType" Data..= autoAdjustType)
          ]
      )
