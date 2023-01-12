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
-- Module      : Amazonka.QuickSight.Types.ExcludePeriodConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ExcludePeriodConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TimeGranularity
import Amazonka.QuickSight.Types.WidgetStatus

-- | The exclude period of @TimeRangeFilter@ or @RelativeDatesFilter@.
--
-- /See:/ 'newExcludePeriodConfiguration' smart constructor.
data ExcludePeriodConfiguration = ExcludePeriodConfiguration'
  { -- | The status of the exclude period. Choose from the following options:
    --
    -- -   @ENABLED@
    --
    -- -   @DISABLED@
    status :: Prelude.Maybe WidgetStatus,
    -- | The amount or number of the exclude period.
    amount :: Prelude.Int,
    -- | The granularity or unit (day, month, year) of the exclude period.
    granularity :: TimeGranularity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExcludePeriodConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'excludePeriodConfiguration_status' - The status of the exclude period. Choose from the following options:
--
-- -   @ENABLED@
--
-- -   @DISABLED@
--
-- 'amount', 'excludePeriodConfiguration_amount' - The amount or number of the exclude period.
--
-- 'granularity', 'excludePeriodConfiguration_granularity' - The granularity or unit (day, month, year) of the exclude period.
newExcludePeriodConfiguration ::
  -- | 'amount'
  Prelude.Int ->
  -- | 'granularity'
  TimeGranularity ->
  ExcludePeriodConfiguration
newExcludePeriodConfiguration pAmount_ pGranularity_ =
  ExcludePeriodConfiguration'
    { status =
        Prelude.Nothing,
      amount = pAmount_,
      granularity = pGranularity_
    }

-- | The status of the exclude period. Choose from the following options:
--
-- -   @ENABLED@
--
-- -   @DISABLED@
excludePeriodConfiguration_status :: Lens.Lens' ExcludePeriodConfiguration (Prelude.Maybe WidgetStatus)
excludePeriodConfiguration_status = Lens.lens (\ExcludePeriodConfiguration' {status} -> status) (\s@ExcludePeriodConfiguration' {} a -> s {status = a} :: ExcludePeriodConfiguration)

-- | The amount or number of the exclude period.
excludePeriodConfiguration_amount :: Lens.Lens' ExcludePeriodConfiguration Prelude.Int
excludePeriodConfiguration_amount = Lens.lens (\ExcludePeriodConfiguration' {amount} -> amount) (\s@ExcludePeriodConfiguration' {} a -> s {amount = a} :: ExcludePeriodConfiguration)

-- | The granularity or unit (day, month, year) of the exclude period.
excludePeriodConfiguration_granularity :: Lens.Lens' ExcludePeriodConfiguration TimeGranularity
excludePeriodConfiguration_granularity = Lens.lens (\ExcludePeriodConfiguration' {granularity} -> granularity) (\s@ExcludePeriodConfiguration' {} a -> s {granularity = a} :: ExcludePeriodConfiguration)

instance Data.FromJSON ExcludePeriodConfiguration where
  parseJSON =
    Data.withObject
      "ExcludePeriodConfiguration"
      ( \x ->
          ExcludePeriodConfiguration'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..: "Amount")
            Prelude.<*> (x Data..: "Granularity")
      )

instance Prelude.Hashable ExcludePeriodConfiguration where
  hashWithSalt _salt ExcludePeriodConfiguration' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` granularity

instance Prelude.NFData ExcludePeriodConfiguration where
  rnf ExcludePeriodConfiguration' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf amount
      `Prelude.seq` Prelude.rnf granularity

instance Data.ToJSON ExcludePeriodConfiguration where
  toJSON ExcludePeriodConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("Amount" Data..= amount),
            Prelude.Just ("Granularity" Data..= granularity)
          ]
      )
