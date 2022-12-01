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
-- Module      : Amazonka.ServiceQuotas.Types.QuotaPeriod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.QuotaPeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceQuotas.Types.PeriodUnit

-- | Information about the quota period.
--
-- /See:/ 'newQuotaPeriod' smart constructor.
data QuotaPeriod = QuotaPeriod'
  { -- | The time unit.
    periodUnit :: Prelude.Maybe PeriodUnit,
    -- | The value.
    periodValue :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuotaPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'periodUnit', 'quotaPeriod_periodUnit' - The time unit.
--
-- 'periodValue', 'quotaPeriod_periodValue' - The value.
newQuotaPeriod ::
  QuotaPeriod
newQuotaPeriod =
  QuotaPeriod'
    { periodUnit = Prelude.Nothing,
      periodValue = Prelude.Nothing
    }

-- | The time unit.
quotaPeriod_periodUnit :: Lens.Lens' QuotaPeriod (Prelude.Maybe PeriodUnit)
quotaPeriod_periodUnit = Lens.lens (\QuotaPeriod' {periodUnit} -> periodUnit) (\s@QuotaPeriod' {} a -> s {periodUnit = a} :: QuotaPeriod)

-- | The value.
quotaPeriod_periodValue :: Lens.Lens' QuotaPeriod (Prelude.Maybe Prelude.Int)
quotaPeriod_periodValue = Lens.lens (\QuotaPeriod' {periodValue} -> periodValue) (\s@QuotaPeriod' {} a -> s {periodValue = a} :: QuotaPeriod)

instance Core.FromJSON QuotaPeriod where
  parseJSON =
    Core.withObject
      "QuotaPeriod"
      ( \x ->
          QuotaPeriod'
            Prelude.<$> (x Core..:? "PeriodUnit")
            Prelude.<*> (x Core..:? "PeriodValue")
      )

instance Prelude.Hashable QuotaPeriod where
  hashWithSalt _salt QuotaPeriod' {..} =
    _salt `Prelude.hashWithSalt` periodUnit
      `Prelude.hashWithSalt` periodValue

instance Prelude.NFData QuotaPeriod where
  rnf QuotaPeriod' {..} =
    Prelude.rnf periodUnit
      `Prelude.seq` Prelude.rnf periodValue
