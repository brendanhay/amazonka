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
-- Module      : Amazonka.GuardDuty.Types.Total
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Total where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the total usage with the corresponding currency unit for that
-- value.
--
-- /See:/ 'newTotal' smart constructor.
data Total = Total'
  { -- | The currency unit that the amount is given in.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The total usage.
    amount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Total' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'total_unit' - The currency unit that the amount is given in.
--
-- 'amount', 'total_amount' - The total usage.
newTotal ::
  Total
newTotal =
  Total'
    { unit = Prelude.Nothing,
      amount = Prelude.Nothing
    }

-- | The currency unit that the amount is given in.
total_unit :: Lens.Lens' Total (Prelude.Maybe Prelude.Text)
total_unit = Lens.lens (\Total' {unit} -> unit) (\s@Total' {} a -> s {unit = a} :: Total)

-- | The total usage.
total_amount :: Lens.Lens' Total (Prelude.Maybe Prelude.Text)
total_amount = Lens.lens (\Total' {amount} -> amount) (\s@Total' {} a -> s {amount = a} :: Total)

instance Data.FromJSON Total where
  parseJSON =
    Data.withObject
      "Total"
      ( \x ->
          Total'
            Prelude.<$> (x Data..:? "unit")
            Prelude.<*> (x Data..:? "amount")
      )

instance Prelude.Hashable Total where
  hashWithSalt _salt Total' {..} =
    _salt `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` amount

instance Prelude.NFData Total where
  rnf Total' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf amount
