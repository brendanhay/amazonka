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
-- Module      : Network.AWS.GuardDuty.Types.Total
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Total where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the total usage with the corresponding currency unit for that
-- value.
--
-- /See:/ 'newTotal' smart constructor.
data Total = Total'
  { -- | The total usage.
    amount :: Prelude.Maybe Prelude.Text,
    -- | The currency unit that the amount is given in.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Total' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'total_amount' - The total usage.
--
-- 'unit', 'total_unit' - The currency unit that the amount is given in.
newTotal ::
  Total
newTotal =
  Total'
    { amount = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The total usage.
total_amount :: Lens.Lens' Total (Prelude.Maybe Prelude.Text)
total_amount = Lens.lens (\Total' {amount} -> amount) (\s@Total' {} a -> s {amount = a} :: Total)

-- | The currency unit that the amount is given in.
total_unit :: Lens.Lens' Total (Prelude.Maybe Prelude.Text)
total_unit = Lens.lens (\Total' {unit} -> unit) (\s@Total' {} a -> s {unit = a} :: Total)

instance Prelude.FromJSON Total where
  parseJSON =
    Prelude.withObject
      "Total"
      ( \x ->
          Total'
            Prelude.<$> (x Prelude..:? "amount")
            Prelude.<*> (x Prelude..:? "unit")
      )

instance Prelude.Hashable Total

instance Prelude.NFData Total
