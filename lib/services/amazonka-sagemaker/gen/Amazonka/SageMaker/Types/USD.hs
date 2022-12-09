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
-- Module      : Amazonka.SageMaker.Types.USD
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.USD where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an amount of money in United States dollars.
--
-- /See:/ 'newUSD' smart constructor.
data USD = USD'
  { -- | The fractional portion, in cents, of the amount.
    cents :: Prelude.Maybe Prelude.Natural,
    -- | The whole number of dollars in the amount.
    dollars :: Prelude.Maybe Prelude.Natural,
    -- | Fractions of a cent, in tenths.
    tenthFractionsOfACent :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'USD' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cents', 'usd_cents' - The fractional portion, in cents, of the amount.
--
-- 'dollars', 'usd_dollars' - The whole number of dollars in the amount.
--
-- 'tenthFractionsOfACent', 'usd_tenthFractionsOfACent' - Fractions of a cent, in tenths.
newUSD ::
  USD
newUSD =
  USD'
    { cents = Prelude.Nothing,
      dollars = Prelude.Nothing,
      tenthFractionsOfACent = Prelude.Nothing
    }

-- | The fractional portion, in cents, of the amount.
usd_cents :: Lens.Lens' USD (Prelude.Maybe Prelude.Natural)
usd_cents = Lens.lens (\USD' {cents} -> cents) (\s@USD' {} a -> s {cents = a} :: USD)

-- | The whole number of dollars in the amount.
usd_dollars :: Lens.Lens' USD (Prelude.Maybe Prelude.Natural)
usd_dollars = Lens.lens (\USD' {dollars} -> dollars) (\s@USD' {} a -> s {dollars = a} :: USD)

-- | Fractions of a cent, in tenths.
usd_tenthFractionsOfACent :: Lens.Lens' USD (Prelude.Maybe Prelude.Natural)
usd_tenthFractionsOfACent = Lens.lens (\USD' {tenthFractionsOfACent} -> tenthFractionsOfACent) (\s@USD' {} a -> s {tenthFractionsOfACent = a} :: USD)

instance Data.FromJSON USD where
  parseJSON =
    Data.withObject
      "USD"
      ( \x ->
          USD'
            Prelude.<$> (x Data..:? "Cents")
            Prelude.<*> (x Data..:? "Dollars")
            Prelude.<*> (x Data..:? "TenthFractionsOfACent")
      )

instance Prelude.Hashable USD where
  hashWithSalt _salt USD' {..} =
    _salt `Prelude.hashWithSalt` cents
      `Prelude.hashWithSalt` dollars
      `Prelude.hashWithSalt` tenthFractionsOfACent

instance Prelude.NFData USD where
  rnf USD' {..} =
    Prelude.rnf cents
      `Prelude.seq` Prelude.rnf dollars
      `Prelude.seq` Prelude.rnf tenthFractionsOfACent

instance Data.ToJSON USD where
  toJSON USD' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Cents" Data..=) Prelude.<$> cents,
            ("Dollars" Data..=) Prelude.<$> dollars,
            ("TenthFractionsOfACent" Data..=)
              Prelude.<$> tenthFractionsOfACent
          ]
      )
