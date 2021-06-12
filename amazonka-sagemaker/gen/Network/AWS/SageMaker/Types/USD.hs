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
-- Module      : Network.AWS.SageMaker.Types.USD
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.USD where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an amount of money in United States dollars\/
--
-- /See:/ 'newUSD' smart constructor.
data USD = USD'
  { -- | The whole number of dollars in the amount.
    dollars :: Core.Maybe Core.Natural,
    -- | The fractional portion, in cents, of the amount.
    cents :: Core.Maybe Core.Natural,
    -- | Fractions of a cent, in tenths.
    tenthFractionsOfACent :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'USD' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dollars', 'usd_dollars' - The whole number of dollars in the amount.
--
-- 'cents', 'usd_cents' - The fractional portion, in cents, of the amount.
--
-- 'tenthFractionsOfACent', 'usd_tenthFractionsOfACent' - Fractions of a cent, in tenths.
newUSD ::
  USD
newUSD =
  USD'
    { dollars = Core.Nothing,
      cents = Core.Nothing,
      tenthFractionsOfACent = Core.Nothing
    }

-- | The whole number of dollars in the amount.
usd_dollars :: Lens.Lens' USD (Core.Maybe Core.Natural)
usd_dollars = Lens.lens (\USD' {dollars} -> dollars) (\s@USD' {} a -> s {dollars = a} :: USD)

-- | The fractional portion, in cents, of the amount.
usd_cents :: Lens.Lens' USD (Core.Maybe Core.Natural)
usd_cents = Lens.lens (\USD' {cents} -> cents) (\s@USD' {} a -> s {cents = a} :: USD)

-- | Fractions of a cent, in tenths.
usd_tenthFractionsOfACent :: Lens.Lens' USD (Core.Maybe Core.Natural)
usd_tenthFractionsOfACent = Lens.lens (\USD' {tenthFractionsOfACent} -> tenthFractionsOfACent) (\s@USD' {} a -> s {tenthFractionsOfACent = a} :: USD)

instance Core.FromJSON USD where
  parseJSON =
    Core.withObject
      "USD"
      ( \x ->
          USD'
            Core.<$> (x Core..:? "Dollars")
            Core.<*> (x Core..:? "Cents")
            Core.<*> (x Core..:? "TenthFractionsOfACent")
      )

instance Core.Hashable USD

instance Core.NFData USD

instance Core.ToJSON USD where
  toJSON USD' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Dollars" Core..=) Core.<$> dollars,
            ("Cents" Core..=) Core.<$> cents,
            ("TenthFractionsOfACent" Core..=)
              Core.<$> tenthFractionsOfACent
          ]
      )
