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
-- Module      : Amazonka.Inspector2.Types.Usage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Usage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.Currency
import Amazonka.Inspector2.Types.UsageType
import qualified Amazonka.Prelude as Prelude

-- | Contains usage information about the cost of Amazon Inspector operation.
--
-- /See:/ 'newUsage' smart constructor.
data Usage = Usage'
  { -- | The type scan.
    type' :: Prelude.Maybe UsageType,
    -- | The total of usage.
    total :: Prelude.Maybe Prelude.Double,
    -- | The estimated monthly cost of Amazon Inspector.
    estimatedMonthlyCost :: Prelude.Maybe Prelude.Double,
    -- | The currency type used when calculating usage data.
    currency :: Prelude.Maybe Currency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Usage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'usage_type' - The type scan.
--
-- 'total', 'usage_total' - The total of usage.
--
-- 'estimatedMonthlyCost', 'usage_estimatedMonthlyCost' - The estimated monthly cost of Amazon Inspector.
--
-- 'currency', 'usage_currency' - The currency type used when calculating usage data.
newUsage ::
  Usage
newUsage =
  Usage'
    { type' = Prelude.Nothing,
      total = Prelude.Nothing,
      estimatedMonthlyCost = Prelude.Nothing,
      currency = Prelude.Nothing
    }

-- | The type scan.
usage_type :: Lens.Lens' Usage (Prelude.Maybe UsageType)
usage_type = Lens.lens (\Usage' {type'} -> type') (\s@Usage' {} a -> s {type' = a} :: Usage)

-- | The total of usage.
usage_total :: Lens.Lens' Usage (Prelude.Maybe Prelude.Double)
usage_total = Lens.lens (\Usage' {total} -> total) (\s@Usage' {} a -> s {total = a} :: Usage)

-- | The estimated monthly cost of Amazon Inspector.
usage_estimatedMonthlyCost :: Lens.Lens' Usage (Prelude.Maybe Prelude.Double)
usage_estimatedMonthlyCost = Lens.lens (\Usage' {estimatedMonthlyCost} -> estimatedMonthlyCost) (\s@Usage' {} a -> s {estimatedMonthlyCost = a} :: Usage)

-- | The currency type used when calculating usage data.
usage_currency :: Lens.Lens' Usage (Prelude.Maybe Currency)
usage_currency = Lens.lens (\Usage' {currency} -> currency) (\s@Usage' {} a -> s {currency = a} :: Usage)

instance Core.FromJSON Usage where
  parseJSON =
    Core.withObject
      "Usage"
      ( \x ->
          Usage'
            Prelude.<$> (x Core..:? "type")
            Prelude.<*> (x Core..:? "total")
            Prelude.<*> (x Core..:? "estimatedMonthlyCost")
            Prelude.<*> (x Core..:? "currency")
      )

instance Prelude.Hashable Usage where
  hashWithSalt _salt Usage' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` estimatedMonthlyCost
      `Prelude.hashWithSalt` currency

instance Prelude.NFData Usage where
  rnf Usage' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf estimatedMonthlyCost
      `Prelude.seq` Prelude.rnf currency
