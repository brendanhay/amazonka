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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Usage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Currency
import Amazonka.Inspector2.Types.UsageType
import qualified Amazonka.Prelude as Prelude

-- | Contains usage information about the cost of Amazon Inspector operation.
--
-- /See:/ 'newUsage' smart constructor.
data Usage = Usage'
  { -- | The currency type used when calculating usage data.
    currency :: Prelude.Maybe Currency,
    -- | The estimated monthly cost of Amazon Inspector.
    estimatedMonthlyCost :: Prelude.Maybe Prelude.Double,
    -- | The total of usage.
    total :: Prelude.Maybe Prelude.Double,
    -- | The type scan.
    type' :: Prelude.Maybe UsageType
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
-- 'currency', 'usage_currency' - The currency type used when calculating usage data.
--
-- 'estimatedMonthlyCost', 'usage_estimatedMonthlyCost' - The estimated monthly cost of Amazon Inspector.
--
-- 'total', 'usage_total' - The total of usage.
--
-- 'type'', 'usage_type' - The type scan.
newUsage ::
  Usage
newUsage =
  Usage'
    { currency = Prelude.Nothing,
      estimatedMonthlyCost = Prelude.Nothing,
      total = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The currency type used when calculating usage data.
usage_currency :: Lens.Lens' Usage (Prelude.Maybe Currency)
usage_currency = Lens.lens (\Usage' {currency} -> currency) (\s@Usage' {} a -> s {currency = a} :: Usage)

-- | The estimated monthly cost of Amazon Inspector.
usage_estimatedMonthlyCost :: Lens.Lens' Usage (Prelude.Maybe Prelude.Double)
usage_estimatedMonthlyCost = Lens.lens (\Usage' {estimatedMonthlyCost} -> estimatedMonthlyCost) (\s@Usage' {} a -> s {estimatedMonthlyCost = a} :: Usage)

-- | The total of usage.
usage_total :: Lens.Lens' Usage (Prelude.Maybe Prelude.Double)
usage_total = Lens.lens (\Usage' {total} -> total) (\s@Usage' {} a -> s {total = a} :: Usage)

-- | The type scan.
usage_type :: Lens.Lens' Usage (Prelude.Maybe UsageType)
usage_type = Lens.lens (\Usage' {type'} -> type') (\s@Usage' {} a -> s {type' = a} :: Usage)

instance Data.FromJSON Usage where
  parseJSON =
    Data.withObject
      "Usage"
      ( \x ->
          Usage'
            Prelude.<$> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "estimatedMonthlyCost")
            Prelude.<*> (x Data..:? "total")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Usage where
  hashWithSalt _salt Usage' {..} =
    _salt
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` estimatedMonthlyCost
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Usage where
  rnf Usage' {..} =
    Prelude.rnf currency
      `Prelude.seq` Prelude.rnf estimatedMonthlyCost
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf type'
