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
-- Module      : Amazonka.MacieV2.Types.UsageTotal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageTotal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.Currency
import Amazonka.MacieV2.Types.UsageType
import qualified Amazonka.Prelude as Prelude

-- | Provides aggregated data for an Amazon Macie usage metric. The value for
-- the metric reports estimated usage data for an account for the preceding
-- 30 days or the current calendar month to date, depending on the time
-- period (timeRange) specified in the request.
--
-- /See:/ 'newUsageTotal' smart constructor.
data UsageTotal = UsageTotal'
  { -- | The name of the metric. Possible values are: DATA_INVENTORY_EVALUATION,
    -- for monitoring S3 buckets; and, SENSITIVE_DATA_DISCOVERY, for analyzing
    -- S3 objects to detect sensitive data.
    type' :: Prelude.Maybe UsageType,
    -- | The type of currency that the value for the metric (estimatedCost) is
    -- reported in.
    currency :: Prelude.Maybe Currency,
    -- | The estimated value for the metric.
    estimatedCost :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageTotal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'usageTotal_type' - The name of the metric. Possible values are: DATA_INVENTORY_EVALUATION,
-- for monitoring S3 buckets; and, SENSITIVE_DATA_DISCOVERY, for analyzing
-- S3 objects to detect sensitive data.
--
-- 'currency', 'usageTotal_currency' - The type of currency that the value for the metric (estimatedCost) is
-- reported in.
--
-- 'estimatedCost', 'usageTotal_estimatedCost' - The estimated value for the metric.
newUsageTotal ::
  UsageTotal
newUsageTotal =
  UsageTotal'
    { type' = Prelude.Nothing,
      currency = Prelude.Nothing,
      estimatedCost = Prelude.Nothing
    }

-- | The name of the metric. Possible values are: DATA_INVENTORY_EVALUATION,
-- for monitoring S3 buckets; and, SENSITIVE_DATA_DISCOVERY, for analyzing
-- S3 objects to detect sensitive data.
usageTotal_type :: Lens.Lens' UsageTotal (Prelude.Maybe UsageType)
usageTotal_type = Lens.lens (\UsageTotal' {type'} -> type') (\s@UsageTotal' {} a -> s {type' = a} :: UsageTotal)

-- | The type of currency that the value for the metric (estimatedCost) is
-- reported in.
usageTotal_currency :: Lens.Lens' UsageTotal (Prelude.Maybe Currency)
usageTotal_currency = Lens.lens (\UsageTotal' {currency} -> currency) (\s@UsageTotal' {} a -> s {currency = a} :: UsageTotal)

-- | The estimated value for the metric.
usageTotal_estimatedCost :: Lens.Lens' UsageTotal (Prelude.Maybe Prelude.Text)
usageTotal_estimatedCost = Lens.lens (\UsageTotal' {estimatedCost} -> estimatedCost) (\s@UsageTotal' {} a -> s {estimatedCost = a} :: UsageTotal)

instance Core.FromJSON UsageTotal where
  parseJSON =
    Core.withObject
      "UsageTotal"
      ( \x ->
          UsageTotal'
            Prelude.<$> (x Core..:? "type")
            Prelude.<*> (x Core..:? "currency")
            Prelude.<*> (x Core..:? "estimatedCost")
      )

instance Prelude.Hashable UsageTotal where
  hashWithSalt _salt UsageTotal' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` estimatedCost

instance Prelude.NFData UsageTotal where
  rnf UsageTotal' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf currency
      `Prelude.seq` Prelude.rnf estimatedCost
