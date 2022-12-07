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
-- Module      : Amazonka.BillingConductor.Types.ListCustomLineItemVersionsBillingPeriodRangeFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListCustomLineItemVersionsBillingPeriodRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A billing period filter that specifies the custom line item versions to
-- retrieve.
--
-- /See:/ 'newListCustomLineItemVersionsBillingPeriodRangeFilter' smart constructor.
data ListCustomLineItemVersionsBillingPeriodRangeFilter = ListCustomLineItemVersionsBillingPeriodRangeFilter'
  { -- | The inclusive start billing period that defines a billing period range
    -- where a custom line item version is applied.
    startBillingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The exclusive end billing period that defines a billing period range
    -- where a custom line item version is applied.
    endBillingPeriod :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemVersionsBillingPeriodRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startBillingPeriod', 'listCustomLineItemVersionsBillingPeriodRangeFilter_startBillingPeriod' - The inclusive start billing period that defines a billing period range
-- where a custom line item version is applied.
--
-- 'endBillingPeriod', 'listCustomLineItemVersionsBillingPeriodRangeFilter_endBillingPeriod' - The exclusive end billing period that defines a billing period range
-- where a custom line item version is applied.
newListCustomLineItemVersionsBillingPeriodRangeFilter ::
  ListCustomLineItemVersionsBillingPeriodRangeFilter
newListCustomLineItemVersionsBillingPeriodRangeFilter =
  ListCustomLineItemVersionsBillingPeriodRangeFilter'
    { startBillingPeriod =
        Prelude.Nothing,
      endBillingPeriod =
        Prelude.Nothing
    }

-- | The inclusive start billing period that defines a billing period range
-- where a custom line item version is applied.
listCustomLineItemVersionsBillingPeriodRangeFilter_startBillingPeriod :: Lens.Lens' ListCustomLineItemVersionsBillingPeriodRangeFilter (Prelude.Maybe Prelude.Text)
listCustomLineItemVersionsBillingPeriodRangeFilter_startBillingPeriod = Lens.lens (\ListCustomLineItemVersionsBillingPeriodRangeFilter' {startBillingPeriod} -> startBillingPeriod) (\s@ListCustomLineItemVersionsBillingPeriodRangeFilter' {} a -> s {startBillingPeriod = a} :: ListCustomLineItemVersionsBillingPeriodRangeFilter)

-- | The exclusive end billing period that defines a billing period range
-- where a custom line item version is applied.
listCustomLineItemVersionsBillingPeriodRangeFilter_endBillingPeriod :: Lens.Lens' ListCustomLineItemVersionsBillingPeriodRangeFilter (Prelude.Maybe Prelude.Text)
listCustomLineItemVersionsBillingPeriodRangeFilter_endBillingPeriod = Lens.lens (\ListCustomLineItemVersionsBillingPeriodRangeFilter' {endBillingPeriod} -> endBillingPeriod) (\s@ListCustomLineItemVersionsBillingPeriodRangeFilter' {} a -> s {endBillingPeriod = a} :: ListCustomLineItemVersionsBillingPeriodRangeFilter)

instance
  Prelude.Hashable
    ListCustomLineItemVersionsBillingPeriodRangeFilter
  where
  hashWithSalt
    _salt
    ListCustomLineItemVersionsBillingPeriodRangeFilter' {..} =
      _salt `Prelude.hashWithSalt` startBillingPeriod
        `Prelude.hashWithSalt` endBillingPeriod

instance
  Prelude.NFData
    ListCustomLineItemVersionsBillingPeriodRangeFilter
  where
  rnf
    ListCustomLineItemVersionsBillingPeriodRangeFilter' {..} =
      Prelude.rnf startBillingPeriod
        `Prelude.seq` Prelude.rnf endBillingPeriod

instance
  Data.ToJSON
    ListCustomLineItemVersionsBillingPeriodRangeFilter
  where
  toJSON
    ListCustomLineItemVersionsBillingPeriodRangeFilter' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("StartBillingPeriod" Data..=)
                Prelude.<$> startBillingPeriod,
              ("EndBillingPeriod" Data..=)
                Prelude.<$> endBillingPeriod
            ]
        )
