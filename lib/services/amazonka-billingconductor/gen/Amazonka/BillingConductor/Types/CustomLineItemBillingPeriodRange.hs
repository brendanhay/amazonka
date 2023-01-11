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
-- Module      : Amazonka.BillingConductor.Types.CustomLineItemBillingPeriodRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.CustomLineItemBillingPeriodRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The billing period range in which the custom line item request will be
-- applied.
--
-- /See:/ 'newCustomLineItemBillingPeriodRange' smart constructor.
data CustomLineItemBillingPeriodRange = CustomLineItemBillingPeriodRange'
  { -- | The inclusive end billing period that defines a billing period range
    -- where a custom line is applied.
    exclusiveEndBillingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The inclusive start billing period that defines a billing period range
    -- where a custom line is applied.
    inclusiveStartBillingPeriod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLineItemBillingPeriodRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveEndBillingPeriod', 'customLineItemBillingPeriodRange_exclusiveEndBillingPeriod' - The inclusive end billing period that defines a billing period range
-- where a custom line is applied.
--
-- 'inclusiveStartBillingPeriod', 'customLineItemBillingPeriodRange_inclusiveStartBillingPeriod' - The inclusive start billing period that defines a billing period range
-- where a custom line is applied.
newCustomLineItemBillingPeriodRange ::
  -- | 'inclusiveStartBillingPeriod'
  Prelude.Text ->
  CustomLineItemBillingPeriodRange
newCustomLineItemBillingPeriodRange
  pInclusiveStartBillingPeriod_ =
    CustomLineItemBillingPeriodRange'
      { exclusiveEndBillingPeriod =
          Prelude.Nothing,
        inclusiveStartBillingPeriod =
          pInclusiveStartBillingPeriod_
      }

-- | The inclusive end billing period that defines a billing period range
-- where a custom line is applied.
customLineItemBillingPeriodRange_exclusiveEndBillingPeriod :: Lens.Lens' CustomLineItemBillingPeriodRange (Prelude.Maybe Prelude.Text)
customLineItemBillingPeriodRange_exclusiveEndBillingPeriod = Lens.lens (\CustomLineItemBillingPeriodRange' {exclusiveEndBillingPeriod} -> exclusiveEndBillingPeriod) (\s@CustomLineItemBillingPeriodRange' {} a -> s {exclusiveEndBillingPeriod = a} :: CustomLineItemBillingPeriodRange)

-- | The inclusive start billing period that defines a billing period range
-- where a custom line is applied.
customLineItemBillingPeriodRange_inclusiveStartBillingPeriod :: Lens.Lens' CustomLineItemBillingPeriodRange Prelude.Text
customLineItemBillingPeriodRange_inclusiveStartBillingPeriod = Lens.lens (\CustomLineItemBillingPeriodRange' {inclusiveStartBillingPeriod} -> inclusiveStartBillingPeriod) (\s@CustomLineItemBillingPeriodRange' {} a -> s {inclusiveStartBillingPeriod = a} :: CustomLineItemBillingPeriodRange)

instance
  Prelude.Hashable
    CustomLineItemBillingPeriodRange
  where
  hashWithSalt
    _salt
    CustomLineItemBillingPeriodRange' {..} =
      _salt
        `Prelude.hashWithSalt` exclusiveEndBillingPeriod
        `Prelude.hashWithSalt` inclusiveStartBillingPeriod

instance
  Prelude.NFData
    CustomLineItemBillingPeriodRange
  where
  rnf CustomLineItemBillingPeriodRange' {..} =
    Prelude.rnf exclusiveEndBillingPeriod
      `Prelude.seq` Prelude.rnf inclusiveStartBillingPeriod

instance Data.ToJSON CustomLineItemBillingPeriodRange where
  toJSON CustomLineItemBillingPeriodRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveEndBillingPeriod" Data..=)
              Prelude.<$> exclusiveEndBillingPeriod,
            Prelude.Just
              ( "InclusiveStartBillingPeriod"
                  Data..= inclusiveStartBillingPeriod
              )
          ]
      )
