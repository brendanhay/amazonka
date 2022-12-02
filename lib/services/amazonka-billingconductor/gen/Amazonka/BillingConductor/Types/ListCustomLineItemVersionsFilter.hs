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
-- Module      : Amazonka.BillingConductor.Types.ListCustomLineItemVersionsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListCustomLineItemVersionsFilter where

import Amazonka.BillingConductor.Types.ListCustomLineItemVersionsBillingPeriodRangeFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter that specifies the billing period range where the custom line
-- item versions reside.
--
-- /See:/ 'newListCustomLineItemVersionsFilter' smart constructor.
data ListCustomLineItemVersionsFilter = ListCustomLineItemVersionsFilter'
  { -- | The billing period range in which the custom line item version is
    -- applied.
    billingPeriodRange :: Prelude.Maybe ListCustomLineItemVersionsBillingPeriodRangeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemVersionsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingPeriodRange', 'listCustomLineItemVersionsFilter_billingPeriodRange' - The billing period range in which the custom line item version is
-- applied.
newListCustomLineItemVersionsFilter ::
  ListCustomLineItemVersionsFilter
newListCustomLineItemVersionsFilter =
  ListCustomLineItemVersionsFilter'
    { billingPeriodRange =
        Prelude.Nothing
    }

-- | The billing period range in which the custom line item version is
-- applied.
listCustomLineItemVersionsFilter_billingPeriodRange :: Lens.Lens' ListCustomLineItemVersionsFilter (Prelude.Maybe ListCustomLineItemVersionsBillingPeriodRangeFilter)
listCustomLineItemVersionsFilter_billingPeriodRange = Lens.lens (\ListCustomLineItemVersionsFilter' {billingPeriodRange} -> billingPeriodRange) (\s@ListCustomLineItemVersionsFilter' {} a -> s {billingPeriodRange = a} :: ListCustomLineItemVersionsFilter)

instance
  Prelude.Hashable
    ListCustomLineItemVersionsFilter
  where
  hashWithSalt
    _salt
    ListCustomLineItemVersionsFilter' {..} =
      _salt `Prelude.hashWithSalt` billingPeriodRange

instance
  Prelude.NFData
    ListCustomLineItemVersionsFilter
  where
  rnf ListCustomLineItemVersionsFilter' {..} =
    Prelude.rnf billingPeriodRange

instance Data.ToJSON ListCustomLineItemVersionsFilter where
  toJSON ListCustomLineItemVersionsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingPeriodRange" Data..=)
              Prelude.<$> billingPeriodRange
          ]
      )
