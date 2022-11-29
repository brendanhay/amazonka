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
-- Module      : Amazonka.BillingConductor.Types.ListBillingGroupsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListBillingGroupsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The filter that specifies the billing groups and pricing plans to
-- retrieve billing group information.
--
-- /See:/ 'newListBillingGroupsFilter' smart constructor.
data ListBillingGroupsFilter = ListBillingGroupsFilter'
  { -- | The list of billing group Amazon Resource Names (ARNs) to retrieve
    -- information.
    arns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The pricing plan Amazon Resource Names (ARNs) to retrieve information.
    pricingPlan :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroupsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'listBillingGroupsFilter_arns' - The list of billing group Amazon Resource Names (ARNs) to retrieve
-- information.
--
-- 'pricingPlan', 'listBillingGroupsFilter_pricingPlan' - The pricing plan Amazon Resource Names (ARNs) to retrieve information.
newListBillingGroupsFilter ::
  ListBillingGroupsFilter
newListBillingGroupsFilter =
  ListBillingGroupsFilter'
    { arns = Prelude.Nothing,
      pricingPlan = Prelude.Nothing
    }

-- | The list of billing group Amazon Resource Names (ARNs) to retrieve
-- information.
listBillingGroupsFilter_arns :: Lens.Lens' ListBillingGroupsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listBillingGroupsFilter_arns = Lens.lens (\ListBillingGroupsFilter' {arns} -> arns) (\s@ListBillingGroupsFilter' {} a -> s {arns = a} :: ListBillingGroupsFilter) Prelude.. Lens.mapping Lens.coerced

-- | The pricing plan Amazon Resource Names (ARNs) to retrieve information.
listBillingGroupsFilter_pricingPlan :: Lens.Lens' ListBillingGroupsFilter (Prelude.Maybe Prelude.Text)
listBillingGroupsFilter_pricingPlan = Lens.lens (\ListBillingGroupsFilter' {pricingPlan} -> pricingPlan) (\s@ListBillingGroupsFilter' {} a -> s {pricingPlan = a} :: ListBillingGroupsFilter)

instance Prelude.Hashable ListBillingGroupsFilter where
  hashWithSalt _salt ListBillingGroupsFilter' {..} =
    _salt `Prelude.hashWithSalt` arns
      `Prelude.hashWithSalt` pricingPlan

instance Prelude.NFData ListBillingGroupsFilter where
  rnf ListBillingGroupsFilter' {..} =
    Prelude.rnf arns
      `Prelude.seq` Prelude.rnf pricingPlan

instance Core.ToJSON ListBillingGroupsFilter where
  toJSON ListBillingGroupsFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Arns" Core..=) Prelude.<$> arns,
            ("PricingPlan" Core..=) Prelude.<$> pricingPlan
          ]
      )
