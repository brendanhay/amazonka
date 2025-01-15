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
-- Module      : Amazonka.BillingConductor.Types.ListCustomLineItemsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListCustomLineItemsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter that specifies the custom line items and billing groups to
-- retrieve FFLI information.
--
-- /See:/ 'newListCustomLineItemsFilter' smart constructor.
data ListCustomLineItemsFilter = ListCustomLineItemsFilter'
  { -- | A list of custom line item ARNs to retrieve information.
    arns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The billing group Amazon Resource Names (ARNs) to retrieve information.
    billingGroups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of custom line items to retrieve information.
    names :: Prelude.Maybe (Prelude.NonEmpty (Data.Sensitive Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'listCustomLineItemsFilter_arns' - A list of custom line item ARNs to retrieve information.
--
-- 'billingGroups', 'listCustomLineItemsFilter_billingGroups' - The billing group Amazon Resource Names (ARNs) to retrieve information.
--
-- 'names', 'listCustomLineItemsFilter_names' - A list of custom line items to retrieve information.
newListCustomLineItemsFilter ::
  ListCustomLineItemsFilter
newListCustomLineItemsFilter =
  ListCustomLineItemsFilter'
    { arns = Prelude.Nothing,
      billingGroups = Prelude.Nothing,
      names = Prelude.Nothing
    }

-- | A list of custom line item ARNs to retrieve information.
listCustomLineItemsFilter_arns :: Lens.Lens' ListCustomLineItemsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listCustomLineItemsFilter_arns = Lens.lens (\ListCustomLineItemsFilter' {arns} -> arns) (\s@ListCustomLineItemsFilter' {} a -> s {arns = a} :: ListCustomLineItemsFilter) Prelude.. Lens.mapping Lens.coerced

-- | The billing group Amazon Resource Names (ARNs) to retrieve information.
listCustomLineItemsFilter_billingGroups :: Lens.Lens' ListCustomLineItemsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listCustomLineItemsFilter_billingGroups = Lens.lens (\ListCustomLineItemsFilter' {billingGroups} -> billingGroups) (\s@ListCustomLineItemsFilter' {} a -> s {billingGroups = a} :: ListCustomLineItemsFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of custom line items to retrieve information.
listCustomLineItemsFilter_names :: Lens.Lens' ListCustomLineItemsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listCustomLineItemsFilter_names = Lens.lens (\ListCustomLineItemsFilter' {names} -> names) (\s@ListCustomLineItemsFilter' {} a -> s {names = a} :: ListCustomLineItemsFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListCustomLineItemsFilter where
  hashWithSalt _salt ListCustomLineItemsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` arns
      `Prelude.hashWithSalt` billingGroups
      `Prelude.hashWithSalt` names

instance Prelude.NFData ListCustomLineItemsFilter where
  rnf ListCustomLineItemsFilter' {..} =
    Prelude.rnf arns `Prelude.seq`
      Prelude.rnf billingGroups `Prelude.seq`
        Prelude.rnf names

instance Data.ToJSON ListCustomLineItemsFilter where
  toJSON ListCustomLineItemsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arns" Data..=) Prelude.<$> arns,
            ("BillingGroups" Data..=) Prelude.<$> billingGroups,
            ("Names" Data..=) Prelude.<$> names
          ]
      )
