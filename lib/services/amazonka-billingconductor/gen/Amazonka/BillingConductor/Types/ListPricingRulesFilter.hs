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
-- Module      : Amazonka.BillingConductor.Types.ListPricingRulesFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListPricingRulesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The filter that specifies criteria that the pricing rules returned by
-- the @ListPricingRules@ API will adhere to.
--
-- /See:/ 'newListPricingRulesFilter' smart constructor.
data ListPricingRulesFilter = ListPricingRulesFilter'
  { -- | A list containing the pricing rule Amazon Resource Names (ARNs) to
    -- include in the API response.
    arns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingRulesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'listPricingRulesFilter_arns' - A list containing the pricing rule Amazon Resource Names (ARNs) to
-- include in the API response.
newListPricingRulesFilter ::
  ListPricingRulesFilter
newListPricingRulesFilter =
  ListPricingRulesFilter' {arns = Prelude.Nothing}

-- | A list containing the pricing rule Amazon Resource Names (ARNs) to
-- include in the API response.
listPricingRulesFilter_arns :: Lens.Lens' ListPricingRulesFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listPricingRulesFilter_arns = Lens.lens (\ListPricingRulesFilter' {arns} -> arns) (\s@ListPricingRulesFilter' {} a -> s {arns = a} :: ListPricingRulesFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListPricingRulesFilter where
  hashWithSalt _salt ListPricingRulesFilter' {..} =
    _salt `Prelude.hashWithSalt` arns

instance Prelude.NFData ListPricingRulesFilter where
  rnf ListPricingRulesFilter' {..} = Prelude.rnf arns

instance Data.ToJSON ListPricingRulesFilter where
  toJSON ListPricingRulesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Arns" Data..=) Prelude.<$> arns]
      )
