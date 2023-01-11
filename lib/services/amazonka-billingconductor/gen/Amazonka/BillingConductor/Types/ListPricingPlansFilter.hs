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
-- Module      : Amazonka.BillingConductor.Types.ListPricingPlansFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListPricingPlansFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The filter that specifies the Amazon Resource Names (ARNs) of pricing
-- plans, to retrieve pricing plan information.
--
-- /See:/ 'newListPricingPlansFilter' smart constructor.
data ListPricingPlansFilter = ListPricingPlansFilter'
  { -- | A list of pricing plan Amazon Resource Names (ARNs) to retrieve
    -- information.
    arns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricingPlansFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'listPricingPlansFilter_arns' - A list of pricing plan Amazon Resource Names (ARNs) to retrieve
-- information.
newListPricingPlansFilter ::
  ListPricingPlansFilter
newListPricingPlansFilter =
  ListPricingPlansFilter' {arns = Prelude.Nothing}

-- | A list of pricing plan Amazon Resource Names (ARNs) to retrieve
-- information.
listPricingPlansFilter_arns :: Lens.Lens' ListPricingPlansFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listPricingPlansFilter_arns = Lens.lens (\ListPricingPlansFilter' {arns} -> arns) (\s@ListPricingPlansFilter' {} a -> s {arns = a} :: ListPricingPlansFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListPricingPlansFilter where
  hashWithSalt _salt ListPricingPlansFilter' {..} =
    _salt `Prelude.hashWithSalt` arns

instance Prelude.NFData ListPricingPlansFilter where
  rnf ListPricingPlansFilter' {..} = Prelude.rnf arns

instance Data.ToJSON ListPricingPlansFilter where
  toJSON ListPricingPlansFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Arns" Data..=) Prelude.<$> arns]
      )
