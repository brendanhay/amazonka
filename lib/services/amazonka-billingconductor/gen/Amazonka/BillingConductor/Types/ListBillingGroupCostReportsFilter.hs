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
-- Module      : Amazonka.BillingConductor.Types.ListBillingGroupCostReportsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListBillingGroupCostReportsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The filter used to retrieve specific @BillingGroupCostReportElements@.
--
-- /See:/ 'newListBillingGroupCostReportsFilter' smart constructor.
data ListBillingGroupCostReportsFilter = ListBillingGroupCostReportsFilter'
  { -- | The list of Amazon Resource Names (ARNs) used to filter billing groups
    -- to retrieve reports.
    billingGroupArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroupCostReportsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingGroupArns', 'listBillingGroupCostReportsFilter_billingGroupArns' - The list of Amazon Resource Names (ARNs) used to filter billing groups
-- to retrieve reports.
newListBillingGroupCostReportsFilter ::
  ListBillingGroupCostReportsFilter
newListBillingGroupCostReportsFilter =
  ListBillingGroupCostReportsFilter'
    { billingGroupArns =
        Prelude.Nothing
    }

-- | The list of Amazon Resource Names (ARNs) used to filter billing groups
-- to retrieve reports.
listBillingGroupCostReportsFilter_billingGroupArns :: Lens.Lens' ListBillingGroupCostReportsFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listBillingGroupCostReportsFilter_billingGroupArns = Lens.lens (\ListBillingGroupCostReportsFilter' {billingGroupArns} -> billingGroupArns) (\s@ListBillingGroupCostReportsFilter' {} a -> s {billingGroupArns = a} :: ListBillingGroupCostReportsFilter) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ListBillingGroupCostReportsFilter
  where
  hashWithSalt
    _salt
    ListBillingGroupCostReportsFilter' {..} =
      _salt `Prelude.hashWithSalt` billingGroupArns

instance
  Prelude.NFData
    ListBillingGroupCostReportsFilter
  where
  rnf ListBillingGroupCostReportsFilter' {..} =
    Prelude.rnf billingGroupArns

instance
  Data.ToJSON
    ListBillingGroupCostReportsFilter
  where
  toJSON ListBillingGroupCostReportsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingGroupArns" Data..=)
              Prelude.<$> billingGroupArns
          ]
      )
