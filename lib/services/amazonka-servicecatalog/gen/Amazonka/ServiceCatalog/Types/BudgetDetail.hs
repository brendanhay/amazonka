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
-- Module      : Amazonka.ServiceCatalog.Types.BudgetDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.BudgetDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a budget.
--
-- /See:/ 'newBudgetDetail' smart constructor.
data BudgetDetail = BudgetDetail'
  { -- | Name of the associated budget.
    budgetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BudgetDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budgetName', 'budgetDetail_budgetName' - Name of the associated budget.
newBudgetDetail ::
  BudgetDetail
newBudgetDetail =
  BudgetDetail' {budgetName = Prelude.Nothing}

-- | Name of the associated budget.
budgetDetail_budgetName :: Lens.Lens' BudgetDetail (Prelude.Maybe Prelude.Text)
budgetDetail_budgetName = Lens.lens (\BudgetDetail' {budgetName} -> budgetName) (\s@BudgetDetail' {} a -> s {budgetName = a} :: BudgetDetail)

instance Data.FromJSON BudgetDetail where
  parseJSON =
    Data.withObject
      "BudgetDetail"
      ( \x ->
          BudgetDetail' Prelude.<$> (x Data..:? "BudgetName")
      )

instance Prelude.Hashable BudgetDetail where
  hashWithSalt _salt BudgetDetail' {..} =
    _salt `Prelude.hashWithSalt` budgetName

instance Prelude.NFData BudgetDetail where
  rnf BudgetDetail' {..} = Prelude.rnf budgetName
