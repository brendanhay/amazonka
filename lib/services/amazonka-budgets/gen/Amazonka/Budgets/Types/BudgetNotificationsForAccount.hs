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
-- Module      : Amazonka.Budgets.Types.BudgetNotificationsForAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.BudgetNotificationsForAccount where

import Amazonka.Budgets.Types.Notification
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The budget name and associated notifications for an account.
--
-- /See:/ 'newBudgetNotificationsForAccount' smart constructor.
data BudgetNotificationsForAccount = BudgetNotificationsForAccount'
  { notifications :: Prelude.Maybe [Notification],
    budgetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BudgetNotificationsForAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notifications', 'budgetNotificationsForAccount_notifications' - Undocumented member.
--
-- 'budgetName', 'budgetNotificationsForAccount_budgetName' - Undocumented member.
newBudgetNotificationsForAccount ::
  BudgetNotificationsForAccount
newBudgetNotificationsForAccount =
  BudgetNotificationsForAccount'
    { notifications =
        Prelude.Nothing,
      budgetName = Prelude.Nothing
    }

-- | Undocumented member.
budgetNotificationsForAccount_notifications :: Lens.Lens' BudgetNotificationsForAccount (Prelude.Maybe [Notification])
budgetNotificationsForAccount_notifications = Lens.lens (\BudgetNotificationsForAccount' {notifications} -> notifications) (\s@BudgetNotificationsForAccount' {} a -> s {notifications = a} :: BudgetNotificationsForAccount) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
budgetNotificationsForAccount_budgetName :: Lens.Lens' BudgetNotificationsForAccount (Prelude.Maybe Prelude.Text)
budgetNotificationsForAccount_budgetName = Lens.lens (\BudgetNotificationsForAccount' {budgetName} -> budgetName) (\s@BudgetNotificationsForAccount' {} a -> s {budgetName = a} :: BudgetNotificationsForAccount)

instance Data.FromJSON BudgetNotificationsForAccount where
  parseJSON =
    Data.withObject
      "BudgetNotificationsForAccount"
      ( \x ->
          BudgetNotificationsForAccount'
            Prelude.<$> (x Data..:? "Notifications" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BudgetName")
      )

instance
  Prelude.Hashable
    BudgetNotificationsForAccount
  where
  hashWithSalt _salt BudgetNotificationsForAccount' {..} =
    _salt `Prelude.hashWithSalt` notifications
      `Prelude.hashWithSalt` budgetName

instance Prelude.NFData BudgetNotificationsForAccount where
  rnf BudgetNotificationsForAccount' {..} =
    Prelude.rnf notifications
      `Prelude.seq` Prelude.rnf budgetName
