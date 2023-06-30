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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { budgetName :: Prelude.Maybe Prelude.Text,
    notifications :: Prelude.Maybe [Notification]
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
-- 'budgetName', 'budgetNotificationsForAccount_budgetName' - Undocumented member.
--
-- 'notifications', 'budgetNotificationsForAccount_notifications' - Undocumented member.
newBudgetNotificationsForAccount ::
  BudgetNotificationsForAccount
newBudgetNotificationsForAccount =
  BudgetNotificationsForAccount'
    { budgetName =
        Prelude.Nothing,
      notifications = Prelude.Nothing
    }

-- | Undocumented member.
budgetNotificationsForAccount_budgetName :: Lens.Lens' BudgetNotificationsForAccount (Prelude.Maybe Prelude.Text)
budgetNotificationsForAccount_budgetName = Lens.lens (\BudgetNotificationsForAccount' {budgetName} -> budgetName) (\s@BudgetNotificationsForAccount' {} a -> s {budgetName = a} :: BudgetNotificationsForAccount)

-- | Undocumented member.
budgetNotificationsForAccount_notifications :: Lens.Lens' BudgetNotificationsForAccount (Prelude.Maybe [Notification])
budgetNotificationsForAccount_notifications = Lens.lens (\BudgetNotificationsForAccount' {notifications} -> notifications) (\s@BudgetNotificationsForAccount' {} a -> s {notifications = a} :: BudgetNotificationsForAccount) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BudgetNotificationsForAccount where
  parseJSON =
    Data.withObject
      "BudgetNotificationsForAccount"
      ( \x ->
          BudgetNotificationsForAccount'
            Prelude.<$> (x Data..:? "BudgetName")
            Prelude.<*> (x Data..:? "Notifications" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BudgetNotificationsForAccount
  where
  hashWithSalt _salt BudgetNotificationsForAccount' {..} =
    _salt
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` notifications

instance Prelude.NFData BudgetNotificationsForAccount where
  rnf BudgetNotificationsForAccount' {..} =
    Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf notifications
