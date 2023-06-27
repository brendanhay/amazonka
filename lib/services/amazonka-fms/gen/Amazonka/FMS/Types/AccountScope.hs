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
-- Module      : Amazonka.FMS.Types.AccountScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AccountScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures the accounts within the administrator\'s Organizations
-- organization that the specified Firewall Manager administrator can apply
-- policies to.
--
-- /See:/ 'newAccountScope' smart constructor.
data AccountScope = AccountScope'
  { -- | The list of accounts within the organization that the specified Firewall
    -- Manager administrator either can or cannot apply policies to, based on
    -- the value of @ExcludeSpecifiedAccounts@. If @ExcludeSpecifiedAccounts@
    -- is set to @true@, then the Firewall Manager administrator can apply
    -- policies to all members of the organization except for the accounts in
    -- this list. If @ExcludeSpecifiedAccounts@ is set to @false@, then the
    -- Firewall Manager administrator can only apply policies to the accounts
    -- in this list.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | A boolean value that indicates if the administrator can apply policies
    -- to all accounts within an organization. If true, the administrator can
    -- apply policies to all accounts within the organization. You can either
    -- enable management of all accounts through this operation, or you can
    -- specify a list of accounts to manage in @AccountScope$Accounts@. You
    -- cannot specify both.
    allAccountsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value that excludes the accounts in @AccountScope$Accounts@
    -- from the administrator\'s scope. If true, the Firewall Manager
    -- administrator can apply policies to all members of the organization
    -- except for the accounts listed in @AccountScope$Accounts@. You can
    -- either specify a list of accounts to exclude by @AccountScope$Accounts@,
    -- or you can enable management of all accounts by
    -- @AccountScope$AllAccountsEnabled@. You cannot specify both.
    excludeSpecifiedAccounts :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'accountScope_accounts' - The list of accounts within the organization that the specified Firewall
-- Manager administrator either can or cannot apply policies to, based on
-- the value of @ExcludeSpecifiedAccounts@. If @ExcludeSpecifiedAccounts@
-- is set to @true@, then the Firewall Manager administrator can apply
-- policies to all members of the organization except for the accounts in
-- this list. If @ExcludeSpecifiedAccounts@ is set to @false@, then the
-- Firewall Manager administrator can only apply policies to the accounts
-- in this list.
--
-- 'allAccountsEnabled', 'accountScope_allAccountsEnabled' - A boolean value that indicates if the administrator can apply policies
-- to all accounts within an organization. If true, the administrator can
-- apply policies to all accounts within the organization. You can either
-- enable management of all accounts through this operation, or you can
-- specify a list of accounts to manage in @AccountScope$Accounts@. You
-- cannot specify both.
--
-- 'excludeSpecifiedAccounts', 'accountScope_excludeSpecifiedAccounts' - A boolean value that excludes the accounts in @AccountScope$Accounts@
-- from the administrator\'s scope. If true, the Firewall Manager
-- administrator can apply policies to all members of the organization
-- except for the accounts listed in @AccountScope$Accounts@. You can
-- either specify a list of accounts to exclude by @AccountScope$Accounts@,
-- or you can enable management of all accounts by
-- @AccountScope$AllAccountsEnabled@. You cannot specify both.
newAccountScope ::
  AccountScope
newAccountScope =
  AccountScope'
    { accounts = Prelude.Nothing,
      allAccountsEnabled = Prelude.Nothing,
      excludeSpecifiedAccounts = Prelude.Nothing
    }

-- | The list of accounts within the organization that the specified Firewall
-- Manager administrator either can or cannot apply policies to, based on
-- the value of @ExcludeSpecifiedAccounts@. If @ExcludeSpecifiedAccounts@
-- is set to @true@, then the Firewall Manager administrator can apply
-- policies to all members of the organization except for the accounts in
-- this list. If @ExcludeSpecifiedAccounts@ is set to @false@, then the
-- Firewall Manager administrator can only apply policies to the accounts
-- in this list.
accountScope_accounts :: Lens.Lens' AccountScope (Prelude.Maybe [Prelude.Text])
accountScope_accounts = Lens.lens (\AccountScope' {accounts} -> accounts) (\s@AccountScope' {} a -> s {accounts = a} :: AccountScope) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value that indicates if the administrator can apply policies
-- to all accounts within an organization. If true, the administrator can
-- apply policies to all accounts within the organization. You can either
-- enable management of all accounts through this operation, or you can
-- specify a list of accounts to manage in @AccountScope$Accounts@. You
-- cannot specify both.
accountScope_allAccountsEnabled :: Lens.Lens' AccountScope (Prelude.Maybe Prelude.Bool)
accountScope_allAccountsEnabled = Lens.lens (\AccountScope' {allAccountsEnabled} -> allAccountsEnabled) (\s@AccountScope' {} a -> s {allAccountsEnabled = a} :: AccountScope)

-- | A boolean value that excludes the accounts in @AccountScope$Accounts@
-- from the administrator\'s scope. If true, the Firewall Manager
-- administrator can apply policies to all members of the organization
-- except for the accounts listed in @AccountScope$Accounts@. You can
-- either specify a list of accounts to exclude by @AccountScope$Accounts@,
-- or you can enable management of all accounts by
-- @AccountScope$AllAccountsEnabled@. You cannot specify both.
accountScope_excludeSpecifiedAccounts :: Lens.Lens' AccountScope (Prelude.Maybe Prelude.Bool)
accountScope_excludeSpecifiedAccounts = Lens.lens (\AccountScope' {excludeSpecifiedAccounts} -> excludeSpecifiedAccounts) (\s@AccountScope' {} a -> s {excludeSpecifiedAccounts = a} :: AccountScope)

instance Data.FromJSON AccountScope where
  parseJSON =
    Data.withObject
      "AccountScope"
      ( \x ->
          AccountScope'
            Prelude.<$> (x Data..:? "Accounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AllAccountsEnabled")
            Prelude.<*> (x Data..:? "ExcludeSpecifiedAccounts")
      )

instance Prelude.Hashable AccountScope where
  hashWithSalt _salt AccountScope' {..} =
    _salt
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` allAccountsEnabled
      `Prelude.hashWithSalt` excludeSpecifiedAccounts

instance Prelude.NFData AccountScope where
  rnf AccountScope' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf allAccountsEnabled
      `Prelude.seq` Prelude.rnf excludeSpecifiedAccounts

instance Data.ToJSON AccountScope where
  toJSON AccountScope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Accounts" Data..=) Prelude.<$> accounts,
            ("AllAccountsEnabled" Data..=)
              Prelude.<$> allAccountsEnabled,
            ("ExcludeSpecifiedAccounts" Data..=)
              Prelude.<$> excludeSpecifiedAccounts
          ]
      )
