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
-- Module      : Amazonka.DrS.Types.Account
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.Account where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | AWS account.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | Account ID of AWS account.
    accountID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Account' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountID', 'account_accountID' - Account ID of AWS account.
newAccount ::
  Account
newAccount = Account' {accountID = Prelude.Nothing}

-- | Account ID of AWS account.
account_accountID :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_accountID = Lens.lens (\Account' {accountID} -> accountID) (\s@Account' {} a -> s {accountID = a} :: Account)

instance Data.FromJSON Account where
  parseJSON =
    Data.withObject
      "Account"
      ( \x ->
          Account' Prelude.<$> (x Data..:? "accountID")
      )

instance Prelude.Hashable Account where
  hashWithSalt _salt Account' {..} =
    _salt `Prelude.hashWithSalt` accountID

instance Prelude.NFData Account where
  rnf Account' {..} = Prelude.rnf accountID
