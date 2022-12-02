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
-- Module      : Amazonka.MacieV2.Types.AccountDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AccountDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the details of an account to associate with an Amazon Macie
-- administrator account.
--
-- /See:/ 'newAccountDetail' smart constructor.
data AccountDetail = AccountDetail'
  { -- | The email address for the account.
    email :: Prelude.Text,
    -- | The Amazon Web Services account ID for the account.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'accountDetail_email' - The email address for the account.
--
-- 'accountId', 'accountDetail_accountId' - The Amazon Web Services account ID for the account.
newAccountDetail ::
  -- | 'email'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  AccountDetail
newAccountDetail pEmail_ pAccountId_ =
  AccountDetail'
    { email = pEmail_,
      accountId = pAccountId_
    }

-- | The email address for the account.
accountDetail_email :: Lens.Lens' AccountDetail Prelude.Text
accountDetail_email = Lens.lens (\AccountDetail' {email} -> email) (\s@AccountDetail' {} a -> s {email = a} :: AccountDetail)

-- | The Amazon Web Services account ID for the account.
accountDetail_accountId :: Lens.Lens' AccountDetail Prelude.Text
accountDetail_accountId = Lens.lens (\AccountDetail' {accountId} -> accountId) (\s@AccountDetail' {} a -> s {accountId = a} :: AccountDetail)

instance Prelude.Hashable AccountDetail where
  hashWithSalt _salt AccountDetail' {..} =
    _salt `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData AccountDetail where
  rnf AccountDetail' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToJSON AccountDetail where
  toJSON AccountDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("email" Data..= email),
            Prelude.Just ("accountId" Data..= accountId)
          ]
      )
