{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.AccountDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccountDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the account.
--
-- /See:/ 'newAccountDetail' smart constructor.
data AccountDetail = AccountDetail'
  { -- | The member account ID.
    accountId :: Prelude.Text,
    -- | The email address of the member account.
    email :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountDetail_accountId' - The member account ID.
--
-- 'email', 'accountDetail_email' - The email address of the member account.
newAccountDetail ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'email'
  Prelude.Text ->
  AccountDetail
newAccountDetail pAccountId_ pEmail_ =
  AccountDetail'
    { accountId = pAccountId_,
      email = pEmail_
    }

-- | The member account ID.
accountDetail_accountId :: Lens.Lens' AccountDetail Prelude.Text
accountDetail_accountId = Lens.lens (\AccountDetail' {accountId} -> accountId) (\s@AccountDetail' {} a -> s {accountId = a} :: AccountDetail)

-- | The email address of the member account.
accountDetail_email :: Lens.Lens' AccountDetail Prelude.Text
accountDetail_email = Lens.lens (\AccountDetail' {email} -> email) (\s@AccountDetail' {} a -> s {email = a} :: AccountDetail)

instance Prelude.Hashable AccountDetail

instance Prelude.NFData AccountDetail

instance Prelude.ToJSON AccountDetail where
  toJSON AccountDetail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("accountId" Prelude..= accountId),
            Prelude.Just ("email" Prelude..= email)
          ]
      )
