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
-- Module      : Network.AWS.Redshift.Types.AccountWithRestoreAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AccountWithRestoreAccess where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes an AWS customer account authorized to restore a snapshot.
--
-- /See:/ 'newAccountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { -- | The identifier of an AWS support account authorized to restore a
    -- snapshot. For AWS support, the identifier is @amazon-redshift-support@.
    accountAlias :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an AWS customer account authorized to restore a
    -- snapshot.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountWithRestoreAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAlias', 'accountWithRestoreAccess_accountAlias' - The identifier of an AWS support account authorized to restore a
-- snapshot. For AWS support, the identifier is @amazon-redshift-support@.
--
-- 'accountId', 'accountWithRestoreAccess_accountId' - The identifier of an AWS customer account authorized to restore a
-- snapshot.
newAccountWithRestoreAccess ::
  AccountWithRestoreAccess
newAccountWithRestoreAccess =
  AccountWithRestoreAccess'
    { accountAlias =
        Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The identifier of an AWS support account authorized to restore a
-- snapshot. For AWS support, the identifier is @amazon-redshift-support@.
accountWithRestoreAccess_accountAlias :: Lens.Lens' AccountWithRestoreAccess (Prelude.Maybe Prelude.Text)
accountWithRestoreAccess_accountAlias = Lens.lens (\AccountWithRestoreAccess' {accountAlias} -> accountAlias) (\s@AccountWithRestoreAccess' {} a -> s {accountAlias = a} :: AccountWithRestoreAccess)

-- | The identifier of an AWS customer account authorized to restore a
-- snapshot.
accountWithRestoreAccess_accountId :: Lens.Lens' AccountWithRestoreAccess (Prelude.Maybe Prelude.Text)
accountWithRestoreAccess_accountId = Lens.lens (\AccountWithRestoreAccess' {accountId} -> accountId) (\s@AccountWithRestoreAccess' {} a -> s {accountId = a} :: AccountWithRestoreAccess)

instance Prelude.FromXML AccountWithRestoreAccess where
  parseXML x =
    AccountWithRestoreAccess'
      Prelude.<$> (x Prelude..@? "AccountAlias")
      Prelude.<*> (x Prelude..@? "AccountId")

instance Prelude.Hashable AccountWithRestoreAccess

instance Prelude.NFData AccountWithRestoreAccess
