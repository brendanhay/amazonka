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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes an AWS customer account authorized to restore a snapshot.
--
-- /See:/ 'newAccountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { -- | The identifier of an AWS support account authorized to restore a
    -- snapshot. For AWS support, the identifier is @amazon-redshift-support@.
    accountAlias :: Core.Maybe Core.Text,
    -- | The identifier of an AWS customer account authorized to restore a
    -- snapshot.
    accountId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      accountId = Core.Nothing
    }

-- | The identifier of an AWS support account authorized to restore a
-- snapshot. For AWS support, the identifier is @amazon-redshift-support@.
accountWithRestoreAccess_accountAlias :: Lens.Lens' AccountWithRestoreAccess (Core.Maybe Core.Text)
accountWithRestoreAccess_accountAlias = Lens.lens (\AccountWithRestoreAccess' {accountAlias} -> accountAlias) (\s@AccountWithRestoreAccess' {} a -> s {accountAlias = a} :: AccountWithRestoreAccess)

-- | The identifier of an AWS customer account authorized to restore a
-- snapshot.
accountWithRestoreAccess_accountId :: Lens.Lens' AccountWithRestoreAccess (Core.Maybe Core.Text)
accountWithRestoreAccess_accountId = Lens.lens (\AccountWithRestoreAccess' {accountId} -> accountId) (\s@AccountWithRestoreAccess' {} a -> s {accountId = a} :: AccountWithRestoreAccess)

instance Core.FromXML AccountWithRestoreAccess where
  parseXML x =
    AccountWithRestoreAccess'
      Core.<$> (x Core..@? "AccountAlias")
      Core.<*> (x Core..@? "AccountId")

instance Core.Hashable AccountWithRestoreAccess

instance Core.NFData AccountWithRestoreAccess
