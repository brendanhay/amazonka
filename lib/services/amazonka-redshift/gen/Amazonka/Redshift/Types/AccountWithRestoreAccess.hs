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
-- Module      : Amazonka.Redshift.Types.AccountWithRestoreAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.AccountWithRestoreAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes an Amazon Web Services account authorized to restore a
-- snapshot.
--
-- /See:/ 'newAccountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { -- | The identifier of an Amazon Web Services support account authorized to
    -- restore a snapshot. For Amazon Web Services Support, the identifier is
    -- @amazon-redshift-support@.
    accountAlias :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an Amazon Web Services account authorized to restore a
    -- snapshot.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountWithRestoreAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAlias', 'accountWithRestoreAccess_accountAlias' - The identifier of an Amazon Web Services support account authorized to
-- restore a snapshot. For Amazon Web Services Support, the identifier is
-- @amazon-redshift-support@.
--
-- 'accountId', 'accountWithRestoreAccess_accountId' - The identifier of an Amazon Web Services account authorized to restore a
-- snapshot.
newAccountWithRestoreAccess ::
  AccountWithRestoreAccess
newAccountWithRestoreAccess =
  AccountWithRestoreAccess'
    { accountAlias =
        Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The identifier of an Amazon Web Services support account authorized to
-- restore a snapshot. For Amazon Web Services Support, the identifier is
-- @amazon-redshift-support@.
accountWithRestoreAccess_accountAlias :: Lens.Lens' AccountWithRestoreAccess (Prelude.Maybe Prelude.Text)
accountWithRestoreAccess_accountAlias = Lens.lens (\AccountWithRestoreAccess' {accountAlias} -> accountAlias) (\s@AccountWithRestoreAccess' {} a -> s {accountAlias = a} :: AccountWithRestoreAccess)

-- | The identifier of an Amazon Web Services account authorized to restore a
-- snapshot.
accountWithRestoreAccess_accountId :: Lens.Lens' AccountWithRestoreAccess (Prelude.Maybe Prelude.Text)
accountWithRestoreAccess_accountId = Lens.lens (\AccountWithRestoreAccess' {accountId} -> accountId) (\s@AccountWithRestoreAccess' {} a -> s {accountId = a} :: AccountWithRestoreAccess)

instance Core.FromXML AccountWithRestoreAccess where
  parseXML x =
    AccountWithRestoreAccess'
      Prelude.<$> (x Core..@? "AccountAlias")
      Prelude.<*> (x Core..@? "AccountId")

instance Prelude.Hashable AccountWithRestoreAccess where
  hashWithSalt _salt AccountWithRestoreAccess' {..} =
    _salt `Prelude.hashWithSalt` accountAlias
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData AccountWithRestoreAccess where
  rnf AccountWithRestoreAccess' {..} =
    Prelude.rnf accountAlias
      `Prelude.seq` Prelude.rnf accountId
