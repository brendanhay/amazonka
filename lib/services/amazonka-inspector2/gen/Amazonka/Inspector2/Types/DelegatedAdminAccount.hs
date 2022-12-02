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
-- Module      : Amazonka.Inspector2.Types.DelegatedAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.DelegatedAdminAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.DelegatedAdminStatus
import qualified Amazonka.Prelude as Prelude

-- | Details of the Amazon Inspector delegated administrator for your
-- organization.
--
-- /See:/ 'newDelegatedAdminAccount' smart constructor.
data DelegatedAdminAccount = DelegatedAdminAccount'
  { -- | The status of the Amazon Inspector delegated administrator.
    status :: Prelude.Maybe DelegatedAdminStatus,
    -- | The Amazon Web Services account ID of the Amazon Inspector delegated
    -- administrator for your organization.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelegatedAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'delegatedAdminAccount_status' - The status of the Amazon Inspector delegated administrator.
--
-- 'accountId', 'delegatedAdminAccount_accountId' - The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator for your organization.
newDelegatedAdminAccount ::
  DelegatedAdminAccount
newDelegatedAdminAccount =
  DelegatedAdminAccount'
    { status = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The status of the Amazon Inspector delegated administrator.
delegatedAdminAccount_status :: Lens.Lens' DelegatedAdminAccount (Prelude.Maybe DelegatedAdminStatus)
delegatedAdminAccount_status = Lens.lens (\DelegatedAdminAccount' {status} -> status) (\s@DelegatedAdminAccount' {} a -> s {status = a} :: DelegatedAdminAccount)

-- | The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator for your organization.
delegatedAdminAccount_accountId :: Lens.Lens' DelegatedAdminAccount (Prelude.Maybe Prelude.Text)
delegatedAdminAccount_accountId = Lens.lens (\DelegatedAdminAccount' {accountId} -> accountId) (\s@DelegatedAdminAccount' {} a -> s {accountId = a} :: DelegatedAdminAccount)

instance Data.FromJSON DelegatedAdminAccount where
  parseJSON =
    Data.withObject
      "DelegatedAdminAccount"
      ( \x ->
          DelegatedAdminAccount'
            Prelude.<$> (x Data..:? "status")
            Prelude.<*> (x Data..:? "accountId")
      )

instance Prelude.Hashable DelegatedAdminAccount where
  hashWithSalt _salt DelegatedAdminAccount' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData DelegatedAdminAccount where
  rnf DelegatedAdminAccount' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf accountId
