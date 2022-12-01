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
-- Module      : Amazonka.MacieV2.Types.AdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AdminAccount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.AdminStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the delegated Amazon Macie administrator
-- account for an organization in Organizations.
--
-- /See:/ 'newAdminAccount' smart constructor.
data AdminAccount = AdminAccount'
  { -- | The current status of the account as the delegated Amazon Macie
    -- administrator account for the organization.
    status :: Prelude.Maybe AdminStatus,
    -- | The Amazon Web Services account ID for the account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'adminAccount_status' - The current status of the account as the delegated Amazon Macie
-- administrator account for the organization.
--
-- 'accountId', 'adminAccount_accountId' - The Amazon Web Services account ID for the account.
newAdminAccount ::
  AdminAccount
newAdminAccount =
  AdminAccount'
    { status = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The current status of the account as the delegated Amazon Macie
-- administrator account for the organization.
adminAccount_status :: Lens.Lens' AdminAccount (Prelude.Maybe AdminStatus)
adminAccount_status = Lens.lens (\AdminAccount' {status} -> status) (\s@AdminAccount' {} a -> s {status = a} :: AdminAccount)

-- | The Amazon Web Services account ID for the account.
adminAccount_accountId :: Lens.Lens' AdminAccount (Prelude.Maybe Prelude.Text)
adminAccount_accountId = Lens.lens (\AdminAccount' {accountId} -> accountId) (\s@AdminAccount' {} a -> s {accountId = a} :: AdminAccount)

instance Core.FromJSON AdminAccount where
  parseJSON =
    Core.withObject
      "AdminAccount"
      ( \x ->
          AdminAccount'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable AdminAccount where
  hashWithSalt _salt AdminAccount' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData AdminAccount where
  rnf AdminAccount' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf accountId
