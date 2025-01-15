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
-- Module      : Amazonka.NetworkManager.Types.AccountStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.AccountStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the current status of an account within an Amazon Web Services
-- Organization, including service-linked roles (SLRs).
--
-- /See:/ 'newAccountStatus' smart constructor.
data AccountStatus = AccountStatus'
  { -- | The ID of an account within the Amazon Web Services Organization.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The status of SLR deployment for the account.
    sLRDeploymentStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountStatus_accountId' - The ID of an account within the Amazon Web Services Organization.
--
-- 'sLRDeploymentStatus', 'accountStatus_sLRDeploymentStatus' - The status of SLR deployment for the account.
newAccountStatus ::
  AccountStatus
newAccountStatus =
  AccountStatus'
    { accountId = Prelude.Nothing,
      sLRDeploymentStatus = Prelude.Nothing
    }

-- | The ID of an account within the Amazon Web Services Organization.
accountStatus_accountId :: Lens.Lens' AccountStatus (Prelude.Maybe Prelude.Text)
accountStatus_accountId = Lens.lens (\AccountStatus' {accountId} -> accountId) (\s@AccountStatus' {} a -> s {accountId = a} :: AccountStatus)

-- | The status of SLR deployment for the account.
accountStatus_sLRDeploymentStatus :: Lens.Lens' AccountStatus (Prelude.Maybe Prelude.Text)
accountStatus_sLRDeploymentStatus = Lens.lens (\AccountStatus' {sLRDeploymentStatus} -> sLRDeploymentStatus) (\s@AccountStatus' {} a -> s {sLRDeploymentStatus = a} :: AccountStatus)

instance Data.FromJSON AccountStatus where
  parseJSON =
    Data.withObject
      "AccountStatus"
      ( \x ->
          AccountStatus'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "SLRDeploymentStatus")
      )

instance Prelude.Hashable AccountStatus where
  hashWithSalt _salt AccountStatus' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` sLRDeploymentStatus

instance Prelude.NFData AccountStatus where
  rnf AccountStatus' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf sLRDeploymentStatus
