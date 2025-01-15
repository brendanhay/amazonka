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
-- Module      : Amazonka.Inspector2.Types.Account
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Account where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ResourceStatus
import Amazonka.Inspector2.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Web Services account within your environment that Amazon
-- Inspector has been enabled for.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Text,
    -- | Details of the status of Amazon Inspector scans by resource type.
    resourceStatus :: ResourceStatus,
    -- | The status of Amazon Inspector for the account.
    status :: Status
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
-- 'accountId', 'account_accountId' - The ID of the Amazon Web Services account.
--
-- 'resourceStatus', 'account_resourceStatus' - Details of the status of Amazon Inspector scans by resource type.
--
-- 'status', 'account_status' - The status of Amazon Inspector for the account.
newAccount ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'resourceStatus'
  ResourceStatus ->
  -- | 'status'
  Status ->
  Account
newAccount pAccountId_ pResourceStatus_ pStatus_ =
  Account'
    { accountId = pAccountId_,
      resourceStatus = pResourceStatus_,
      status = pStatus_
    }

-- | The ID of the Amazon Web Services account.
account_accountId :: Lens.Lens' Account Prelude.Text
account_accountId = Lens.lens (\Account' {accountId} -> accountId) (\s@Account' {} a -> s {accountId = a} :: Account)

-- | Details of the status of Amazon Inspector scans by resource type.
account_resourceStatus :: Lens.Lens' Account ResourceStatus
account_resourceStatus = Lens.lens (\Account' {resourceStatus} -> resourceStatus) (\s@Account' {} a -> s {resourceStatus = a} :: Account)

-- | The status of Amazon Inspector for the account.
account_status :: Lens.Lens' Account Status
account_status = Lens.lens (\Account' {status} -> status) (\s@Account' {} a -> s {status = a} :: Account)

instance Data.FromJSON Account where
  parseJSON =
    Data.withObject
      "Account"
      ( \x ->
          Account'
            Prelude.<$> (x Data..: "accountId")
            Prelude.<*> (x Data..: "resourceStatus")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable Account where
  hashWithSalt _salt Account' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` resourceStatus
      `Prelude.hashWithSalt` status

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf resourceStatus `Prelude.seq`
        Prelude.rnf status
