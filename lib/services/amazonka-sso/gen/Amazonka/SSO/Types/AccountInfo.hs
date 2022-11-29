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
-- Module      : Amazonka.SSO.Types.AccountInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSO.Types.AccountInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about your AWS account.
--
-- /See:/ 'newAccountInfo' smart constructor.
data AccountInfo = AccountInfo'
  { -- | The identifier of the AWS account that is assigned to the user.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The display name of the AWS account that is assigned to the user.
    accountName :: Prelude.Maybe Prelude.Text,
    -- | The email address of the AWS account that is assigned to the user.
    emailAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountInfo_accountId' - The identifier of the AWS account that is assigned to the user.
--
-- 'accountName', 'accountInfo_accountName' - The display name of the AWS account that is assigned to the user.
--
-- 'emailAddress', 'accountInfo_emailAddress' - The email address of the AWS account that is assigned to the user.
newAccountInfo ::
  AccountInfo
newAccountInfo =
  AccountInfo'
    { accountId = Prelude.Nothing,
      accountName = Prelude.Nothing,
      emailAddress = Prelude.Nothing
    }

-- | The identifier of the AWS account that is assigned to the user.
accountInfo_accountId :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_accountId = Lens.lens (\AccountInfo' {accountId} -> accountId) (\s@AccountInfo' {} a -> s {accountId = a} :: AccountInfo)

-- | The display name of the AWS account that is assigned to the user.
accountInfo_accountName :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_accountName = Lens.lens (\AccountInfo' {accountName} -> accountName) (\s@AccountInfo' {} a -> s {accountName = a} :: AccountInfo)

-- | The email address of the AWS account that is assigned to the user.
accountInfo_emailAddress :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_emailAddress = Lens.lens (\AccountInfo' {emailAddress} -> emailAddress) (\s@AccountInfo' {} a -> s {emailAddress = a} :: AccountInfo)

instance Core.FromJSON AccountInfo where
  parseJSON =
    Core.withObject
      "AccountInfo"
      ( \x ->
          AccountInfo'
            Prelude.<$> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "accountName")
            Prelude.<*> (x Core..:? "emailAddress")
      )

instance Prelude.Hashable AccountInfo where
  hashWithSalt _salt AccountInfo' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData AccountInfo where
  rnf AccountInfo' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf emailAddress
