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
-- Module      : Amazonka.SSM.Types.AccountSharingInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AccountSharingInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information includes the Amazon Web Services account ID where the
-- current document is shared and the version shared with that account.
--
-- /See:/ 'newAccountSharingInfo' smart constructor.
data AccountSharingInfo = AccountSharingInfo'
  { -- | The Amazon Web Services account ID where the current document is shared.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The version of the current document shared with the account.
    sharedDocumentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSharingInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountSharingInfo_accountId' - The Amazon Web Services account ID where the current document is shared.
--
-- 'sharedDocumentVersion', 'accountSharingInfo_sharedDocumentVersion' - The version of the current document shared with the account.
newAccountSharingInfo ::
  AccountSharingInfo
newAccountSharingInfo =
  AccountSharingInfo'
    { accountId = Prelude.Nothing,
      sharedDocumentVersion = Prelude.Nothing
    }

-- | The Amazon Web Services account ID where the current document is shared.
accountSharingInfo_accountId :: Lens.Lens' AccountSharingInfo (Prelude.Maybe Prelude.Text)
accountSharingInfo_accountId = Lens.lens (\AccountSharingInfo' {accountId} -> accountId) (\s@AccountSharingInfo' {} a -> s {accountId = a} :: AccountSharingInfo)

-- | The version of the current document shared with the account.
accountSharingInfo_sharedDocumentVersion :: Lens.Lens' AccountSharingInfo (Prelude.Maybe Prelude.Text)
accountSharingInfo_sharedDocumentVersion = Lens.lens (\AccountSharingInfo' {sharedDocumentVersion} -> sharedDocumentVersion) (\s@AccountSharingInfo' {} a -> s {sharedDocumentVersion = a} :: AccountSharingInfo)

instance Data.FromJSON AccountSharingInfo where
  parseJSON =
    Data.withObject
      "AccountSharingInfo"
      ( \x ->
          AccountSharingInfo'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "SharedDocumentVersion")
      )

instance Prelude.Hashable AccountSharingInfo where
  hashWithSalt _salt AccountSharingInfo' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` sharedDocumentVersion

instance Prelude.NFData AccountSharingInfo where
  rnf AccountSharingInfo' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf sharedDocumentVersion
