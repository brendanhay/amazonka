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
-- Module      : Network.AWS.SSM.Types.AccountSharingInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AccountSharingInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information includes the Amazon Web Services account ID where the
-- current document is shared and the version shared with that account.
--
-- /See:/ 'newAccountSharingInfo' smart constructor.
data AccountSharingInfo = AccountSharingInfo'
  { -- | The version of the current document shared with the account.
    sharedDocumentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID where the current document is shared.
    accountId :: Prelude.Maybe Prelude.Text
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
-- 'sharedDocumentVersion', 'accountSharingInfo_sharedDocumentVersion' - The version of the current document shared with the account.
--
-- 'accountId', 'accountSharingInfo_accountId' - The Amazon Web Services account ID where the current document is shared.
newAccountSharingInfo ::
  AccountSharingInfo
newAccountSharingInfo =
  AccountSharingInfo'
    { sharedDocumentVersion =
        Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The version of the current document shared with the account.
accountSharingInfo_sharedDocumentVersion :: Lens.Lens' AccountSharingInfo (Prelude.Maybe Prelude.Text)
accountSharingInfo_sharedDocumentVersion = Lens.lens (\AccountSharingInfo' {sharedDocumentVersion} -> sharedDocumentVersion) (\s@AccountSharingInfo' {} a -> s {sharedDocumentVersion = a} :: AccountSharingInfo)

-- | The Amazon Web Services account ID where the current document is shared.
accountSharingInfo_accountId :: Lens.Lens' AccountSharingInfo (Prelude.Maybe Prelude.Text)
accountSharingInfo_accountId = Lens.lens (\AccountSharingInfo' {accountId} -> accountId) (\s@AccountSharingInfo' {} a -> s {accountId = a} :: AccountSharingInfo)

instance Core.FromJSON AccountSharingInfo where
  parseJSON =
    Core.withObject
      "AccountSharingInfo"
      ( \x ->
          AccountSharingInfo'
            Prelude.<$> (x Core..:? "SharedDocumentVersion")
            Prelude.<*> (x Core..:? "AccountId")
      )

instance Prelude.Hashable AccountSharingInfo

instance Prelude.NFData AccountSharingInfo
