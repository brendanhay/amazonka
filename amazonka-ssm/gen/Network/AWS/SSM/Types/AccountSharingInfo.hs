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
-- Module      : Network.AWS.SSM.Types.AccountSharingInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AccountSharingInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information includes the AWS account ID where the current document is
-- shared and the version shared with that account.
--
-- /See:/ 'newAccountSharingInfo' smart constructor.
data AccountSharingInfo = AccountSharingInfo'
  { -- | The AWS account ID where the current document is shared.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The version of the current document shared with the account.
    sharedDocumentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountSharingInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountSharingInfo_accountId' - The AWS account ID where the current document is shared.
--
-- 'sharedDocumentVersion', 'accountSharingInfo_sharedDocumentVersion' - The version of the current document shared with the account.
newAccountSharingInfo ::
  AccountSharingInfo
newAccountSharingInfo =
  AccountSharingInfo'
    { accountId = Prelude.Nothing,
      sharedDocumentVersion = Prelude.Nothing
    }

-- | The AWS account ID where the current document is shared.
accountSharingInfo_accountId :: Lens.Lens' AccountSharingInfo (Prelude.Maybe Prelude.Text)
accountSharingInfo_accountId = Lens.lens (\AccountSharingInfo' {accountId} -> accountId) (\s@AccountSharingInfo' {} a -> s {accountId = a} :: AccountSharingInfo)

-- | The version of the current document shared with the account.
accountSharingInfo_sharedDocumentVersion :: Lens.Lens' AccountSharingInfo (Prelude.Maybe Prelude.Text)
accountSharingInfo_sharedDocumentVersion = Lens.lens (\AccountSharingInfo' {sharedDocumentVersion} -> sharedDocumentVersion) (\s@AccountSharingInfo' {} a -> s {sharedDocumentVersion = a} :: AccountSharingInfo)

instance Prelude.FromJSON AccountSharingInfo where
  parseJSON =
    Prelude.withObject
      "AccountSharingInfo"
      ( \x ->
          AccountSharingInfo'
            Prelude.<$> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "SharedDocumentVersion")
      )

instance Prelude.Hashable AccountSharingInfo

instance Prelude.NFData AccountSharingInfo
