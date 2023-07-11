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
-- Module      : Amazonka.QuickSight.Types.AccountInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AccountInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Edition

-- | A structure that contains the following account information elements:
--
-- -   Your Amazon QuickSight account name.
--
-- -   The edition of Amazon QuickSight that your account is using.
--
-- -   The notification email address that is associated with the Amazon
--     QuickSight account.
--
-- -   The authentication type of the Amazon QuickSight account.
--
-- -   The status of the Amazon QuickSight account\'s subscription.
--
-- /See:/ 'newAccountInfo' smart constructor.
data AccountInfo = AccountInfo'
  { -- | The account name that you provided for the Amazon QuickSight
    -- subscription in your Amazon Web Services account. You create this name
    -- when you sign up for Amazon QuickSight. It\'s unique over all of Amazon
    -- Web Services, and it appears only when users sign in.
    accountName :: Prelude.Maybe Prelude.Text,
    -- | The status of your account subscription.
    accountSubscriptionStatus :: Prelude.Maybe Prelude.Text,
    -- | The way that your Amazon QuickSight account is authenticated.
    authenticationType :: Prelude.Maybe Prelude.Text,
    -- | The edition of your Amazon QuickSight account.
    edition :: Prelude.Maybe Edition,
    -- | The email address that will be used for Amazon QuickSight to send
    -- notifications regarding your Amazon Web Services account or Amazon
    -- QuickSight subscription.
    notificationEmail :: Prelude.Maybe Prelude.Text
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
-- 'accountName', 'accountInfo_accountName' - The account name that you provided for the Amazon QuickSight
-- subscription in your Amazon Web Services account. You create this name
-- when you sign up for Amazon QuickSight. It\'s unique over all of Amazon
-- Web Services, and it appears only when users sign in.
--
-- 'accountSubscriptionStatus', 'accountInfo_accountSubscriptionStatus' - The status of your account subscription.
--
-- 'authenticationType', 'accountInfo_authenticationType' - The way that your Amazon QuickSight account is authenticated.
--
-- 'edition', 'accountInfo_edition' - The edition of your Amazon QuickSight account.
--
-- 'notificationEmail', 'accountInfo_notificationEmail' - The email address that will be used for Amazon QuickSight to send
-- notifications regarding your Amazon Web Services account or Amazon
-- QuickSight subscription.
newAccountInfo ::
  AccountInfo
newAccountInfo =
  AccountInfo'
    { accountName = Prelude.Nothing,
      accountSubscriptionStatus = Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      edition = Prelude.Nothing,
      notificationEmail = Prelude.Nothing
    }

-- | The account name that you provided for the Amazon QuickSight
-- subscription in your Amazon Web Services account. You create this name
-- when you sign up for Amazon QuickSight. It\'s unique over all of Amazon
-- Web Services, and it appears only when users sign in.
accountInfo_accountName :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_accountName = Lens.lens (\AccountInfo' {accountName} -> accountName) (\s@AccountInfo' {} a -> s {accountName = a} :: AccountInfo)

-- | The status of your account subscription.
accountInfo_accountSubscriptionStatus :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_accountSubscriptionStatus = Lens.lens (\AccountInfo' {accountSubscriptionStatus} -> accountSubscriptionStatus) (\s@AccountInfo' {} a -> s {accountSubscriptionStatus = a} :: AccountInfo)

-- | The way that your Amazon QuickSight account is authenticated.
accountInfo_authenticationType :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_authenticationType = Lens.lens (\AccountInfo' {authenticationType} -> authenticationType) (\s@AccountInfo' {} a -> s {authenticationType = a} :: AccountInfo)

-- | The edition of your Amazon QuickSight account.
accountInfo_edition :: Lens.Lens' AccountInfo (Prelude.Maybe Edition)
accountInfo_edition = Lens.lens (\AccountInfo' {edition} -> edition) (\s@AccountInfo' {} a -> s {edition = a} :: AccountInfo)

-- | The email address that will be used for Amazon QuickSight to send
-- notifications regarding your Amazon Web Services account or Amazon
-- QuickSight subscription.
accountInfo_notificationEmail :: Lens.Lens' AccountInfo (Prelude.Maybe Prelude.Text)
accountInfo_notificationEmail = Lens.lens (\AccountInfo' {notificationEmail} -> notificationEmail) (\s@AccountInfo' {} a -> s {notificationEmail = a} :: AccountInfo)

instance Data.FromJSON AccountInfo where
  parseJSON =
    Data.withObject
      "AccountInfo"
      ( \x ->
          AccountInfo'
            Prelude.<$> (x Data..:? "AccountName")
            Prelude.<*> (x Data..:? "AccountSubscriptionStatus")
            Prelude.<*> (x Data..:? "AuthenticationType")
            Prelude.<*> (x Data..:? "Edition")
            Prelude.<*> (x Data..:? "NotificationEmail")
      )

instance Prelude.Hashable AccountInfo where
  hashWithSalt _salt AccountInfo' {..} =
    _salt
      `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` accountSubscriptionStatus
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` notificationEmail

instance Prelude.NFData AccountInfo where
  rnf AccountInfo' {..} =
    Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf accountSubscriptionStatus
      `Prelude.seq` Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf notificationEmail
