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
-- Module      : Amazonka.QuickSight.Types.AccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Edition

-- | The Amazon QuickSight settings associated with your Amazon Web Services
-- account.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | The \"account name\" you provided for the Amazon QuickSight subscription
    -- in your Amazon Web Services account. You create this name when you sign
    -- up for Amazon QuickSight. It is unique in all of Amazon Web Services and
    -- it appears only when users sign in.
    accountName :: Prelude.Maybe Prelude.Text,
    -- | The default Amazon QuickSight namespace for your Amazon Web Services
    -- account.
    defaultNamespace :: Prelude.Maybe Prelude.Text,
    -- | The edition of Amazon QuickSight that you\'re currently subscribed to:
    -- Enterprise edition or Standard edition.
    edition :: Prelude.Maybe Edition,
    -- | The main notification email for your Amazon QuickSight subscription.
    notificationEmail :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that indicates whether public sharing is turned on for
    -- an Amazon QuickSight account. For more information about turning on
    -- public sharing, see
    -- <https://docs.aws.amazon.com/quicksight/latest/APIReference/API_UpdatePublicSharingSettings.html UpdatePublicSharingSettings>.
    publicSharingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value that determines whether or not an Amazon QuickSight
    -- account can be deleted. A @True@ value doesn\'t allow the account to be
    -- deleted and results in an error message if a user tries to make a
    -- @DeleteAccountSubsctiption@ request. A @False@ value will allow the
    -- ccount to be deleted.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountName', 'accountSettings_accountName' - The \"account name\" you provided for the Amazon QuickSight subscription
-- in your Amazon Web Services account. You create this name when you sign
-- up for Amazon QuickSight. It is unique in all of Amazon Web Services and
-- it appears only when users sign in.
--
-- 'defaultNamespace', 'accountSettings_defaultNamespace' - The default Amazon QuickSight namespace for your Amazon Web Services
-- account.
--
-- 'edition', 'accountSettings_edition' - The edition of Amazon QuickSight that you\'re currently subscribed to:
-- Enterprise edition or Standard edition.
--
-- 'notificationEmail', 'accountSettings_notificationEmail' - The main notification email for your Amazon QuickSight subscription.
--
-- 'publicSharingEnabled', 'accountSettings_publicSharingEnabled' - A Boolean value that indicates whether public sharing is turned on for
-- an Amazon QuickSight account. For more information about turning on
-- public sharing, see
-- <https://docs.aws.amazon.com/quicksight/latest/APIReference/API_UpdatePublicSharingSettings.html UpdatePublicSharingSettings>.
--
-- 'terminationProtectionEnabled', 'accountSettings_terminationProtectionEnabled' - A boolean value that determines whether or not an Amazon QuickSight
-- account can be deleted. A @True@ value doesn\'t allow the account to be
-- deleted and results in an error message if a user tries to make a
-- @DeleteAccountSubsctiption@ request. A @False@ value will allow the
-- ccount to be deleted.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { accountName = Prelude.Nothing,
      defaultNamespace = Prelude.Nothing,
      edition = Prelude.Nothing,
      notificationEmail = Prelude.Nothing,
      publicSharingEnabled = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing
    }

-- | The \"account name\" you provided for the Amazon QuickSight subscription
-- in your Amazon Web Services account. You create this name when you sign
-- up for Amazon QuickSight. It is unique in all of Amazon Web Services and
-- it appears only when users sign in.
accountSettings_accountName :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_accountName = Lens.lens (\AccountSettings' {accountName} -> accountName) (\s@AccountSettings' {} a -> s {accountName = a} :: AccountSettings)

-- | The default Amazon QuickSight namespace for your Amazon Web Services
-- account.
accountSettings_defaultNamespace :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_defaultNamespace = Lens.lens (\AccountSettings' {defaultNamespace} -> defaultNamespace) (\s@AccountSettings' {} a -> s {defaultNamespace = a} :: AccountSettings)

-- | The edition of Amazon QuickSight that you\'re currently subscribed to:
-- Enterprise edition or Standard edition.
accountSettings_edition :: Lens.Lens' AccountSettings (Prelude.Maybe Edition)
accountSettings_edition = Lens.lens (\AccountSettings' {edition} -> edition) (\s@AccountSettings' {} a -> s {edition = a} :: AccountSettings)

-- | The main notification email for your Amazon QuickSight subscription.
accountSettings_notificationEmail :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_notificationEmail = Lens.lens (\AccountSettings' {notificationEmail} -> notificationEmail) (\s@AccountSettings' {} a -> s {notificationEmail = a} :: AccountSettings)

-- | A Boolean value that indicates whether public sharing is turned on for
-- an Amazon QuickSight account. For more information about turning on
-- public sharing, see
-- <https://docs.aws.amazon.com/quicksight/latest/APIReference/API_UpdatePublicSharingSettings.html UpdatePublicSharingSettings>.
accountSettings_publicSharingEnabled :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_publicSharingEnabled = Lens.lens (\AccountSettings' {publicSharingEnabled} -> publicSharingEnabled) (\s@AccountSettings' {} a -> s {publicSharingEnabled = a} :: AccountSettings)

-- | A boolean value that determines whether or not an Amazon QuickSight
-- account can be deleted. A @True@ value doesn\'t allow the account to be
-- deleted and results in an error message if a user tries to make a
-- @DeleteAccountSubsctiption@ request. A @False@ value will allow the
-- ccount to be deleted.
accountSettings_terminationProtectionEnabled :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_terminationProtectionEnabled = Lens.lens (\AccountSettings' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@AccountSettings' {} a -> s {terminationProtectionEnabled = a} :: AccountSettings)

instance Data.FromJSON AccountSettings where
  parseJSON =
    Data.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Data..:? "AccountName")
            Prelude.<*> (x Data..:? "DefaultNamespace")
            Prelude.<*> (x Data..:? "Edition")
            Prelude.<*> (x Data..:? "NotificationEmail")
            Prelude.<*> (x Data..:? "PublicSharingEnabled")
            Prelude.<*> (x Data..:? "TerminationProtectionEnabled")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` defaultNamespace
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` notificationEmail
      `Prelude.hashWithSalt` publicSharingEnabled
      `Prelude.hashWithSalt` terminationProtectionEnabled

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf defaultNamespace
      `Prelude.seq` Prelude.rnf edition
      `Prelude.seq` Prelude.rnf notificationEmail
      `Prelude.seq` Prelude.rnf publicSharingEnabled
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
