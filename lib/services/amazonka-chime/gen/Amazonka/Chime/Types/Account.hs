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
-- Module      : Amazonka.Chime.Types.Account
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Account where

import Amazonka.Chime.Types.AccountStatus
import Amazonka.Chime.Types.AccountType
import Amazonka.Chime.Types.License
import Amazonka.Chime.Types.SigninDelegateGroup
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime account details. An AWS account can have multiple
-- Amazon Chime accounts.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The status of the account.
    accountStatus :: Prelude.Maybe AccountStatus,
    -- | The Amazon Chime account type. For more information about different
    -- account types, see
    -- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
    -- in the /Amazon Chime Administration Guide/.
    accountType :: Prelude.Maybe AccountType,
    -- | The Amazon Chime account creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The default license for the Amazon Chime account.
    defaultLicense :: Prelude.Maybe License,
    -- | The sign-in delegate groups associated with the account.
    signinDelegateGroups :: Prelude.Maybe [SigninDelegateGroup],
    -- | Supported licenses for the Amazon Chime account.
    supportedLicenses :: Prelude.Maybe [License],
    -- | The AWS account ID.
    awsAccountId :: Prelude.Text,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The Amazon Chime account name.
    name :: Prelude.Text
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
-- 'accountStatus', 'account_accountStatus' - The status of the account.
--
-- 'accountType', 'account_accountType' - The Amazon Chime account type. For more information about different
-- account types, see
-- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
-- in the /Amazon Chime Administration Guide/.
--
-- 'createdTimestamp', 'account_createdTimestamp' - The Amazon Chime account creation timestamp, in ISO 8601 format.
--
-- 'defaultLicense', 'account_defaultLicense' - The default license for the Amazon Chime account.
--
-- 'signinDelegateGroups', 'account_signinDelegateGroups' - The sign-in delegate groups associated with the account.
--
-- 'supportedLicenses', 'account_supportedLicenses' - Supported licenses for the Amazon Chime account.
--
-- 'awsAccountId', 'account_awsAccountId' - The AWS account ID.
--
-- 'accountId', 'account_accountId' - The Amazon Chime account ID.
--
-- 'name', 'account_name' - The Amazon Chime account name.
newAccount ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Account
newAccount pAwsAccountId_ pAccountId_ pName_ =
  Account'
    { accountStatus = Prelude.Nothing,
      accountType = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      defaultLicense = Prelude.Nothing,
      signinDelegateGroups = Prelude.Nothing,
      supportedLicenses = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      accountId = pAccountId_,
      name = pName_
    }

-- | The status of the account.
account_accountStatus :: Lens.Lens' Account (Prelude.Maybe AccountStatus)
account_accountStatus = Lens.lens (\Account' {accountStatus} -> accountStatus) (\s@Account' {} a -> s {accountStatus = a} :: Account)

-- | The Amazon Chime account type. For more information about different
-- account types, see
-- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
-- in the /Amazon Chime Administration Guide/.
account_accountType :: Lens.Lens' Account (Prelude.Maybe AccountType)
account_accountType = Lens.lens (\Account' {accountType} -> accountType) (\s@Account' {} a -> s {accountType = a} :: Account)

-- | The Amazon Chime account creation timestamp, in ISO 8601 format.
account_createdTimestamp :: Lens.Lens' Account (Prelude.Maybe Prelude.UTCTime)
account_createdTimestamp = Lens.lens (\Account' {createdTimestamp} -> createdTimestamp) (\s@Account' {} a -> s {createdTimestamp = a} :: Account) Prelude.. Lens.mapping Data._Time

-- | The default license for the Amazon Chime account.
account_defaultLicense :: Lens.Lens' Account (Prelude.Maybe License)
account_defaultLicense = Lens.lens (\Account' {defaultLicense} -> defaultLicense) (\s@Account' {} a -> s {defaultLicense = a} :: Account)

-- | The sign-in delegate groups associated with the account.
account_signinDelegateGroups :: Lens.Lens' Account (Prelude.Maybe [SigninDelegateGroup])
account_signinDelegateGroups = Lens.lens (\Account' {signinDelegateGroups} -> signinDelegateGroups) (\s@Account' {} a -> s {signinDelegateGroups = a} :: Account) Prelude.. Lens.mapping Lens.coerced

-- | Supported licenses for the Amazon Chime account.
account_supportedLicenses :: Lens.Lens' Account (Prelude.Maybe [License])
account_supportedLicenses = Lens.lens (\Account' {supportedLicenses} -> supportedLicenses) (\s@Account' {} a -> s {supportedLicenses = a} :: Account) Prelude.. Lens.mapping Lens.coerced

-- | The AWS account ID.
account_awsAccountId :: Lens.Lens' Account Prelude.Text
account_awsAccountId = Lens.lens (\Account' {awsAccountId} -> awsAccountId) (\s@Account' {} a -> s {awsAccountId = a} :: Account)

-- | The Amazon Chime account ID.
account_accountId :: Lens.Lens' Account Prelude.Text
account_accountId = Lens.lens (\Account' {accountId} -> accountId) (\s@Account' {} a -> s {accountId = a} :: Account)

-- | The Amazon Chime account name.
account_name :: Lens.Lens' Account Prelude.Text
account_name = Lens.lens (\Account' {name} -> name) (\s@Account' {} a -> s {name = a} :: Account)

instance Data.FromJSON Account where
  parseJSON =
    Data.withObject
      "Account"
      ( \x ->
          Account'
            Prelude.<$> (x Data..:? "AccountStatus")
            Prelude.<*> (x Data..:? "AccountType")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "DefaultLicense")
            Prelude.<*> ( x
                            Data..:? "SigninDelegateGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SupportedLicenses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "AwsAccountId")
            Prelude.<*> (x Data..: "AccountId")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Account where
  hashWithSalt _salt Account' {..} =
    _salt
      `Prelude.hashWithSalt` accountStatus
      `Prelude.hashWithSalt` accountType
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` defaultLicense
      `Prelude.hashWithSalt` signinDelegateGroups
      `Prelude.hashWithSalt` supportedLicenses
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` name

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf accountStatus `Prelude.seq`
      Prelude.rnf accountType `Prelude.seq`
        Prelude.rnf createdTimestamp `Prelude.seq`
          Prelude.rnf defaultLicense `Prelude.seq`
            Prelude.rnf signinDelegateGroups `Prelude.seq`
              Prelude.rnf supportedLicenses `Prelude.seq`
                Prelude.rnf awsAccountId `Prelude.seq`
                  Prelude.rnf accountId `Prelude.seq`
                    Prelude.rnf name
