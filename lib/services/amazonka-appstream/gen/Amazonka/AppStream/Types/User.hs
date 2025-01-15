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
-- Module      : Amazonka.AppStream.Types.User
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.User where

import Amazonka.AppStream.Types.AuthenticationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a user in the user pool.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The ARN of the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the user was created in the user pool.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the user in the user pool is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The first name, or given name, of the user.
    firstName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The last name, or surname, of the user.
    lastName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The status of the user in the user pool. The status can be one of the
    -- following:
    --
    -- -   UNCONFIRMED – The user is created but not confirmed.
    --
    -- -   CONFIRMED – The user is confirmed.
    --
    -- -   ARCHIVED – The user is no longer active.
    --
    -- -   COMPROMISED – The user is disabled because of a potential security
    --     threat.
    --
    -- -   UNKNOWN – The user status is not known.
    status :: Prelude.Maybe Prelude.Text,
    -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The authentication type for the user.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'user_arn' - The ARN of the user.
--
-- 'createdTime', 'user_createdTime' - The date and time the user was created in the user pool.
--
-- 'enabled', 'user_enabled' - Specifies whether the user in the user pool is enabled.
--
-- 'firstName', 'user_firstName' - The first name, or given name, of the user.
--
-- 'lastName', 'user_lastName' - The last name, or surname, of the user.
--
-- 'status', 'user_status' - The status of the user in the user pool. The status can be one of the
-- following:
--
-- -   UNCONFIRMED – The user is created but not confirmed.
--
-- -   CONFIRMED – The user is confirmed.
--
-- -   ARCHIVED – The user is no longer active.
--
-- -   COMPROMISED – The user is disabled because of a potential security
--     threat.
--
-- -   UNKNOWN – The user status is not known.
--
-- 'userName', 'user_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive.
--
-- 'authenticationType', 'user_authenticationType' - The authentication type for the user.
newUser ::
  -- | 'authenticationType'
  AuthenticationType ->
  User
newUser pAuthenticationType_ =
  User'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      enabled = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      status = Prelude.Nothing,
      userName = Prelude.Nothing,
      authenticationType = pAuthenticationType_
    }

-- | The ARN of the user.
user_arn :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | The date and time the user was created in the user pool.
user_createdTime :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_createdTime = Lens.lens (\User' {createdTime} -> createdTime) (\s@User' {} a -> s {createdTime = a} :: User) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the user in the user pool is enabled.
user_enabled :: Lens.Lens' User (Prelude.Maybe Prelude.Bool)
user_enabled = Lens.lens (\User' {enabled} -> enabled) (\s@User' {} a -> s {enabled = a} :: User)

-- | The first name, or given name, of the user.
user_firstName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_firstName = Lens.lens (\User' {firstName} -> firstName) (\s@User' {} a -> s {firstName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The last name, or surname, of the user.
user_lastName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_lastName = Lens.lens (\User' {lastName} -> lastName) (\s@User' {} a -> s {lastName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The status of the user in the user pool. The status can be one of the
-- following:
--
-- -   UNCONFIRMED – The user is created but not confirmed.
--
-- -   CONFIRMED – The user is confirmed.
--
-- -   ARCHIVED – The user is no longer active.
--
-- -   COMPROMISED – The user is disabled because of a potential security
--     threat.
--
-- -   UNKNOWN – The user status is not known.
user_status :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive.
user_userName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The authentication type for the user.
user_authenticationType :: Lens.Lens' User AuthenticationType
user_authenticationType = Lens.lens (\User' {authenticationType} -> authenticationType) (\s@User' {} a -> s {authenticationType = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserName")
            Prelude.<*> (x Data..: "AuthenticationType")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf enabled `Prelude.seq`
          Prelude.rnf firstName `Prelude.seq`
            Prelude.rnf lastName `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf userName `Prelude.seq`
                  Prelude.rnf authenticationType
