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
-- Module      : Network.AWS.AppStream.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.User where

import Network.AWS.AppStream.Types.AuthenticationType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a user in the user pool.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The status of the user in the user pool. The status can be one of the
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
    status :: Core.Maybe Core.Text,
    -- | The ARN of the user.
    arn :: Core.Maybe Core.Text,
    -- | Specifies whether the user in the user pool is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The date and time the user was created in the user pool.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The first name, or given name, of the user.
    firstName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The last name, or surname, of the user.
    lastName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The authentication type for the user.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'arn', 'user_arn' - The ARN of the user.
--
-- 'enabled', 'user_enabled' - Specifies whether the user in the user pool is enabled.
--
-- 'createdTime', 'user_createdTime' - The date and time the user was created in the user pool.
--
-- 'userName', 'user_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive.
--
-- 'firstName', 'user_firstName' - The first name, or given name, of the user.
--
-- 'lastName', 'user_lastName' - The last name, or surname, of the user.
--
-- 'authenticationType', 'user_authenticationType' - The authentication type for the user.
newUser ::
  -- | 'authenticationType'
  AuthenticationType ->
  User
newUser pAuthenticationType_ =
  User'
    { status = Core.Nothing,
      arn = Core.Nothing,
      enabled = Core.Nothing,
      createdTime = Core.Nothing,
      userName = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      authenticationType = pAuthenticationType_
    }

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
user_status :: Lens.Lens' User (Core.Maybe Core.Text)
user_status = Lens.lens (\User' {status} -> status) (\s@User' {} a -> s {status = a} :: User)

-- | The ARN of the user.
user_arn :: Lens.Lens' User (Core.Maybe Core.Text)
user_arn = Lens.lens (\User' {arn} -> arn) (\s@User' {} a -> s {arn = a} :: User)

-- | Specifies whether the user in the user pool is enabled.
user_enabled :: Lens.Lens' User (Core.Maybe Core.Bool)
user_enabled = Lens.lens (\User' {enabled} -> enabled) (\s@User' {} a -> s {enabled = a} :: User)

-- | The date and time the user was created in the user pool.
user_createdTime :: Lens.Lens' User (Core.Maybe Core.UTCTime)
user_createdTime = Lens.lens (\User' {createdTime} -> createdTime) (\s@User' {} a -> s {createdTime = a} :: User) Core.. Lens.mapping Core._Time

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive.
user_userName :: Lens.Lens' User (Core.Maybe Core.Text)
user_userName = Lens.lens (\User' {userName} -> userName) (\s@User' {} a -> s {userName = a} :: User) Core.. Lens.mapping Core._Sensitive

-- | The first name, or given name, of the user.
user_firstName :: Lens.Lens' User (Core.Maybe Core.Text)
user_firstName = Lens.lens (\User' {firstName} -> firstName) (\s@User' {} a -> s {firstName = a} :: User) Core.. Lens.mapping Core._Sensitive

-- | The last name, or surname, of the user.
user_lastName :: Lens.Lens' User (Core.Maybe Core.Text)
user_lastName = Lens.lens (\User' {lastName} -> lastName) (\s@User' {} a -> s {lastName = a} :: User) Core.. Lens.mapping Core._Sensitive

-- | The authentication type for the user.
user_authenticationType :: Lens.Lens' User AuthenticationType
user_authenticationType = Lens.lens (\User' {authenticationType} -> authenticationType) (\s@User' {} a -> s {authenticationType = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "UserName")
            Core.<*> (x Core..:? "FirstName")
            Core.<*> (x Core..:? "LastName")
            Core.<*> (x Core..: "AuthenticationType")
      )

instance Core.Hashable User

instance Core.NFData User
