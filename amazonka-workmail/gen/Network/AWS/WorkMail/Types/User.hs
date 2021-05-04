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
-- Module      : Network.AWS.WorkMail.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.User where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.UserRole

-- | The representation of an Amazon WorkMail user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The date indicating when the user was enabled for Amazon WorkMail use.
    enabledDate :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The role of the user.
    userRole :: Prelude.Maybe UserRole,
    -- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The name of the user.
    name :: Prelude.Maybe Prelude.Text,
    -- | The email of the user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the user was disabled from Amazon WorkMail use.
    disabledDate :: Prelude.Maybe Prelude.POSIX,
    -- | The display name of the user.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledDate', 'user_enabledDate' - The date indicating when the user was enabled for Amazon WorkMail use.
--
-- 'id', 'user_id' - The identifier of the user.
--
-- 'userRole', 'user_userRole' - The role of the user.
--
-- 'state', 'user_state' - The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- 'name', 'user_name' - The name of the user.
--
-- 'email', 'user_email' - The email of the user.
--
-- 'disabledDate', 'user_disabledDate' - The date indicating when the user was disabled from Amazon WorkMail use.
--
-- 'displayName', 'user_displayName' - The display name of the user.
newUser ::
  User
newUser =
  User'
    { enabledDate = Prelude.Nothing,
      id = Prelude.Nothing,
      userRole = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      email = Prelude.Nothing,
      disabledDate = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The date indicating when the user was enabled for Amazon WorkMail use.
user_enabledDate :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_enabledDate = Lens.lens (\User' {enabledDate} -> enabledDate) (\s@User' {} a -> s {enabledDate = a} :: User) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the user.
user_id :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The role of the user.
user_userRole :: Lens.Lens' User (Prelude.Maybe UserRole)
user_userRole = Lens.lens (\User' {userRole} -> userRole) (\s@User' {} a -> s {userRole = a} :: User)

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
user_state :: Lens.Lens' User (Prelude.Maybe EntityState)
user_state = Lens.lens (\User' {state} -> state) (\s@User' {} a -> s {state = a} :: User)

-- | The name of the user.
user_name :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_name = Lens.lens (\User' {name} -> name) (\s@User' {} a -> s {name = a} :: User)

-- | The email of the user.
user_email :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_email = Lens.lens (\User' {email} -> email) (\s@User' {} a -> s {email = a} :: User)

-- | The date indicating when the user was disabled from Amazon WorkMail use.
user_disabledDate :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_disabledDate = Lens.lens (\User' {disabledDate} -> disabledDate) (\s@User' {} a -> s {disabledDate = a} :: User) Prelude.. Lens.mapping Prelude._Time

-- | The display name of the user.
user_displayName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_displayName = Lens.lens (\User' {displayName} -> displayName) (\s@User' {} a -> s {displayName = a} :: User)

instance Prelude.FromJSON User where
  parseJSON =
    Prelude.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Prelude..:? "EnabledDate")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "UserRole")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Email")
            Prelude.<*> (x Prelude..:? "DisabledDate")
            Prelude.<*> (x Prelude..:? "DisplayName")
      )

instance Prelude.Hashable User

instance Prelude.NFData User
