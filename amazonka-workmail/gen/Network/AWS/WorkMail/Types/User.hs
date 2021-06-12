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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.UserRole

-- | The representation of an Amazon WorkMail user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The date indicating when the user was enabled for Amazon WorkMail use.
    enabledDate :: Core.Maybe Core.POSIX,
    -- | The identifier of the user.
    id :: Core.Maybe Core.Text,
    -- | The role of the user.
    userRole :: Core.Maybe UserRole,
    -- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
    state :: Core.Maybe EntityState,
    -- | The name of the user.
    name :: Core.Maybe Core.Text,
    -- | The email of the user.
    email :: Core.Maybe Core.Text,
    -- | The date indicating when the user was disabled from Amazon WorkMail use.
    disabledDate :: Core.Maybe Core.POSIX,
    -- | The display name of the user.
    displayName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { enabledDate = Core.Nothing,
      id = Core.Nothing,
      userRole = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      email = Core.Nothing,
      disabledDate = Core.Nothing,
      displayName = Core.Nothing
    }

-- | The date indicating when the user was enabled for Amazon WorkMail use.
user_enabledDate :: Lens.Lens' User (Core.Maybe Core.UTCTime)
user_enabledDate = Lens.lens (\User' {enabledDate} -> enabledDate) (\s@User' {} a -> s {enabledDate = a} :: User) Core.. Lens.mapping Core._Time

-- | The identifier of the user.
user_id :: Lens.Lens' User (Core.Maybe Core.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The role of the user.
user_userRole :: Lens.Lens' User (Core.Maybe UserRole)
user_userRole = Lens.lens (\User' {userRole} -> userRole) (\s@User' {} a -> s {userRole = a} :: User)

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
user_state :: Lens.Lens' User (Core.Maybe EntityState)
user_state = Lens.lens (\User' {state} -> state) (\s@User' {} a -> s {state = a} :: User)

-- | The name of the user.
user_name :: Lens.Lens' User (Core.Maybe Core.Text)
user_name = Lens.lens (\User' {name} -> name) (\s@User' {} a -> s {name = a} :: User)

-- | The email of the user.
user_email :: Lens.Lens' User (Core.Maybe Core.Text)
user_email = Lens.lens (\User' {email} -> email) (\s@User' {} a -> s {email = a} :: User)

-- | The date indicating when the user was disabled from Amazon WorkMail use.
user_disabledDate :: Lens.Lens' User (Core.Maybe Core.UTCTime)
user_disabledDate = Lens.lens (\User' {disabledDate} -> disabledDate) (\s@User' {} a -> s {disabledDate = a} :: User) Core.. Lens.mapping Core._Time

-- | The display name of the user.
user_displayName :: Lens.Lens' User (Core.Maybe Core.Text)
user_displayName = Lens.lens (\User' {displayName} -> displayName) (\s@User' {} a -> s {displayName = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Core.<$> (x Core..:? "EnabledDate")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "UserRole")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Email")
            Core.<*> (x Core..:? "DisabledDate")
            Core.<*> (x Core..:? "DisplayName")
      )

instance Core.Hashable User

instance Core.NFData User
