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
-- Module      : Amazonka.WorkMail.Types.User
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.EntityState
import Amazonka.WorkMail.Types.UserRole

-- | The representation of an WorkMail user.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The date indicating when the user was disabled from WorkMail use.
    disabledDate :: Prelude.Maybe Data.POSIX,
    -- | The display name of the user.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The email of the user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the user was enabled for WorkMail use.
    enabledDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the user.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the user.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The role of the user.
    userRole :: Prelude.Maybe UserRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabledDate', 'user_disabledDate' - The date indicating when the user was disabled from WorkMail use.
--
-- 'displayName', 'user_displayName' - The display name of the user.
--
-- 'email', 'user_email' - The email of the user.
--
-- 'enabledDate', 'user_enabledDate' - The date indicating when the user was enabled for WorkMail use.
--
-- 'id', 'user_id' - The identifier of the user.
--
-- 'name', 'user_name' - The name of the user.
--
-- 'state', 'user_state' - The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- 'userRole', 'user_userRole' - The role of the user.
newUser ::
  User
newUser =
  User'
    { disabledDate = Prelude.Nothing,
      displayName = Prelude.Nothing,
      email = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      userRole = Prelude.Nothing
    }

-- | The date indicating when the user was disabled from WorkMail use.
user_disabledDate :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_disabledDate = Lens.lens (\User' {disabledDate} -> disabledDate) (\s@User' {} a -> s {disabledDate = a} :: User) Prelude.. Lens.mapping Data._Time

-- | The display name of the user.
user_displayName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_displayName = Lens.lens (\User' {displayName} -> displayName) (\s@User' {} a -> s {displayName = a} :: User)

-- | The email of the user.
user_email :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_email = Lens.lens (\User' {email} -> email) (\s@User' {} a -> s {email = a} :: User)

-- | The date indicating when the user was enabled for WorkMail use.
user_enabledDate :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_enabledDate = Lens.lens (\User' {enabledDate} -> enabledDate) (\s@User' {} a -> s {enabledDate = a} :: User) Prelude.. Lens.mapping Data._Time

-- | The identifier of the user.
user_id :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_id = Lens.lens (\User' {id} -> id) (\s@User' {} a -> s {id = a} :: User)

-- | The name of the user.
user_name :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_name = Lens.lens (\User' {name} -> name) (\s@User' {} a -> s {name = a} :: User)

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
user_state :: Lens.Lens' User (Prelude.Maybe EntityState)
user_state = Lens.lens (\User' {state} -> state) (\s@User' {} a -> s {state = a} :: User)

-- | The role of the user.
user_userRole :: Lens.Lens' User (Prelude.Maybe UserRole)
user_userRole = Lens.lens (\User' {userRole} -> userRole) (\s@User' {} a -> s {userRole = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "DisabledDate")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "EnabledDate")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "UserRole")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt
      `Prelude.hashWithSalt` disabledDate
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` enabledDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` userRole

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf disabledDate
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf userRole
