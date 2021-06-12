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
-- Module      : Network.AWS.MQ.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.User where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A user associated with the broker.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Core.Maybe [Core.Text],
    -- | Required. The password of the broker user. This value must be at least
    -- 12 characters long, must contain at least 4 unique characters, and must
    -- not contain commas.
    password :: Core.Maybe Core.Text,
    -- | Required. The username of the broker user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Core.Maybe Core.Text,
    -- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does
    -- not apply to RabbitMQ brokers).
    consoleAccess :: Core.Maybe Core.Bool
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
-- 'groups', 'user_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'password', 'user_password' - Required. The password of the broker user. This value must be at least
-- 12 characters long, must contain at least 4 unique characters, and must
-- not contain commas.
--
-- 'username', 'user_username' - Required. The username of the broker user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
--
-- 'consoleAccess', 'user_consoleAccess' - Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does
-- not apply to RabbitMQ brokers).
newUser ::
  User
newUser =
  User'
    { groups = Core.Nothing,
      password = Core.Nothing,
      username = Core.Nothing,
      consoleAccess = Core.Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
user_groups :: Lens.Lens' User (Core.Maybe [Core.Text])
user_groups = Lens.lens (\User' {groups} -> groups) (\s@User' {} a -> s {groups = a} :: User) Core.. Lens.mapping Lens._Coerce

-- | Required. The password of the broker user. This value must be at least
-- 12 characters long, must contain at least 4 unique characters, and must
-- not contain commas.
user_password :: Lens.Lens' User (Core.Maybe Core.Text)
user_password = Lens.lens (\User' {password} -> password) (\s@User' {} a -> s {password = a} :: User)

-- | Required. The username of the broker user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
user_username :: Lens.Lens' User (Core.Maybe Core.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does
-- not apply to RabbitMQ brokers).
user_consoleAccess :: Lens.Lens' User (Core.Maybe Core.Bool)
user_consoleAccess = Lens.lens (\User' {consoleAccess} -> consoleAccess) (\s@User' {} a -> s {consoleAccess = a} :: User)

instance Core.Hashable User

instance Core.NFData User

instance Core.ToJSON User where
  toJSON User' {..} =
    Core.object
      ( Core.catMaybes
          [ ("groups" Core..=) Core.<$> groups,
            ("password" Core..=) Core.<$> password,
            ("username" Core..=) Core.<$> username,
            ("consoleAccess" Core..=) Core.<$> consoleAccess
          ]
      )
