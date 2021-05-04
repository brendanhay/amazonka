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
-- Module      : Network.AWS.MQ.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.User where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A user associated with the broker.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Required. The password of the broker user. This value must be at least
    -- 12 characters long, must contain at least 4 unique characters, and must
    -- not contain commas.
    password :: Prelude.Maybe Prelude.Text,
    -- | Required. The username of the broker user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Maybe Prelude.Text,
    -- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does
    -- not apply to RabbitMQ brokers).
    consoleAccess :: Prelude.Maybe Prelude.Bool
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
    { groups = Prelude.Nothing,
      password = Prelude.Nothing,
      username = Prelude.Nothing,
      consoleAccess = Prelude.Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
user_groups :: Lens.Lens' User (Prelude.Maybe [Prelude.Text])
user_groups = Lens.lens (\User' {groups} -> groups) (\s@User' {} a -> s {groups = a} :: User) Prelude.. Lens.mapping Prelude._Coerce

-- | Required. The password of the broker user. This value must be at least
-- 12 characters long, must contain at least 4 unique characters, and must
-- not contain commas.
user_password :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_password = Lens.lens (\User' {password} -> password) (\s@User' {} a -> s {password = a} :: User)

-- | Required. The username of the broker user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
user_username :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does
-- not apply to RabbitMQ brokers).
user_consoleAccess :: Lens.Lens' User (Prelude.Maybe Prelude.Bool)
user_consoleAccess = Lens.lens (\User' {consoleAccess} -> consoleAccess) (\s@User' {} a -> s {consoleAccess = a} :: User)

instance Prelude.Hashable User

instance Prelude.NFData User

instance Prelude.ToJSON User where
  toJSON User' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("groups" Prelude..=) Prelude.<$> groups,
            ("password" Prelude..=) Prelude.<$> password,
            ("username" Prelude..=) Prelude.<$> username,
            ("consoleAccess" Prelude..=)
              Prelude.<$> consoleAccess
          ]
      )
