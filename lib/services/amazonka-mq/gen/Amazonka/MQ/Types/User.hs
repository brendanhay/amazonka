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
-- Module      : Amazonka.MQ.Types.User
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A user associated with the broker. For Amazon MQ for RabbitMQ brokers,
-- one and only one administrative user is accepted and created when a
-- broker is first provisioned. All subsequent broker users are created by
-- making RabbitMQ API calls directly to brokers or via the RabbitMQ web
-- console.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | Enables access to the ActiveMQ Web Console for the ActiveMQ user. Does
    -- not apply to RabbitMQ brokers.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long. Does not apply to RabbitMQ brokers.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Defines if this user is intended for CRDR replication purposes.
    replicationUser :: Prelude.Maybe Prelude.Bool,
    -- | The username of the broker user. The following restrictions apply to
    -- broker usernames:
    --
    -- -   For Amazon MQ for ActiveMQ brokers, this value can contain only
    --     alphanumeric characters, dashes, periods, underscores, and tildes (-
    --     . _ ~). This value must be 2-100 characters long.
    --
    -- -   para>For Amazon MQ for RabbitMQ brokers, this value can contain only
    --     alphanumeric characters, dashes, periods, underscores (- . _). This
    --     value must not contain a tilde (~) character. Amazon MQ prohibts
    --     using guest as a valid usename. This value must be 2-100 characters
    --     long.
    --
    -- Do not add personally identifiable information (PII) or other
    -- confidential or sensitive information in broker usernames. Broker
    -- usernames are accessible to other Amazon Web Services services,
    -- including CloudWatch Logs. Broker usernames are not intended to be used
    -- for private or sensitive data.
    username :: Prelude.Text,
    -- | Required. The password of the user. This value must be at least 12
    -- characters long, must contain at least 4 unique characters, and must not
    -- contain commas, colons, or equal signs (,:=).
    password :: Prelude.Text
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
-- 'consoleAccess', 'user_consoleAccess' - Enables access to the ActiveMQ Web Console for the ActiveMQ user. Does
-- not apply to RabbitMQ brokers.
--
-- 'groups', 'user_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long. Does not apply to RabbitMQ brokers.
--
-- 'replicationUser', 'user_replicationUser' - Defines if this user is intended for CRDR replication purposes.
--
-- 'username', 'user_username' - The username of the broker user. The following restrictions apply to
-- broker usernames:
--
-- -   For Amazon MQ for ActiveMQ brokers, this value can contain only
--     alphanumeric characters, dashes, periods, underscores, and tildes (-
--     . _ ~). This value must be 2-100 characters long.
--
-- -   para>For Amazon MQ for RabbitMQ brokers, this value can contain only
--     alphanumeric characters, dashes, periods, underscores (- . _). This
--     value must not contain a tilde (~) character. Amazon MQ prohibts
--     using guest as a valid usename. This value must be 2-100 characters
--     long.
--
-- Do not add personally identifiable information (PII) or other
-- confidential or sensitive information in broker usernames. Broker
-- usernames are accessible to other Amazon Web Services services,
-- including CloudWatch Logs. Broker usernames are not intended to be used
-- for private or sensitive data.
--
-- 'password', 'user_password' - Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas, colons, or equal signs (,:=).
newUser ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  User
newUser pUsername_ pPassword_ =
  User'
    { consoleAccess = Prelude.Nothing,
      groups = Prelude.Nothing,
      replicationUser = Prelude.Nothing,
      username = pUsername_,
      password = pPassword_
    }

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user. Does
-- not apply to RabbitMQ brokers.
user_consoleAccess :: Lens.Lens' User (Prelude.Maybe Prelude.Bool)
user_consoleAccess = Lens.lens (\User' {consoleAccess} -> consoleAccess) (\s@User' {} a -> s {consoleAccess = a} :: User)

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long. Does not apply to RabbitMQ brokers.
user_groups :: Lens.Lens' User (Prelude.Maybe [Prelude.Text])
user_groups = Lens.lens (\User' {groups} -> groups) (\s@User' {} a -> s {groups = a} :: User) Prelude.. Lens.mapping Lens.coerced

-- | Defines if this user is intended for CRDR replication purposes.
user_replicationUser :: Lens.Lens' User (Prelude.Maybe Prelude.Bool)
user_replicationUser = Lens.lens (\User' {replicationUser} -> replicationUser) (\s@User' {} a -> s {replicationUser = a} :: User)

-- | The username of the broker user. The following restrictions apply to
-- broker usernames:
--
-- -   For Amazon MQ for ActiveMQ brokers, this value can contain only
--     alphanumeric characters, dashes, periods, underscores, and tildes (-
--     . _ ~). This value must be 2-100 characters long.
--
-- -   para>For Amazon MQ for RabbitMQ brokers, this value can contain only
--     alphanumeric characters, dashes, periods, underscores (- . _). This
--     value must not contain a tilde (~) character. Amazon MQ prohibts
--     using guest as a valid usename. This value must be 2-100 characters
--     long.
--
-- Do not add personally identifiable information (PII) or other
-- confidential or sensitive information in broker usernames. Broker
-- usernames are accessible to other Amazon Web Services services,
-- including CloudWatch Logs. Broker usernames are not intended to be used
-- for private or sensitive data.
user_username :: Lens.Lens' User Prelude.Text
user_username = Lens.lens (\User' {username} -> username) (\s@User' {} a -> s {username = a} :: User)

-- | Required. The password of the user. This value must be at least 12
-- characters long, must contain at least 4 unique characters, and must not
-- contain commas, colons, or equal signs (,:=).
user_password :: Lens.Lens' User Prelude.Text
user_password = Lens.lens (\User' {password} -> password) (\s@User' {} a -> s {password = a} :: User)

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt
      `Prelude.hashWithSalt` consoleAccess
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` replicationUser
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf consoleAccess
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf replicationUser
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Data.ToJSON User where
  toJSON User' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("consoleAccess" Data..=) Prelude.<$> consoleAccess,
            ("groups" Data..=) Prelude.<$> groups,
            ("replicationUser" Data..=)
              Prelude.<$> replicationUser,
            Prelude.Just ("username" Data..= username),
            Prelude.Just ("password" Data..= password)
          ]
      )
