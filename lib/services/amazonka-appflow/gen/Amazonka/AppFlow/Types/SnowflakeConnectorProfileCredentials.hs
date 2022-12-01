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
-- Module      : Amazonka.AppFlow.Types.SnowflakeConnectorProfileCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SnowflakeConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using
-- Snowflake.
--
-- /See:/ 'newSnowflakeConnectorProfileCredentials' smart constructor.
data SnowflakeConnectorProfileCredentials = SnowflakeConnectorProfileCredentials'
  { -- | The name of the user.
    username :: Prelude.Text,
    -- | The password that corresponds to the user name.
    password :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnowflakeConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'snowflakeConnectorProfileCredentials_username' - The name of the user.
--
-- 'password', 'snowflakeConnectorProfileCredentials_password' - The password that corresponds to the user name.
newSnowflakeConnectorProfileCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  SnowflakeConnectorProfileCredentials
newSnowflakeConnectorProfileCredentials
  pUsername_
  pPassword_ =
    SnowflakeConnectorProfileCredentials'
      { username =
          pUsername_,
        password =
          Core._Sensitive Lens.# pPassword_
      }

-- | The name of the user.
snowflakeConnectorProfileCredentials_username :: Lens.Lens' SnowflakeConnectorProfileCredentials Prelude.Text
snowflakeConnectorProfileCredentials_username = Lens.lens (\SnowflakeConnectorProfileCredentials' {username} -> username) (\s@SnowflakeConnectorProfileCredentials' {} a -> s {username = a} :: SnowflakeConnectorProfileCredentials)

-- | The password that corresponds to the user name.
snowflakeConnectorProfileCredentials_password :: Lens.Lens' SnowflakeConnectorProfileCredentials Prelude.Text
snowflakeConnectorProfileCredentials_password = Lens.lens (\SnowflakeConnectorProfileCredentials' {password} -> password) (\s@SnowflakeConnectorProfileCredentials' {} a -> s {password = a} :: SnowflakeConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    SnowflakeConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    SnowflakeConnectorProfileCredentials' {..} =
      _salt `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` password

instance
  Prelude.NFData
    SnowflakeConnectorProfileCredentials
  where
  rnf SnowflakeConnectorProfileCredentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance
  Core.ToJSON
    SnowflakeConnectorProfileCredentials
  where
  toJSON SnowflakeConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("username" Core..= username),
            Prelude.Just ("password" Core..= password)
          ]
      )
