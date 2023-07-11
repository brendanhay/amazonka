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
-- Module      : Amazonka.AppFlow.Types.VeevaConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.VeevaConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using Veeva.
--
-- /See:/ 'newVeevaConnectorProfileCredentials' smart constructor.
data VeevaConnectorProfileCredentials = VeevaConnectorProfileCredentials'
  { -- | The name of the user.
    username :: Prelude.Text,
    -- | The password that corresponds to the user name.
    password :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VeevaConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'veevaConnectorProfileCredentials_username' - The name of the user.
--
-- 'password', 'veevaConnectorProfileCredentials_password' - The password that corresponds to the user name.
newVeevaConnectorProfileCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  VeevaConnectorProfileCredentials
newVeevaConnectorProfileCredentials
  pUsername_
  pPassword_ =
    VeevaConnectorProfileCredentials'
      { username =
          pUsername_,
        password =
          Data._Sensitive Lens.# pPassword_
      }

-- | The name of the user.
veevaConnectorProfileCredentials_username :: Lens.Lens' VeevaConnectorProfileCredentials Prelude.Text
veevaConnectorProfileCredentials_username = Lens.lens (\VeevaConnectorProfileCredentials' {username} -> username) (\s@VeevaConnectorProfileCredentials' {} a -> s {username = a} :: VeevaConnectorProfileCredentials)

-- | The password that corresponds to the user name.
veevaConnectorProfileCredentials_password :: Lens.Lens' VeevaConnectorProfileCredentials Prelude.Text
veevaConnectorProfileCredentials_password = Lens.lens (\VeevaConnectorProfileCredentials' {password} -> password) (\s@VeevaConnectorProfileCredentials' {} a -> s {password = a} :: VeevaConnectorProfileCredentials) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    VeevaConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    VeevaConnectorProfileCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` username
        `Prelude.hashWithSalt` password

instance
  Prelude.NFData
    VeevaConnectorProfileCredentials
  where
  rnf VeevaConnectorProfileCredentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Data.ToJSON VeevaConnectorProfileCredentials where
  toJSON VeevaConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("username" Data..= username),
            Prelude.Just ("password" Data..= password)
          ]
      )
