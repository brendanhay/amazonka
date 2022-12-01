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
-- Module      : Amazonka.AppFlow.Types.BasicAuthCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.BasicAuthCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The basic auth credentials required for basic authentication.
--
-- /See:/ 'newBasicAuthCredentials' smart constructor.
data BasicAuthCredentials = BasicAuthCredentials'
  { -- | The username to use to connect to a resource.
    username :: Prelude.Text,
    -- | The password to use to connect to a resource.
    password :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BasicAuthCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'basicAuthCredentials_username' - The username to use to connect to a resource.
--
-- 'password', 'basicAuthCredentials_password' - The password to use to connect to a resource.
newBasicAuthCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  BasicAuthCredentials
newBasicAuthCredentials pUsername_ pPassword_ =
  BasicAuthCredentials'
    { username = pUsername_,
      password = Core._Sensitive Lens.# pPassword_
    }

-- | The username to use to connect to a resource.
basicAuthCredentials_username :: Lens.Lens' BasicAuthCredentials Prelude.Text
basicAuthCredentials_username = Lens.lens (\BasicAuthCredentials' {username} -> username) (\s@BasicAuthCredentials' {} a -> s {username = a} :: BasicAuthCredentials)

-- | The password to use to connect to a resource.
basicAuthCredentials_password :: Lens.Lens' BasicAuthCredentials Prelude.Text
basicAuthCredentials_password = Lens.lens (\BasicAuthCredentials' {password} -> password) (\s@BasicAuthCredentials' {} a -> s {password = a} :: BasicAuthCredentials) Prelude.. Core._Sensitive

instance Prelude.Hashable BasicAuthCredentials where
  hashWithSalt _salt BasicAuthCredentials' {..} =
    _salt `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData BasicAuthCredentials where
  rnf BasicAuthCredentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Core.ToJSON BasicAuthCredentials where
  toJSON BasicAuthCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("username" Core..= username),
            Prelude.Just ("password" Core..= password)
          ]
      )
