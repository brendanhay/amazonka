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
-- Module      : Amazonka.EMR.Types.UsernamePassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.UsernamePassword where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The username and password that you use to connect to cluster endpoints.
--
-- /See:/ 'newUsernamePassword' smart constructor.
data UsernamePassword = UsernamePassword'
  { -- | The password associated with the temporary credentials that you use to
    -- connect to cluster endpoints.
    password :: Prelude.Maybe Prelude.Text,
    -- | The username associated with the temporary credentials that you use to
    -- connect to cluster endpoints.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsernamePassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'password', 'usernamePassword_password' - The password associated with the temporary credentials that you use to
-- connect to cluster endpoints.
--
-- 'username', 'usernamePassword_username' - The username associated with the temporary credentials that you use to
-- connect to cluster endpoints.
newUsernamePassword ::
  UsernamePassword
newUsernamePassword =
  UsernamePassword'
    { password = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The password associated with the temporary credentials that you use to
-- connect to cluster endpoints.
usernamePassword_password :: Lens.Lens' UsernamePassword (Prelude.Maybe Prelude.Text)
usernamePassword_password = Lens.lens (\UsernamePassword' {password} -> password) (\s@UsernamePassword' {} a -> s {password = a} :: UsernamePassword)

-- | The username associated with the temporary credentials that you use to
-- connect to cluster endpoints.
usernamePassword_username :: Lens.Lens' UsernamePassword (Prelude.Maybe Prelude.Text)
usernamePassword_username = Lens.lens (\UsernamePassword' {username} -> username) (\s@UsernamePassword' {} a -> s {username = a} :: UsernamePassword)

instance Data.FromJSON UsernamePassword where
  parseJSON =
    Data.withObject
      "UsernamePassword"
      ( \x ->
          UsernamePassword'
            Prelude.<$> (x Data..:? "Password")
            Prelude.<*> (x Data..:? "Username")
      )

instance Prelude.Hashable UsernamePassword where
  hashWithSalt _salt UsernamePassword' {..} =
    _salt
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username

instance Prelude.NFData UsernamePassword where
  rnf UsernamePassword' {..} =
    Prelude.rnf password
      `Prelude.seq` Prelude.rnf username
