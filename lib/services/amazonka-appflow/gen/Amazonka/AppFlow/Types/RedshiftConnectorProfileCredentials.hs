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
-- Module      : Amazonka.AppFlow.Types.RedshiftConnectorProfileCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.RedshiftConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using Amazon
-- Redshift.
--
-- /See:/ 'newRedshiftConnectorProfileCredentials' smart constructor.
data RedshiftConnectorProfileCredentials = RedshiftConnectorProfileCredentials'
  { -- | The password that corresponds to the user name.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The name of the user.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'password', 'redshiftConnectorProfileCredentials_password' - The password that corresponds to the user name.
--
-- 'username', 'redshiftConnectorProfileCredentials_username' - The name of the user.
newRedshiftConnectorProfileCredentials ::
  RedshiftConnectorProfileCredentials
newRedshiftConnectorProfileCredentials =
  RedshiftConnectorProfileCredentials'
    { password =
        Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The password that corresponds to the user name.
redshiftConnectorProfileCredentials_password :: Lens.Lens' RedshiftConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileCredentials_password = Lens.lens (\RedshiftConnectorProfileCredentials' {password} -> password) (\s@RedshiftConnectorProfileCredentials' {} a -> s {password = a} :: RedshiftConnectorProfileCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The name of the user.
redshiftConnectorProfileCredentials_username :: Lens.Lens' RedshiftConnectorProfileCredentials (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileCredentials_username = Lens.lens (\RedshiftConnectorProfileCredentials' {username} -> username) (\s@RedshiftConnectorProfileCredentials' {} a -> s {username = a} :: RedshiftConnectorProfileCredentials)

instance
  Prelude.Hashable
    RedshiftConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    RedshiftConnectorProfileCredentials' {..} =
      _salt `Prelude.hashWithSalt` password
        `Prelude.hashWithSalt` username

instance
  Prelude.NFData
    RedshiftConnectorProfileCredentials
  where
  rnf RedshiftConnectorProfileCredentials' {..} =
    Prelude.rnf password
      `Prelude.seq` Prelude.rnf username

instance
  Core.ToJSON
    RedshiftConnectorProfileCredentials
  where
  toJSON RedshiftConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("password" Core..=) Prelude.<$> password,
            ("username" Core..=) Prelude.<$> username
          ]
      )
