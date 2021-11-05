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
-- Module      : Network.AWS.AppFlow.Types.RedshiftConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.RedshiftConnectorProfileCredentials where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The connector-specific profile credentials required when using Amazon
-- Redshift.
--
-- /See:/ 'newRedshiftConnectorProfileCredentials' smart constructor.
data RedshiftConnectorProfileCredentials = RedshiftConnectorProfileCredentials'
  { -- | The name of the user.
    username :: Prelude.Text,
    -- | The password that corresponds to the user name.
    password :: Core.Sensitive Prelude.Text
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
-- 'username', 'redshiftConnectorProfileCredentials_username' - The name of the user.
--
-- 'password', 'redshiftConnectorProfileCredentials_password' - The password that corresponds to the user name.
newRedshiftConnectorProfileCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  RedshiftConnectorProfileCredentials
newRedshiftConnectorProfileCredentials
  pUsername_
  pPassword_ =
    RedshiftConnectorProfileCredentials'
      { username =
          pUsername_,
        password =
          Core._Sensitive Lens.# pPassword_
      }

-- | The name of the user.
redshiftConnectorProfileCredentials_username :: Lens.Lens' RedshiftConnectorProfileCredentials Prelude.Text
redshiftConnectorProfileCredentials_username = Lens.lens (\RedshiftConnectorProfileCredentials' {username} -> username) (\s@RedshiftConnectorProfileCredentials' {} a -> s {username = a} :: RedshiftConnectorProfileCredentials)

-- | The password that corresponds to the user name.
redshiftConnectorProfileCredentials_password :: Lens.Lens' RedshiftConnectorProfileCredentials Prelude.Text
redshiftConnectorProfileCredentials_password = Lens.lens (\RedshiftConnectorProfileCredentials' {password} -> password) (\s@RedshiftConnectorProfileCredentials' {} a -> s {password = a} :: RedshiftConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    RedshiftConnectorProfileCredentials

instance
  Prelude.NFData
    RedshiftConnectorProfileCredentials

instance
  Core.ToJSON
    RedshiftConnectorProfileCredentials
  where
  toJSON RedshiftConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("username" Core..= username),
            Prelude.Just ("password" Core..= password)
          ]
      )
