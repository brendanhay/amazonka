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
-- Module      : Amazonka.MachineLearning.Types.RDSDatabaseCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RDSDatabaseCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The database credentials to connect to a database on an RDS DB instance.
--
-- /See:/ 'newRDSDatabaseCredentials' smart constructor.
data RDSDatabaseCredentials = RDSDatabaseCredentials'
  { username :: Prelude.Text,
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RDSDatabaseCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'rDSDatabaseCredentials_username' - Undocumented member.
--
-- 'password', 'rDSDatabaseCredentials_password' - Undocumented member.
newRDSDatabaseCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  RDSDatabaseCredentials
newRDSDatabaseCredentials pUsername_ pPassword_ =
  RDSDatabaseCredentials'
    { username = pUsername_,
      password = pPassword_
    }

-- | Undocumented member.
rDSDatabaseCredentials_username :: Lens.Lens' RDSDatabaseCredentials Prelude.Text
rDSDatabaseCredentials_username = Lens.lens (\RDSDatabaseCredentials' {username} -> username) (\s@RDSDatabaseCredentials' {} a -> s {username = a} :: RDSDatabaseCredentials)

-- | Undocumented member.
rDSDatabaseCredentials_password :: Lens.Lens' RDSDatabaseCredentials Prelude.Text
rDSDatabaseCredentials_password = Lens.lens (\RDSDatabaseCredentials' {password} -> password) (\s@RDSDatabaseCredentials' {} a -> s {password = a} :: RDSDatabaseCredentials)

instance Prelude.Hashable RDSDatabaseCredentials where
  hashWithSalt _salt RDSDatabaseCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData RDSDatabaseCredentials where
  rnf RDSDatabaseCredentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Data.ToJSON RDSDatabaseCredentials where
  toJSON RDSDatabaseCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Username" Data..= username),
            Prelude.Just ("Password" Data..= password)
          ]
      )
