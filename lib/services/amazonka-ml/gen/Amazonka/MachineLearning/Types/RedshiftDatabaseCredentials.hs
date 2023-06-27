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
-- Module      : Amazonka.MachineLearning.Types.RedshiftDatabaseCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.RedshiftDatabaseCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the database credentials for connecting to a database on an
-- Amazon Redshift cluster.
--
-- /See:/ 'newRedshiftDatabaseCredentials' smart constructor.
data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials'
  { username :: Prelude.Text,
    password :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDatabaseCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'redshiftDatabaseCredentials_username' - Undocumented member.
--
-- 'password', 'redshiftDatabaseCredentials_password' - Undocumented member.
newRedshiftDatabaseCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  RedshiftDatabaseCredentials
newRedshiftDatabaseCredentials pUsername_ pPassword_ =
  RedshiftDatabaseCredentials'
    { username = pUsername_,
      password = pPassword_
    }

-- | Undocumented member.
redshiftDatabaseCredentials_username :: Lens.Lens' RedshiftDatabaseCredentials Prelude.Text
redshiftDatabaseCredentials_username = Lens.lens (\RedshiftDatabaseCredentials' {username} -> username) (\s@RedshiftDatabaseCredentials' {} a -> s {username = a} :: RedshiftDatabaseCredentials)

-- | Undocumented member.
redshiftDatabaseCredentials_password :: Lens.Lens' RedshiftDatabaseCredentials Prelude.Text
redshiftDatabaseCredentials_password = Lens.lens (\RedshiftDatabaseCredentials' {password} -> password) (\s@RedshiftDatabaseCredentials' {} a -> s {password = a} :: RedshiftDatabaseCredentials)

instance Prelude.Hashable RedshiftDatabaseCredentials where
  hashWithSalt _salt RedshiftDatabaseCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` password

instance Prelude.NFData RedshiftDatabaseCredentials where
  rnf RedshiftDatabaseCredentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance Data.ToJSON RedshiftDatabaseCredentials where
  toJSON RedshiftDatabaseCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Username" Data..= username),
            Prelude.Just ("Password" Data..= password)
          ]
      )
