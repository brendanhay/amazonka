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
-- Module      : Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the database credentials for connecting to a database on an
-- Amazon Redshift cluster.
--
-- /See:/ 'newRedshiftDatabaseCredentials' smart constructor.
data RedshiftDatabaseCredentials = RedshiftDatabaseCredentials'
  { username :: Core.Text,
    password :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'password'
  Core.Text ->
  RedshiftDatabaseCredentials
newRedshiftDatabaseCredentials pUsername_ pPassword_ =
  RedshiftDatabaseCredentials'
    { username = pUsername_,
      password = pPassword_
    }

-- | Undocumented member.
redshiftDatabaseCredentials_username :: Lens.Lens' RedshiftDatabaseCredentials Core.Text
redshiftDatabaseCredentials_username = Lens.lens (\RedshiftDatabaseCredentials' {username} -> username) (\s@RedshiftDatabaseCredentials' {} a -> s {username = a} :: RedshiftDatabaseCredentials)

-- | Undocumented member.
redshiftDatabaseCredentials_password :: Lens.Lens' RedshiftDatabaseCredentials Core.Text
redshiftDatabaseCredentials_password = Lens.lens (\RedshiftDatabaseCredentials' {password} -> password) (\s@RedshiftDatabaseCredentials' {} a -> s {password = a} :: RedshiftDatabaseCredentials)

instance Core.Hashable RedshiftDatabaseCredentials

instance Core.NFData RedshiftDatabaseCredentials

instance Core.ToJSON RedshiftDatabaseCredentials where
  toJSON RedshiftDatabaseCredentials' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password)
          ]
      )
