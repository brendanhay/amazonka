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
-- Module      : Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.RDSDatabaseCredentials where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The database credentials to connect to a database on an RDS DB instance.
--
-- /See:/ 'newRDSDatabaseCredentials' smart constructor.
data RDSDatabaseCredentials = RDSDatabaseCredentials'
  { username :: Core.Text,
    password :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'password'
  Core.Text ->
  RDSDatabaseCredentials
newRDSDatabaseCredentials pUsername_ pPassword_ =
  RDSDatabaseCredentials'
    { username = pUsername_,
      password = pPassword_
    }

-- | Undocumented member.
rDSDatabaseCredentials_username :: Lens.Lens' RDSDatabaseCredentials Core.Text
rDSDatabaseCredentials_username = Lens.lens (\RDSDatabaseCredentials' {username} -> username) (\s@RDSDatabaseCredentials' {} a -> s {username = a} :: RDSDatabaseCredentials)

-- | Undocumented member.
rDSDatabaseCredentials_password :: Lens.Lens' RDSDatabaseCredentials Core.Text
rDSDatabaseCredentials_password = Lens.lens (\RDSDatabaseCredentials' {password} -> password) (\s@RDSDatabaseCredentials' {} a -> s {password = a} :: RDSDatabaseCredentials)

instance Core.Hashable RDSDatabaseCredentials

instance Core.NFData RDSDatabaseCredentials

instance Core.ToJSON RDSDatabaseCredentials where
  toJSON RDSDatabaseCredentials' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Username" Core..= username),
            Core.Just ("Password" Core..= password)
          ]
      )
