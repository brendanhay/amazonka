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
-- Module      : Amazonka.Cloud9.Types.EnvironmentMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.EnvironmentMember where

import Amazonka.Cloud9.Types.Permissions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an environment member for an Cloud9 development
-- environment.
--
-- /See:/ 'newEnvironmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { -- | The time, expressed in epoch time format, when the environment member
    -- last opened the environment.
    lastAccess :: Prelude.Maybe Data.POSIX,
    -- | The type of environment member permissions associated with this
    -- environment member. Available values include:
    --
    -- -   @owner@: Owns the environment.
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    permissions :: Permissions,
    -- | The user ID in Identity and Access Management (IAM) of the environment
    -- member.
    userId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment member.
    userArn :: Prelude.Text,
    -- | The ID of the environment for the environment member.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastAccess', 'environmentMember_lastAccess' - The time, expressed in epoch time format, when the environment member
-- last opened the environment.
--
-- 'permissions', 'environmentMember_permissions' - The type of environment member permissions associated with this
-- environment member. Available values include:
--
-- -   @owner@: Owns the environment.
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
--
-- 'userId', 'environmentMember_userId' - The user ID in Identity and Access Management (IAM) of the environment
-- member.
--
-- 'userArn', 'environmentMember_userArn' - The Amazon Resource Name (ARN) of the environment member.
--
-- 'environmentId', 'environmentMember_environmentId' - The ID of the environment for the environment member.
newEnvironmentMember ::
  -- | 'permissions'
  Permissions ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'userArn'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  EnvironmentMember
newEnvironmentMember
  pPermissions_
  pUserId_
  pUserArn_
  pEnvironmentId_ =
    EnvironmentMember'
      { lastAccess = Prelude.Nothing,
        permissions = pPermissions_,
        userId = pUserId_,
        userArn = pUserArn_,
        environmentId = pEnvironmentId_
      }

-- | The time, expressed in epoch time format, when the environment member
-- last opened the environment.
environmentMember_lastAccess :: Lens.Lens' EnvironmentMember (Prelude.Maybe Prelude.UTCTime)
environmentMember_lastAccess = Lens.lens (\EnvironmentMember' {lastAccess} -> lastAccess) (\s@EnvironmentMember' {} a -> s {lastAccess = a} :: EnvironmentMember) Prelude.. Lens.mapping Data._Time

-- | The type of environment member permissions associated with this
-- environment member. Available values include:
--
-- -   @owner@: Owns the environment.
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
environmentMember_permissions :: Lens.Lens' EnvironmentMember Permissions
environmentMember_permissions = Lens.lens (\EnvironmentMember' {permissions} -> permissions) (\s@EnvironmentMember' {} a -> s {permissions = a} :: EnvironmentMember)

-- | The user ID in Identity and Access Management (IAM) of the environment
-- member.
environmentMember_userId :: Lens.Lens' EnvironmentMember Prelude.Text
environmentMember_userId = Lens.lens (\EnvironmentMember' {userId} -> userId) (\s@EnvironmentMember' {} a -> s {userId = a} :: EnvironmentMember)

-- | The Amazon Resource Name (ARN) of the environment member.
environmentMember_userArn :: Lens.Lens' EnvironmentMember Prelude.Text
environmentMember_userArn = Lens.lens (\EnvironmentMember' {userArn} -> userArn) (\s@EnvironmentMember' {} a -> s {userArn = a} :: EnvironmentMember)

-- | The ID of the environment for the environment member.
environmentMember_environmentId :: Lens.Lens' EnvironmentMember Prelude.Text
environmentMember_environmentId = Lens.lens (\EnvironmentMember' {environmentId} -> environmentId) (\s@EnvironmentMember' {} a -> s {environmentId = a} :: EnvironmentMember)

instance Data.FromJSON EnvironmentMember where
  parseJSON =
    Data.withObject
      "EnvironmentMember"
      ( \x ->
          EnvironmentMember'
            Prelude.<$> (x Data..:? "lastAccess")
            Prelude.<*> (x Data..: "permissions")
            Prelude.<*> (x Data..: "userId")
            Prelude.<*> (x Data..: "userArn")
            Prelude.<*> (x Data..: "environmentId")
      )

instance Prelude.Hashable EnvironmentMember where
  hashWithSalt _salt EnvironmentMember' {..} =
    _salt `Prelude.hashWithSalt` lastAccess
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData EnvironmentMember where
  rnf EnvironmentMember' {..} =
    Prelude.rnf lastAccess
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf environmentId
