{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Cloud9.Types.EnvironmentMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentMember where

import Network.AWS.Cloud9.Types.Permissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an environment member for an AWS Cloud9 development
-- environment.
--
-- /See:/ 'newEnvironmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { -- | The Amazon Resource Name (ARN) of the environment member.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The type of environment member permissions associated with this
    -- environment member. Available values include:
    --
    -- -   @owner@: Owns the environment.
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    permissions :: Prelude.Maybe Permissions,
    -- | The ID of the environment for the environment member.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The user ID in AWS Identity and Access Management (AWS IAM) of the
    -- environment member.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The time, expressed in epoch time format, when the environment member
    -- last opened the environment.
    lastAccess :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'environmentMember_userArn' - The Amazon Resource Name (ARN) of the environment member.
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
-- 'environmentId', 'environmentMember_environmentId' - The ID of the environment for the environment member.
--
-- 'userId', 'environmentMember_userId' - The user ID in AWS Identity and Access Management (AWS IAM) of the
-- environment member.
--
-- 'lastAccess', 'environmentMember_lastAccess' - The time, expressed in epoch time format, when the environment member
-- last opened the environment.
newEnvironmentMember ::
  EnvironmentMember
newEnvironmentMember =
  EnvironmentMember'
    { userArn = Prelude.Nothing,
      permissions = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      userId = Prelude.Nothing,
      lastAccess = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the environment member.
environmentMember_userArn :: Lens.Lens' EnvironmentMember (Prelude.Maybe Prelude.Text)
environmentMember_userArn = Lens.lens (\EnvironmentMember' {userArn} -> userArn) (\s@EnvironmentMember' {} a -> s {userArn = a} :: EnvironmentMember)

-- | The type of environment member permissions associated with this
-- environment member. Available values include:
--
-- -   @owner@: Owns the environment.
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
environmentMember_permissions :: Lens.Lens' EnvironmentMember (Prelude.Maybe Permissions)
environmentMember_permissions = Lens.lens (\EnvironmentMember' {permissions} -> permissions) (\s@EnvironmentMember' {} a -> s {permissions = a} :: EnvironmentMember)

-- | The ID of the environment for the environment member.
environmentMember_environmentId :: Lens.Lens' EnvironmentMember (Prelude.Maybe Prelude.Text)
environmentMember_environmentId = Lens.lens (\EnvironmentMember' {environmentId} -> environmentId) (\s@EnvironmentMember' {} a -> s {environmentId = a} :: EnvironmentMember)

-- | The user ID in AWS Identity and Access Management (AWS IAM) of the
-- environment member.
environmentMember_userId :: Lens.Lens' EnvironmentMember (Prelude.Maybe Prelude.Text)
environmentMember_userId = Lens.lens (\EnvironmentMember' {userId} -> userId) (\s@EnvironmentMember' {} a -> s {userId = a} :: EnvironmentMember)

-- | The time, expressed in epoch time format, when the environment member
-- last opened the environment.
environmentMember_lastAccess :: Lens.Lens' EnvironmentMember (Prelude.Maybe Prelude.UTCTime)
environmentMember_lastAccess = Lens.lens (\EnvironmentMember' {lastAccess} -> lastAccess) (\s@EnvironmentMember' {} a -> s {lastAccess = a} :: EnvironmentMember) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON EnvironmentMember where
  parseJSON =
    Prelude.withObject
      "EnvironmentMember"
      ( \x ->
          EnvironmentMember'
            Prelude.<$> (x Prelude..:? "userArn")
            Prelude.<*> (x Prelude..:? "permissions")
            Prelude.<*> (x Prelude..:? "environmentId")
            Prelude.<*> (x Prelude..:? "userId")
            Prelude.<*> (x Prelude..:? "lastAccess")
      )

instance Prelude.Hashable EnvironmentMember

instance Prelude.NFData EnvironmentMember
