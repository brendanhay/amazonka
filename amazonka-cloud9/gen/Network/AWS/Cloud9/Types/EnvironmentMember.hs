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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an environment member for an AWS Cloud9 development
-- environment.
--
-- /See:/ 'newEnvironmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { -- | The Amazon Resource Name (ARN) of the environment member.
    userArn :: Core.Maybe Core.Text,
    -- | The type of environment member permissions associated with this
    -- environment member. Available values include:
    --
    -- -   @owner@: Owns the environment.
    --
    -- -   @read-only@: Has read-only access to the environment.
    --
    -- -   @read-write@: Has read-write access to the environment.
    permissions :: Core.Maybe Permissions,
    -- | The ID of the environment for the environment member.
    environmentId :: Core.Maybe Core.Text,
    -- | The user ID in AWS Identity and Access Management (AWS IAM) of the
    -- environment member.
    userId :: Core.Maybe Core.Text,
    -- | The time, expressed in epoch time format, when the environment member
    -- last opened the environment.
    lastAccess :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { userArn = Core.Nothing,
      permissions = Core.Nothing,
      environmentId = Core.Nothing,
      userId = Core.Nothing,
      lastAccess = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the environment member.
environmentMember_userArn :: Lens.Lens' EnvironmentMember (Core.Maybe Core.Text)
environmentMember_userArn = Lens.lens (\EnvironmentMember' {userArn} -> userArn) (\s@EnvironmentMember' {} a -> s {userArn = a} :: EnvironmentMember)

-- | The type of environment member permissions associated with this
-- environment member. Available values include:
--
-- -   @owner@: Owns the environment.
--
-- -   @read-only@: Has read-only access to the environment.
--
-- -   @read-write@: Has read-write access to the environment.
environmentMember_permissions :: Lens.Lens' EnvironmentMember (Core.Maybe Permissions)
environmentMember_permissions = Lens.lens (\EnvironmentMember' {permissions} -> permissions) (\s@EnvironmentMember' {} a -> s {permissions = a} :: EnvironmentMember)

-- | The ID of the environment for the environment member.
environmentMember_environmentId :: Lens.Lens' EnvironmentMember (Core.Maybe Core.Text)
environmentMember_environmentId = Lens.lens (\EnvironmentMember' {environmentId} -> environmentId) (\s@EnvironmentMember' {} a -> s {environmentId = a} :: EnvironmentMember)

-- | The user ID in AWS Identity and Access Management (AWS IAM) of the
-- environment member.
environmentMember_userId :: Lens.Lens' EnvironmentMember (Core.Maybe Core.Text)
environmentMember_userId = Lens.lens (\EnvironmentMember' {userId} -> userId) (\s@EnvironmentMember' {} a -> s {userId = a} :: EnvironmentMember)

-- | The time, expressed in epoch time format, when the environment member
-- last opened the environment.
environmentMember_lastAccess :: Lens.Lens' EnvironmentMember (Core.Maybe Core.UTCTime)
environmentMember_lastAccess = Lens.lens (\EnvironmentMember' {lastAccess} -> lastAccess) (\s@EnvironmentMember' {} a -> s {lastAccess = a} :: EnvironmentMember) Core.. Lens.mapping Core._Time

instance Core.FromJSON EnvironmentMember where
  parseJSON =
    Core.withObject
      "EnvironmentMember"
      ( \x ->
          EnvironmentMember'
            Core.<$> (x Core..:? "userArn")
            Core.<*> (x Core..:? "permissions")
            Core.<*> (x Core..:? "environmentId")
            Core.<*> (x Core..:? "userId")
            Core.<*> (x Core..:? "lastAccess")
      )

instance Core.Hashable EnvironmentMember

instance Core.NFData EnvironmentMember
