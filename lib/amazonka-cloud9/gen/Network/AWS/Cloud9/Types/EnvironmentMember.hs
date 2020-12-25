{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentMember
  ( EnvironmentMember (..),

    -- * Smart constructor
    mkEnvironmentMember,

    -- * Lenses
    emEnvironmentId,
    emLastAccess,
    emPermissions,
    emUserArn,
    emUserId,
  )
where

import qualified Network.AWS.Cloud9.Types.EnvironmentId as Types
import qualified Network.AWS.Cloud9.Types.Permissions as Types
import qualified Network.AWS.Cloud9.Types.String as Types
import qualified Network.AWS.Cloud9.Types.UserArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an environment member for an AWS Cloud9 development environment.
--
-- /See:/ 'mkEnvironmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { -- | The ID of the environment for the environment member.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | The time, expressed in epoch time format, when the environment member last opened the environment.
    lastAccess :: Core.Maybe Core.NominalDiffTime,
    -- | The type of environment member permissions associated with this environment member. Available values include:
    --
    --
    --     * @owner@ : Owns the environment.
    --
    --
    --     * @read-only@ : Has read-only access to the environment.
    --
    --
    --     * @read-write@ : Has read-write access to the environment.
    permissions :: Core.Maybe Types.Permissions,
    -- | The Amazon Resource Name (ARN) of the environment member.
    userArn :: Core.Maybe Types.UserArn,
    -- | The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
    userId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EnvironmentMember' value with any optional fields omitted.
mkEnvironmentMember ::
  EnvironmentMember
mkEnvironmentMember =
  EnvironmentMember'
    { environmentId = Core.Nothing,
      lastAccess = Core.Nothing,
      permissions = Core.Nothing,
      userArn = Core.Nothing,
      userId = Core.Nothing
    }

-- | The ID of the environment for the environment member.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emEnvironmentId :: Lens.Lens' EnvironmentMember (Core.Maybe Types.EnvironmentId)
emEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED emEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The time, expressed in epoch time format, when the environment member last opened the environment.
--
-- /Note:/ Consider using 'lastAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emLastAccess :: Lens.Lens' EnvironmentMember (Core.Maybe Core.NominalDiffTime)
emLastAccess = Lens.field @"lastAccess"
{-# DEPRECATED emLastAccess "Use generic-lens or generic-optics with 'lastAccess' instead." #-}

-- | The type of environment member permissions associated with this environment member. Available values include:
--
--
--     * @owner@ : Owns the environment.
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emPermissions :: Lens.Lens' EnvironmentMember (Core.Maybe Types.Permissions)
emPermissions = Lens.field @"permissions"
{-# DEPRECATED emPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emUserArn :: Lens.Lens' EnvironmentMember (Core.Maybe Types.UserArn)
emUserArn = Lens.field @"userArn"
{-# DEPRECATED emUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emUserId :: Lens.Lens' EnvironmentMember (Core.Maybe Types.String)
emUserId = Lens.field @"userId"
{-# DEPRECATED emUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.FromJSON EnvironmentMember where
  parseJSON =
    Core.withObject "EnvironmentMember" Core.$
      \x ->
        EnvironmentMember'
          Core.<$> (x Core..:? "environmentId")
          Core.<*> (x Core..:? "lastAccess")
          Core.<*> (x Core..:? "permissions")
          Core.<*> (x Core..:? "userArn")
          Core.<*> (x Core..:? "userId")
