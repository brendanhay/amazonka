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
    emLastAccess,
    emUserId,
    emUserARN,
    emPermissions,
    emEnvironmentId,
  )
where

import Network.AWS.Cloud9.Types.Permissions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an environment member for an AWS Cloud9 development environment.
--
-- /See:/ 'mkEnvironmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { -- | The time, expressed in epoch time format, when the environment member last opened the environment.
    lastAccess :: Lude.Maybe Lude.Timestamp,
    -- | The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
    userId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the environment member.
    userARN :: Lude.Maybe Lude.Text,
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
    permissions :: Lude.Maybe Permissions,
    -- | The ID of the environment for the environment member.
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentMember' with the minimum fields required to make a request.
--
-- * 'lastAccess' - The time, expressed in epoch time format, when the environment member last opened the environment.
-- * 'userId' - The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
-- * 'userARN' - The Amazon Resource Name (ARN) of the environment member.
-- * 'permissions' - The type of environment member permissions associated with this environment member. Available values include:
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
-- * 'environmentId' - The ID of the environment for the environment member.
mkEnvironmentMember ::
  EnvironmentMember
mkEnvironmentMember =
  EnvironmentMember'
    { lastAccess = Lude.Nothing,
      userId = Lude.Nothing,
      userARN = Lude.Nothing,
      permissions = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | The time, expressed in epoch time format, when the environment member last opened the environment.
--
-- /Note:/ Consider using 'lastAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emLastAccess :: Lens.Lens' EnvironmentMember (Lude.Maybe Lude.Timestamp)
emLastAccess = Lens.lens (lastAccess :: EnvironmentMember -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccess = a} :: EnvironmentMember)
{-# DEPRECATED emLastAccess "Use generic-lens or generic-optics with 'lastAccess' instead." #-}

-- | The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emUserId :: Lens.Lens' EnvironmentMember (Lude.Maybe Lude.Text)
emUserId = Lens.lens (userId :: EnvironmentMember -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: EnvironmentMember)
{-# DEPRECATED emUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emUserARN :: Lens.Lens' EnvironmentMember (Lude.Maybe Lude.Text)
emUserARN = Lens.lens (userARN :: EnvironmentMember -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: EnvironmentMember)
{-# DEPRECATED emUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

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
emPermissions :: Lens.Lens' EnvironmentMember (Lude.Maybe Permissions)
emPermissions = Lens.lens (permissions :: EnvironmentMember -> Lude.Maybe Permissions) (\s a -> s {permissions = a} :: EnvironmentMember)
{-# DEPRECATED emPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The ID of the environment for the environment member.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emEnvironmentId :: Lens.Lens' EnvironmentMember (Lude.Maybe Lude.Text)
emEnvironmentId = Lens.lens (environmentId :: EnvironmentMember -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: EnvironmentMember)
{-# DEPRECATED emEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.FromJSON EnvironmentMember where
  parseJSON =
    Lude.withObject
      "EnvironmentMember"
      ( \x ->
          EnvironmentMember'
            Lude.<$> (x Lude..:? "lastAccess")
            Lude.<*> (x Lude..:? "userId")
            Lude.<*> (x Lude..:? "userArn")
            Lude.<*> (x Lude..:? "permissions")
            Lude.<*> (x Lude..:? "environmentId")
      )
