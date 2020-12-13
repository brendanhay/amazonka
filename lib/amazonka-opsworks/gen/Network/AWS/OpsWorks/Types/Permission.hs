{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Permission
  ( Permission (..),

    -- * Smart constructor
    mkPermission,

    -- * Lenses
    pIAMUserARN,
    pAllowSudo,
    pStackId,
    pLevel,
    pAllowSSH,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes stack or user permissions.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { -- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    iamUserARN :: Lude.Maybe Lude.Text,
    -- | Whether the user can use __sudo__ .
    allowSudo :: Lude.Maybe Lude.Bool,
    -- | A stack ID.
    stackId :: Lude.Maybe Lude.Text,
    -- | The user's permission level, which must be the following:
    --
    --
    --     * @deny@
    --
    --
    --     * @show@
    --
    --
    --     * @deploy@
    --
    --
    --     * @manage@
    --
    --
    --     * @iam_only@
    --
    --
    -- For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
    level :: Lude.Maybe Lude.Text,
    -- | Whether the user can use SSH.
    allowSSH :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- * 'iamUserARN' - The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
-- * 'allowSudo' - Whether the user can use __sudo__ .
-- * 'stackId' - A stack ID.
-- * 'level' - The user's permission level, which must be the following:
--
--
--     * @deny@
--
--
--     * @show@
--
--
--     * @deploy@
--
--
--     * @manage@
--
--
--     * @iam_only@
--
--
-- For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
-- * 'allowSSH' - Whether the user can use SSH.
mkPermission ::
  Permission
mkPermission =
  Permission'
    { iamUserARN = Lude.Nothing,
      allowSudo = Lude.Nothing,
      stackId = Lude.Nothing,
      level = Lude.Nothing,
      allowSSH = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIAMUserARN :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pIAMUserARN = Lens.lens (iamUserARN :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: Permission)
{-# DEPRECATED pIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | Whether the user can use __sudo__ .
--
-- /Note:/ Consider using 'allowSudo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowSudo :: Lens.Lens' Permission (Lude.Maybe Lude.Bool)
pAllowSudo = Lens.lens (allowSudo :: Permission -> Lude.Maybe Lude.Bool) (\s a -> s {allowSudo = a} :: Permission)
{-# DEPRECATED pAllowSudo "Use generic-lens or generic-optics with 'allowSudo' instead." #-}

-- | A stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStackId :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pStackId = Lens.lens (stackId :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Permission)
{-# DEPRECATED pStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The user's permission level, which must be the following:
--
--
--     * @deny@
--
--
--     * @show@
--
--
--     * @deploy@
--
--
--     * @manage@
--
--
--     * @iam_only@
--
--
-- For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLevel :: Lens.Lens' Permission (Lude.Maybe Lude.Text)
pLevel = Lens.lens (level :: Permission -> Lude.Maybe Lude.Text) (\s a -> s {level = a} :: Permission)
{-# DEPRECATED pLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | Whether the user can use SSH.
--
-- /Note:/ Consider using 'allowSSH' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowSSH :: Lens.Lens' Permission (Lude.Maybe Lude.Bool)
pAllowSSH = Lens.lens (allowSSH :: Permission -> Lude.Maybe Lude.Bool) (\s a -> s {allowSSH = a} :: Permission)
{-# DEPRECATED pAllowSSH "Use generic-lens or generic-optics with 'allowSSH' instead." #-}

instance Lude.FromJSON Permission where
  parseJSON =
    Lude.withObject
      "Permission"
      ( \x ->
          Permission'
            Lude.<$> (x Lude..:? "IamUserArn")
            Lude.<*> (x Lude..:? "AllowSudo")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "Level")
            Lude.<*> (x Lude..:? "AllowSsh")
      )
