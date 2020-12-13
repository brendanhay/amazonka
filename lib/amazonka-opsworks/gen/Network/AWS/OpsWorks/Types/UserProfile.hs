{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.UserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.UserProfile
  ( UserProfile (..),

    -- * Smart constructor
    mkUserProfile,

    -- * Lenses
    upAllowSelfManagement,
    upSSHPublicKey,
    upSSHUsername,
    upIAMUserARN,
    upName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a user's SSH information.
--
-- /See:/ 'mkUserProfile' smart constructor.
data UserProfile = UserProfile'
  { -- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
    allowSelfManagement :: Lude.Maybe Lude.Bool,
    -- | The user's SSH public key.
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The user's SSH user name.
    sshUsername :: Lude.Maybe Lude.Text,
    -- | The user's IAM ARN.
    iamUserARN :: Lude.Maybe Lude.Text,
    -- | The user's name.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserProfile' with the minimum fields required to make a request.
--
-- * 'allowSelfManagement' - Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
-- * 'sshPublicKey' - The user's SSH public key.
-- * 'sshUsername' - The user's SSH user name.
-- * 'iamUserARN' - The user's IAM ARN.
-- * 'name' - The user's name.
mkUserProfile ::
  UserProfile
mkUserProfile =
  UserProfile'
    { allowSelfManagement = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      sshUsername = Lude.Nothing,
      iamUserARN = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
--
-- /Note:/ Consider using 'allowSelfManagement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAllowSelfManagement :: Lens.Lens' UserProfile (Lude.Maybe Lude.Bool)
upAllowSelfManagement = Lens.lens (allowSelfManagement :: UserProfile -> Lude.Maybe Lude.Bool) (\s a -> s {allowSelfManagement = a} :: UserProfile)
{-# DEPRECATED upAllowSelfManagement "Use generic-lens or generic-optics with 'allowSelfManagement' instead." #-}

-- | The user's SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSHPublicKey :: Lens.Lens' UserProfile (Lude.Maybe Lude.Text)
upSSHPublicKey = Lens.lens (sshPublicKey :: UserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: UserProfile)
{-# DEPRECATED upSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSSHUsername :: Lens.Lens' UserProfile (Lude.Maybe Lude.Text)
upSSHUsername = Lens.lens (sshUsername :: UserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshUsername = a} :: UserProfile)
{-# DEPRECATED upSSHUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upIAMUserARN :: Lens.Lens' UserProfile (Lude.Maybe Lude.Text)
upIAMUserARN = Lens.lens (iamUserARN :: UserProfile -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: UserProfile)
{-# DEPRECATED upIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The user's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UserProfile (Lude.Maybe Lude.Text)
upName = Lens.lens (name :: UserProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UserProfile)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON UserProfile where
  parseJSON =
    Lude.withObject
      "UserProfile"
      ( \x ->
          UserProfile'
            Lude.<$> (x Lude..:? "AllowSelfManagement")
            Lude.<*> (x Lude..:? "SshPublicKey")
            Lude.<*> (x Lude..:? "SshUsername")
            Lude.<*> (x Lude..:? "IamUserArn")
            Lude.<*> (x Lude..:? "Name")
      )
