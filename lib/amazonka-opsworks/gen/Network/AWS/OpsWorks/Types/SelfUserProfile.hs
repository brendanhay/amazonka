{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SelfUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SelfUserProfile
  ( SelfUserProfile (..),

    -- * Smart constructor
    mkSelfUserProfile,

    -- * Lenses
    supSSHPublicKey,
    supSSHUsername,
    supIAMUserARN,
    supName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a user's SSH information.
--
-- /See:/ 'mkSelfUserProfile' smart constructor.
data SelfUserProfile = SelfUserProfile'
  { -- | The user's SSH public key.
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

-- | Creates a value of 'SelfUserProfile' with the minimum fields required to make a request.
--
-- * 'sshPublicKey' - The user's SSH public key.
-- * 'sshUsername' - The user's SSH user name.
-- * 'iamUserARN' - The user's IAM ARN.
-- * 'name' - The user's name.
mkSelfUserProfile ::
  SelfUserProfile
mkSelfUserProfile =
  SelfUserProfile'
    { sshPublicKey = Lude.Nothing,
      sshUsername = Lude.Nothing,
      iamUserARN = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The user's SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supSSHPublicKey :: Lens.Lens' SelfUserProfile (Lude.Maybe Lude.Text)
supSSHPublicKey = Lens.lens (sshPublicKey :: SelfUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: SelfUserProfile)
{-# DEPRECATED supSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supSSHUsername :: Lens.Lens' SelfUserProfile (Lude.Maybe Lude.Text)
supSSHUsername = Lens.lens (sshUsername :: SelfUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshUsername = a} :: SelfUserProfile)
{-# DEPRECATED supSSHUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supIAMUserARN :: Lens.Lens' SelfUserProfile (Lude.Maybe Lude.Text)
supIAMUserARN = Lens.lens (iamUserARN :: SelfUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: SelfUserProfile)
{-# DEPRECATED supIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The user's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supName :: Lens.Lens' SelfUserProfile (Lude.Maybe Lude.Text)
supName = Lens.lens (name :: SelfUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SelfUserProfile)
{-# DEPRECATED supName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON SelfUserProfile where
  parseJSON =
    Lude.withObject
      "SelfUserProfile"
      ( \x ->
          SelfUserProfile'
            Lude.<$> (x Lude..:? "SshPublicKey")
            Lude.<*> (x Lude..:? "SshUsername")
            Lude.<*> (x Lude..:? "IamUserArn")
            Lude.<*> (x Lude..:? "Name")
      )
