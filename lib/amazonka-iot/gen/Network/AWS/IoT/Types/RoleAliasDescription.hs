{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RoleAliasDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RoleAliasDescription
  ( RoleAliasDescription (..),

    -- * Smart constructor
    mkRoleAliasDescription,

    -- * Lenses
    radRoleAliasARN,
    radLastModifiedDate,
    radRoleAlias,
    radOwner,
    radCreationDate,
    radCredentialDurationSeconds,
    radRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Role alias description.
--
-- /See:/ 'mkRoleAliasDescription' smart constructor.
data RoleAliasDescription = RoleAliasDescription'
  { -- | The ARN of the role alias.
    roleAliasARN :: Lude.Maybe Lude.Text,
    -- | The UNIX timestamp of when the role alias was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The role alias.
    roleAlias :: Lude.Maybe Lude.Text,
    -- | The role alias owner.
    owner :: Lude.Maybe Lude.Text,
    -- | The UNIX timestamp of when the role alias was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The number of seconds for which the credential is valid.
    credentialDurationSeconds :: Lude.Maybe Lude.Natural,
    -- | The role ARN.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoleAliasDescription' with the minimum fields required to make a request.
--
-- * 'roleAliasARN' - The ARN of the role alias.
-- * 'lastModifiedDate' - The UNIX timestamp of when the role alias was last modified.
-- * 'roleAlias' - The role alias.
-- * 'owner' - The role alias owner.
-- * 'creationDate' - The UNIX timestamp of when the role alias was created.
-- * 'credentialDurationSeconds' - The number of seconds for which the credential is valid.
-- * 'roleARN' - The role ARN.
mkRoleAliasDescription ::
  RoleAliasDescription
mkRoleAliasDescription =
  RoleAliasDescription'
    { roleAliasARN = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      roleAlias = Lude.Nothing,
      owner = Lude.Nothing,
      creationDate = Lude.Nothing,
      credentialDurationSeconds = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The ARN of the role alias.
--
-- /Note:/ Consider using 'roleAliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radRoleAliasARN :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Text)
radRoleAliasARN = Lens.lens (roleAliasARN :: RoleAliasDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleAliasARN = a} :: RoleAliasDescription)
{-# DEPRECATED radRoleAliasARN "Use generic-lens or generic-optics with 'roleAliasARN' instead." #-}

-- | The UNIX timestamp of when the role alias was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radLastModifiedDate :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Timestamp)
radLastModifiedDate = Lens.lens (lastModifiedDate :: RoleAliasDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: RoleAliasDescription)
{-# DEPRECATED radLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radRoleAlias :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Text)
radRoleAlias = Lens.lens (roleAlias :: RoleAliasDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleAlias = a} :: RoleAliasDescription)
{-# DEPRECATED radRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The role alias owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radOwner :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Text)
radOwner = Lens.lens (owner :: RoleAliasDescription -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: RoleAliasDescription)
{-# DEPRECATED radOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The UNIX timestamp of when the role alias was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radCreationDate :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Timestamp)
radCreationDate = Lens.lens (creationDate :: RoleAliasDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: RoleAliasDescription)
{-# DEPRECATED radCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The number of seconds for which the credential is valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radCredentialDurationSeconds :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Natural)
radCredentialDurationSeconds = Lens.lens (credentialDurationSeconds :: RoleAliasDescription -> Lude.Maybe Lude.Natural) (\s a -> s {credentialDurationSeconds = a} :: RoleAliasDescription)
{-# DEPRECATED radCredentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead." #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
radRoleARN :: Lens.Lens' RoleAliasDescription (Lude.Maybe Lude.Text)
radRoleARN = Lens.lens (roleARN :: RoleAliasDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: RoleAliasDescription)
{-# DEPRECATED radRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON RoleAliasDescription where
  parseJSON =
    Lude.withObject
      "RoleAliasDescription"
      ( \x ->
          RoleAliasDescription'
            Lude.<$> (x Lude..:? "roleAliasArn")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "roleAlias")
            Lude.<*> (x Lude..:? "owner")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "credentialDurationSeconds")
            Lude.<*> (x Lude..:? "roleArn")
      )
