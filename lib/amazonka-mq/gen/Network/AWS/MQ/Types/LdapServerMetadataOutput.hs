-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.LdapServerMetadataOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LdapServerMetadataOutput
  ( LdapServerMetadataOutput (..),

    -- * Smart constructor
    mkLdapServerMetadataOutput,

    -- * Lenses
    lsmoUserBase,
    lsmoUserSearchMatching,
    lsmoUserRoleName,
    lsmoServiceAccountUsername,
    lsmoUserSearchSubtree,
    lsmoRoleSearchSubtree,
    lsmoHosts,
    lsmoRoleName,
    lsmoRoleSearchMatching,
    lsmoRoleBase,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /See:/ 'mkLdapServerMetadataOutput' smart constructor.
data LdapServerMetadataOutput = LdapServerMetadataOutput'
  { userBase ::
      Lude.Maybe Lude.Text,
    userSearchMatching ::
      Lude.Maybe Lude.Text,
    userRoleName :: Lude.Maybe Lude.Text,
    serviceAccountUsername ::
      Lude.Maybe Lude.Text,
    userSearchSubtree :: Lude.Maybe Lude.Bool,
    roleSearchSubtree :: Lude.Maybe Lude.Bool,
    hosts :: Lude.Maybe [Lude.Text],
    roleName :: Lude.Maybe Lude.Text,
    roleSearchMatching ::
      Lude.Maybe Lude.Text,
    roleBase :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LdapServerMetadataOutput' with the minimum fields required to make a request.
--
-- * 'hosts' - Fully qualified domain name of the LDAP server. Optional failover server.
-- * 'roleBase' - Fully qualified name of the directory to search for a user’s groups.
-- * 'roleName' - Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
-- * 'roleSearchMatching' - The search criteria for groups.
-- * 'roleSearchSubtree' - The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
-- * 'serviceAccountUsername' - Service account username.
-- * 'userBase' - Fully qualified name of the directory where you want to search for users.
-- * 'userRoleName' - Specifies the name of the LDAP attribute for the user group membership.
-- * 'userSearchMatching' - The search criteria for users.
-- * 'userSearchSubtree' - The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
mkLdapServerMetadataOutput ::
  LdapServerMetadataOutput
mkLdapServerMetadataOutput =
  LdapServerMetadataOutput'
    { userBase = Lude.Nothing,
      userSearchMatching = Lude.Nothing,
      userRoleName = Lude.Nothing,
      serviceAccountUsername = Lude.Nothing,
      userSearchSubtree = Lude.Nothing,
      roleSearchSubtree = Lude.Nothing,
      hosts = Lude.Nothing,
      roleName = Lude.Nothing,
      roleSearchMatching = Lude.Nothing,
      roleBase = Lude.Nothing
    }

-- | Fully qualified name of the directory where you want to search for users.
--
-- /Note:/ Consider using 'userBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoUserBase :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoUserBase = Lens.lens (userBase :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {userBase = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoUserBase "Use generic-lens or generic-optics with 'userBase' instead." #-}

-- | The search criteria for users.
--
-- /Note:/ Consider using 'userSearchMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoUserSearchMatching :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoUserSearchMatching = Lens.lens (userSearchMatching :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {userSearchMatching = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoUserSearchMatching "Use generic-lens or generic-optics with 'userSearchMatching' instead." #-}

-- | Specifies the name of the LDAP attribute for the user group membership.
--
-- /Note:/ Consider using 'userRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoUserRoleName :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoUserRoleName = Lens.lens (userRoleName :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {userRoleName = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoUserRoleName "Use generic-lens or generic-optics with 'userRoleName' instead." #-}

-- | Service account username.
--
-- /Note:/ Consider using 'serviceAccountUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoServiceAccountUsername :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoServiceAccountUsername = Lens.lens (serviceAccountUsername :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccountUsername = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoServiceAccountUsername "Use generic-lens or generic-optics with 'serviceAccountUsername' instead." #-}

-- | The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
--
-- /Note:/ Consider using 'userSearchSubtree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoUserSearchSubtree :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Bool)
lsmoUserSearchSubtree = Lens.lens (userSearchSubtree :: LdapServerMetadataOutput -> Lude.Maybe Lude.Bool) (\s a -> s {userSearchSubtree = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoUserSearchSubtree "Use generic-lens or generic-optics with 'userSearchSubtree' instead." #-}

-- | The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
--
-- /Note:/ Consider using 'roleSearchSubtree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoRoleSearchSubtree :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Bool)
lsmoRoleSearchSubtree = Lens.lens (roleSearchSubtree :: LdapServerMetadataOutput -> Lude.Maybe Lude.Bool) (\s a -> s {roleSearchSubtree = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoRoleSearchSubtree "Use generic-lens or generic-optics with 'roleSearchSubtree' instead." #-}

-- | Fully qualified domain name of the LDAP server. Optional failover server.
--
-- /Note:/ Consider using 'hosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoHosts :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe [Lude.Text])
lsmoHosts = Lens.lens (hosts :: LdapServerMetadataOutput -> Lude.Maybe [Lude.Text]) (\s a -> s {hosts = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoHosts "Use generic-lens or generic-optics with 'hosts' instead." #-}

-- | Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoRoleName :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoRoleName = Lens.lens (roleName :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The search criteria for groups.
--
-- /Note:/ Consider using 'roleSearchMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoRoleSearchMatching :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoRoleSearchMatching = Lens.lens (roleSearchMatching :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {roleSearchMatching = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoRoleSearchMatching "Use generic-lens or generic-optics with 'roleSearchMatching' instead." #-}

-- | Fully qualified name of the directory to search for a user’s groups.
--
-- /Note:/ Consider using 'roleBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmoRoleBase :: Lens.Lens' LdapServerMetadataOutput (Lude.Maybe Lude.Text)
lsmoRoleBase = Lens.lens (roleBase :: LdapServerMetadataOutput -> Lude.Maybe Lude.Text) (\s a -> s {roleBase = a} :: LdapServerMetadataOutput)
{-# DEPRECATED lsmoRoleBase "Use generic-lens or generic-optics with 'roleBase' instead." #-}

instance Lude.FromJSON LdapServerMetadataOutput where
  parseJSON =
    Lude.withObject
      "LdapServerMetadataOutput"
      ( \x ->
          LdapServerMetadataOutput'
            Lude.<$> (x Lude..:? "userBase")
            Lude.<*> (x Lude..:? "userSearchMatching")
            Lude.<*> (x Lude..:? "userRoleName")
            Lude.<*> (x Lude..:? "serviceAccountUsername")
            Lude.<*> (x Lude..:? "userSearchSubtree")
            Lude.<*> (x Lude..:? "roleSearchSubtree")
            Lude.<*> (x Lude..:? "hosts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleName")
            Lude.<*> (x Lude..:? "roleSearchMatching")
            Lude.<*> (x Lude..:? "roleBase")
      )
