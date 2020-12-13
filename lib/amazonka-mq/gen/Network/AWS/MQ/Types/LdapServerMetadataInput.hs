{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.LdapServerMetadataInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LdapServerMetadataInput
  ( LdapServerMetadataInput (..),

    -- * Smart constructor
    mkLdapServerMetadataInput,

    -- * Lenses
    lsmiUserBase,
    lsmiUserSearchMatching,
    lsmiUserRoleName,
    lsmiServiceAccountUsername,
    lsmiUserSearchSubtree,
    lsmiRoleSearchSubtree,
    lsmiHosts,
    lsmiRoleName,
    lsmiServiceAccountPassword,
    lsmiRoleSearchMatching,
    lsmiRoleBase,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /See:/ 'mkLdapServerMetadataInput' smart constructor.
data LdapServerMetadataInput = LdapServerMetadataInput'
  { -- | Fully qualified name of the directory where you want to search for users.
    userBase :: Lude.Maybe Lude.Text,
    -- | The search criteria for users.
    userSearchMatching :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the LDAP attribute for the user group membership.
    userRoleName :: Lude.Maybe Lude.Text,
    -- | Service account username.
    serviceAccountUsername :: Lude.Maybe Lude.Text,
    -- | The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
    userSearchSubtree :: Lude.Maybe Lude.Bool,
    -- | The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
    roleSearchSubtree :: Lude.Maybe Lude.Bool,
    -- | Fully qualified domain name of the LDAP server. Optional failover server.
    hosts :: Lude.Maybe [Lude.Text],
    -- | Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
    roleName :: Lude.Maybe Lude.Text,
    -- | Service account password.
    serviceAccountPassword :: Lude.Maybe Lude.Text,
    -- | The search criteria for groups.
    roleSearchMatching :: Lude.Maybe Lude.Text,
    -- | Fully qualified name of the directory to search for a user’s groups.
    roleBase :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LdapServerMetadataInput' with the minimum fields required to make a request.
--
-- * 'userBase' - Fully qualified name of the directory where you want to search for users.
-- * 'userSearchMatching' - The search criteria for users.
-- * 'userRoleName' - Specifies the name of the LDAP attribute for the user group membership.
-- * 'serviceAccountUsername' - Service account username.
-- * 'userSearchSubtree' - The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
-- * 'roleSearchSubtree' - The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
-- * 'hosts' - Fully qualified domain name of the LDAP server. Optional failover server.
-- * 'roleName' - Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
-- * 'serviceAccountPassword' - Service account password.
-- * 'roleSearchMatching' - The search criteria for groups.
-- * 'roleBase' - Fully qualified name of the directory to search for a user’s groups.
mkLdapServerMetadataInput ::
  LdapServerMetadataInput
mkLdapServerMetadataInput =
  LdapServerMetadataInput'
    { userBase = Lude.Nothing,
      userSearchMatching = Lude.Nothing,
      userRoleName = Lude.Nothing,
      serviceAccountUsername = Lude.Nothing,
      userSearchSubtree = Lude.Nothing,
      roleSearchSubtree = Lude.Nothing,
      hosts = Lude.Nothing,
      roleName = Lude.Nothing,
      serviceAccountPassword = Lude.Nothing,
      roleSearchMatching = Lude.Nothing,
      roleBase = Lude.Nothing
    }

-- | Fully qualified name of the directory where you want to search for users.
--
-- /Note:/ Consider using 'userBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserBase :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiUserBase = Lens.lens (userBase :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {userBase = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiUserBase "Use generic-lens or generic-optics with 'userBase' instead." #-}

-- | The search criteria for users.
--
-- /Note:/ Consider using 'userSearchMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserSearchMatching :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiUserSearchMatching = Lens.lens (userSearchMatching :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {userSearchMatching = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiUserSearchMatching "Use generic-lens or generic-optics with 'userSearchMatching' instead." #-}

-- | Specifies the name of the LDAP attribute for the user group membership.
--
-- /Note:/ Consider using 'userRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserRoleName :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiUserRoleName = Lens.lens (userRoleName :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {userRoleName = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiUserRoleName "Use generic-lens or generic-optics with 'userRoleName' instead." #-}

-- | Service account username.
--
-- /Note:/ Consider using 'serviceAccountUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiServiceAccountUsername :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiServiceAccountUsername = Lens.lens (serviceAccountUsername :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccountUsername = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiServiceAccountUsername "Use generic-lens or generic-optics with 'serviceAccountUsername' instead." #-}

-- | The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
--
-- /Note:/ Consider using 'userSearchSubtree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Bool)
lsmiUserSearchSubtree = Lens.lens (userSearchSubtree :: LdapServerMetadataInput -> Lude.Maybe Lude.Bool) (\s a -> s {userSearchSubtree = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiUserSearchSubtree "Use generic-lens or generic-optics with 'userSearchSubtree' instead." #-}

-- | The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
--
-- /Note:/ Consider using 'roleSearchSubtree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Bool)
lsmiRoleSearchSubtree = Lens.lens (roleSearchSubtree :: LdapServerMetadataInput -> Lude.Maybe Lude.Bool) (\s a -> s {roleSearchSubtree = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiRoleSearchSubtree "Use generic-lens or generic-optics with 'roleSearchSubtree' instead." #-}

-- | Fully qualified domain name of the LDAP server. Optional failover server.
--
-- /Note:/ Consider using 'hosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiHosts :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe [Lude.Text])
lsmiHosts = Lens.lens (hosts :: LdapServerMetadataInput -> Lude.Maybe [Lude.Text]) (\s a -> s {hosts = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiHosts "Use generic-lens or generic-optics with 'hosts' instead." #-}

-- | Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleName :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiRoleName = Lens.lens (roleName :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Service account password.
--
-- /Note:/ Consider using 'serviceAccountPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiServiceAccountPassword :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiServiceAccountPassword = Lens.lens (serviceAccountPassword :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccountPassword = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiServiceAccountPassword "Use generic-lens or generic-optics with 'serviceAccountPassword' instead." #-}

-- | The search criteria for groups.
--
-- /Note:/ Consider using 'roleSearchMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleSearchMatching :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiRoleSearchMatching = Lens.lens (roleSearchMatching :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {roleSearchMatching = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiRoleSearchMatching "Use generic-lens or generic-optics with 'roleSearchMatching' instead." #-}

-- | Fully qualified name of the directory to search for a user’s groups.
--
-- /Note:/ Consider using 'roleBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleBase :: Lens.Lens' LdapServerMetadataInput (Lude.Maybe Lude.Text)
lsmiRoleBase = Lens.lens (roleBase :: LdapServerMetadataInput -> Lude.Maybe Lude.Text) (\s a -> s {roleBase = a} :: LdapServerMetadataInput)
{-# DEPRECATED lsmiRoleBase "Use generic-lens or generic-optics with 'roleBase' instead." #-}

instance Lude.ToJSON LdapServerMetadataInput where
  toJSON LdapServerMetadataInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("userBase" Lude..=) Lude.<$> userBase,
            ("userSearchMatching" Lude..=) Lude.<$> userSearchMatching,
            ("userRoleName" Lude..=) Lude.<$> userRoleName,
            ("serviceAccountUsername" Lude..=) Lude.<$> serviceAccountUsername,
            ("userSearchSubtree" Lude..=) Lude.<$> userSearchSubtree,
            ("roleSearchSubtree" Lude..=) Lude.<$> roleSearchSubtree,
            ("hosts" Lude..=) Lude.<$> hosts,
            ("roleName" Lude..=) Lude.<$> roleName,
            ("serviceAccountPassword" Lude..=) Lude.<$> serviceAccountPassword,
            ("roleSearchMatching" Lude..=) Lude.<$> roleSearchMatching,
            ("roleBase" Lude..=) Lude.<$> roleBase
          ]
      )
