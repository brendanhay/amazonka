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
    lsmiHosts,
    lsmiRoleBase,
    lsmiRoleName,
    lsmiRoleSearchMatching,
    lsmiRoleSearchSubtree,
    lsmiServiceAccountPassword,
    lsmiServiceAccountUsername,
    lsmiUserBase,
    lsmiUserRoleName,
    lsmiUserSearchMatching,
    lsmiUserSearchSubtree,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /See:/ 'mkLdapServerMetadataInput' smart constructor.
data LdapServerMetadataInput = LdapServerMetadataInput'
  { -- | Fully qualified domain name of the LDAP server. Optional failover server.
    hosts :: Core.Maybe [Core.Text],
    -- | Fully qualified name of the directory to search for a user’s groups.
    roleBase :: Core.Maybe Core.Text,
    -- | Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
    roleName :: Core.Maybe Core.Text,
    -- | The search criteria for groups.
    roleSearchMatching :: Core.Maybe Core.Text,
    -- | The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
    roleSearchSubtree :: Core.Maybe Core.Bool,
    -- | Service account password.
    serviceAccountPassword :: Core.Maybe Core.Text,
    -- | Service account username.
    serviceAccountUsername :: Core.Maybe Core.Text,
    -- | Fully qualified name of the directory where you want to search for users.
    userBase :: Core.Maybe Core.Text,
    -- | Specifies the name of the LDAP attribute for the user group membership.
    userRoleName :: Core.Maybe Core.Text,
    -- | The search criteria for users.
    userSearchMatching :: Core.Maybe Core.Text,
    -- | The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
    userSearchSubtree :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LdapServerMetadataInput' value with any optional fields omitted.
mkLdapServerMetadataInput ::
  LdapServerMetadataInput
mkLdapServerMetadataInput =
  LdapServerMetadataInput'
    { hosts = Core.Nothing,
      roleBase = Core.Nothing,
      roleName = Core.Nothing,
      roleSearchMatching = Core.Nothing,
      roleSearchSubtree = Core.Nothing,
      serviceAccountPassword = Core.Nothing,
      serviceAccountUsername = Core.Nothing,
      userBase = Core.Nothing,
      userRoleName = Core.Nothing,
      userSearchMatching = Core.Nothing,
      userSearchSubtree = Core.Nothing
    }

-- | Fully qualified domain name of the LDAP server. Optional failover server.
--
-- /Note:/ Consider using 'hosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiHosts :: Lens.Lens' LdapServerMetadataInput (Core.Maybe [Core.Text])
lsmiHosts = Lens.field @"hosts"
{-# DEPRECATED lsmiHosts "Use generic-lens or generic-optics with 'hosts' instead." #-}

-- | Fully qualified name of the directory to search for a user’s groups.
--
-- /Note:/ Consider using 'roleBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleBase :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiRoleBase = Lens.field @"roleBase"
{-# DEPRECATED lsmiRoleBase "Use generic-lens or generic-optics with 'roleBase' instead." #-}

-- | Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleName :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiRoleName = Lens.field @"roleName"
{-# DEPRECATED lsmiRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The search criteria for groups.
--
-- /Note:/ Consider using 'roleSearchMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleSearchMatching :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiRoleSearchMatching = Lens.field @"roleSearchMatching"
{-# DEPRECATED lsmiRoleSearchMatching "Use generic-lens or generic-optics with 'roleSearchMatching' instead." #-}

-- | The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
--
-- /Note:/ Consider using 'roleSearchSubtree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiRoleSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Bool)
lsmiRoleSearchSubtree = Lens.field @"roleSearchSubtree"
{-# DEPRECATED lsmiRoleSearchSubtree "Use generic-lens or generic-optics with 'roleSearchSubtree' instead." #-}

-- | Service account password.
--
-- /Note:/ Consider using 'serviceAccountPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiServiceAccountPassword :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiServiceAccountPassword = Lens.field @"serviceAccountPassword"
{-# DEPRECATED lsmiServiceAccountPassword "Use generic-lens or generic-optics with 'serviceAccountPassword' instead." #-}

-- | Service account username.
--
-- /Note:/ Consider using 'serviceAccountUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiServiceAccountUsername :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiServiceAccountUsername = Lens.field @"serviceAccountUsername"
{-# DEPRECATED lsmiServiceAccountUsername "Use generic-lens or generic-optics with 'serviceAccountUsername' instead." #-}

-- | Fully qualified name of the directory where you want to search for users.
--
-- /Note:/ Consider using 'userBase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserBase :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiUserBase = Lens.field @"userBase"
{-# DEPRECATED lsmiUserBase "Use generic-lens or generic-optics with 'userBase' instead." #-}

-- | Specifies the name of the LDAP attribute for the user group membership.
--
-- /Note:/ Consider using 'userRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserRoleName :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiUserRoleName = Lens.field @"userRoleName"
{-# DEPRECATED lsmiUserRoleName "Use generic-lens or generic-optics with 'userRoleName' instead." #-}

-- | The search criteria for users.
--
-- /Note:/ Consider using 'userSearchMatching' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserSearchMatching :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
lsmiUserSearchMatching = Lens.field @"userSearchMatching"
{-# DEPRECATED lsmiUserSearchMatching "Use generic-lens or generic-optics with 'userSearchMatching' instead." #-}

-- | The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
--
-- /Note:/ Consider using 'userSearchSubtree' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsmiUserSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Bool)
lsmiUserSearchSubtree = Lens.field @"userSearchSubtree"
{-# DEPRECATED lsmiUserSearchSubtree "Use generic-lens or generic-optics with 'userSearchSubtree' instead." #-}

instance Core.FromJSON LdapServerMetadataInput where
  toJSON LdapServerMetadataInput {..} =
    Core.object
      ( Core.catMaybes
          [ ("hosts" Core..=) Core.<$> hosts,
            ("roleBase" Core..=) Core.<$> roleBase,
            ("roleName" Core..=) Core.<$> roleName,
            ("roleSearchMatching" Core..=) Core.<$> roleSearchMatching,
            ("roleSearchSubtree" Core..=) Core.<$> roleSearchSubtree,
            ("serviceAccountPassword" Core..=) Core.<$> serviceAccountPassword,
            ("serviceAccountUsername" Core..=) Core.<$> serviceAccountUsername,
            ("userBase" Core..=) Core.<$> userBase,
            ("userRoleName" Core..=) Core.<$> userRoleName,
            ("userSearchMatching" Core..=) Core.<$> userSearchMatching,
            ("userSearchSubtree" Core..=) Core.<$> userSearchSubtree
          ]
      )
