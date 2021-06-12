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
-- Module      : Network.AWS.MQ.Types.LdapServerMetadataInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LdapServerMetadataInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- /See:/ 'newLdapServerMetadataInput' smart constructor.
data LdapServerMetadataInput = LdapServerMetadataInput'
  { -- | Fully qualified name of the directory where you want to search for
    -- users.
    userBase :: Core.Maybe Core.Text,
    -- | The search criteria for users.
    userSearchMatching :: Core.Maybe Core.Text,
    -- | Specifies the LDAP attribute that identifies the group name attribute in
    -- the object returned from the group membership query.
    roleName :: Core.Maybe Core.Text,
    -- | Service account password.
    serviceAccountPassword :: Core.Maybe Core.Text,
    -- | The directory search scope for the user. If set to true, scope is to
    -- search the entire sub-tree.
    userSearchSubtree :: Core.Maybe Core.Bool,
    -- | Service account username.
    serviceAccountUsername :: Core.Maybe Core.Text,
    -- | Specifies the name of the LDAP attribute for the user group membership.
    userRoleName :: Core.Maybe Core.Text,
    -- | Fully qualified name of the directory to search for a user’s groups.
    roleBase :: Core.Maybe Core.Text,
    -- | The search criteria for groups.
    roleSearchMatching :: Core.Maybe Core.Text,
    -- | Fully qualified domain name of the LDAP server. Optional failover
    -- server.
    hosts :: Core.Maybe [Core.Text],
    -- | The directory search scope for the role. If set to true, scope is to
    -- search the entire sub-tree.
    roleSearchSubtree :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LdapServerMetadataInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userBase', 'ldapServerMetadataInput_userBase' - Fully qualified name of the directory where you want to search for
-- users.
--
-- 'userSearchMatching', 'ldapServerMetadataInput_userSearchMatching' - The search criteria for users.
--
-- 'roleName', 'ldapServerMetadataInput_roleName' - Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
--
-- 'serviceAccountPassword', 'ldapServerMetadataInput_serviceAccountPassword' - Service account password.
--
-- 'userSearchSubtree', 'ldapServerMetadataInput_userSearchSubtree' - The directory search scope for the user. If set to true, scope is to
-- search the entire sub-tree.
--
-- 'serviceAccountUsername', 'ldapServerMetadataInput_serviceAccountUsername' - Service account username.
--
-- 'userRoleName', 'ldapServerMetadataInput_userRoleName' - Specifies the name of the LDAP attribute for the user group membership.
--
-- 'roleBase', 'ldapServerMetadataInput_roleBase' - Fully qualified name of the directory to search for a user’s groups.
--
-- 'roleSearchMatching', 'ldapServerMetadataInput_roleSearchMatching' - The search criteria for groups.
--
-- 'hosts', 'ldapServerMetadataInput_hosts' - Fully qualified domain name of the LDAP server. Optional failover
-- server.
--
-- 'roleSearchSubtree', 'ldapServerMetadataInput_roleSearchSubtree' - The directory search scope for the role. If set to true, scope is to
-- search the entire sub-tree.
newLdapServerMetadataInput ::
  LdapServerMetadataInput
newLdapServerMetadataInput =
  LdapServerMetadataInput'
    { userBase = Core.Nothing,
      userSearchMatching = Core.Nothing,
      roleName = Core.Nothing,
      serviceAccountPassword = Core.Nothing,
      userSearchSubtree = Core.Nothing,
      serviceAccountUsername = Core.Nothing,
      userRoleName = Core.Nothing,
      roleBase = Core.Nothing,
      roleSearchMatching = Core.Nothing,
      hosts = Core.Nothing,
      roleSearchSubtree = Core.Nothing
    }

-- | Fully qualified name of the directory where you want to search for
-- users.
ldapServerMetadataInput_userBase :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_userBase = Lens.lens (\LdapServerMetadataInput' {userBase} -> userBase) (\s@LdapServerMetadataInput' {} a -> s {userBase = a} :: LdapServerMetadataInput)

-- | The search criteria for users.
ldapServerMetadataInput_userSearchMatching :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_userSearchMatching = Lens.lens (\LdapServerMetadataInput' {userSearchMatching} -> userSearchMatching) (\s@LdapServerMetadataInput' {} a -> s {userSearchMatching = a} :: LdapServerMetadataInput)

-- | Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
ldapServerMetadataInput_roleName :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_roleName = Lens.lens (\LdapServerMetadataInput' {roleName} -> roleName) (\s@LdapServerMetadataInput' {} a -> s {roleName = a} :: LdapServerMetadataInput)

-- | Service account password.
ldapServerMetadataInput_serviceAccountPassword :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_serviceAccountPassword = Lens.lens (\LdapServerMetadataInput' {serviceAccountPassword} -> serviceAccountPassword) (\s@LdapServerMetadataInput' {} a -> s {serviceAccountPassword = a} :: LdapServerMetadataInput)

-- | The directory search scope for the user. If set to true, scope is to
-- search the entire sub-tree.
ldapServerMetadataInput_userSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Bool)
ldapServerMetadataInput_userSearchSubtree = Lens.lens (\LdapServerMetadataInput' {userSearchSubtree} -> userSearchSubtree) (\s@LdapServerMetadataInput' {} a -> s {userSearchSubtree = a} :: LdapServerMetadataInput)

-- | Service account username.
ldapServerMetadataInput_serviceAccountUsername :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_serviceAccountUsername = Lens.lens (\LdapServerMetadataInput' {serviceAccountUsername} -> serviceAccountUsername) (\s@LdapServerMetadataInput' {} a -> s {serviceAccountUsername = a} :: LdapServerMetadataInput)

-- | Specifies the name of the LDAP attribute for the user group membership.
ldapServerMetadataInput_userRoleName :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_userRoleName = Lens.lens (\LdapServerMetadataInput' {userRoleName} -> userRoleName) (\s@LdapServerMetadataInput' {} a -> s {userRoleName = a} :: LdapServerMetadataInput)

-- | Fully qualified name of the directory to search for a user’s groups.
ldapServerMetadataInput_roleBase :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_roleBase = Lens.lens (\LdapServerMetadataInput' {roleBase} -> roleBase) (\s@LdapServerMetadataInput' {} a -> s {roleBase = a} :: LdapServerMetadataInput)

-- | The search criteria for groups.
ldapServerMetadataInput_roleSearchMatching :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Text)
ldapServerMetadataInput_roleSearchMatching = Lens.lens (\LdapServerMetadataInput' {roleSearchMatching} -> roleSearchMatching) (\s@LdapServerMetadataInput' {} a -> s {roleSearchMatching = a} :: LdapServerMetadataInput)

-- | Fully qualified domain name of the LDAP server. Optional failover
-- server.
ldapServerMetadataInput_hosts :: Lens.Lens' LdapServerMetadataInput (Core.Maybe [Core.Text])
ldapServerMetadataInput_hosts = Lens.lens (\LdapServerMetadataInput' {hosts} -> hosts) (\s@LdapServerMetadataInput' {} a -> s {hosts = a} :: LdapServerMetadataInput) Core.. Lens.mapping Lens._Coerce

-- | The directory search scope for the role. If set to true, scope is to
-- search the entire sub-tree.
ldapServerMetadataInput_roleSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Core.Maybe Core.Bool)
ldapServerMetadataInput_roleSearchSubtree = Lens.lens (\LdapServerMetadataInput' {roleSearchSubtree} -> roleSearchSubtree) (\s@LdapServerMetadataInput' {} a -> s {roleSearchSubtree = a} :: LdapServerMetadataInput)

instance Core.Hashable LdapServerMetadataInput

instance Core.NFData LdapServerMetadataInput

instance Core.ToJSON LdapServerMetadataInput where
  toJSON LdapServerMetadataInput' {..} =
    Core.object
      ( Core.catMaybes
          [ ("userBase" Core..=) Core.<$> userBase,
            ("userSearchMatching" Core..=)
              Core.<$> userSearchMatching,
            ("roleName" Core..=) Core.<$> roleName,
            ("serviceAccountPassword" Core..=)
              Core.<$> serviceAccountPassword,
            ("userSearchSubtree" Core..=)
              Core.<$> userSearchSubtree,
            ("serviceAccountUsername" Core..=)
              Core.<$> serviceAccountUsername,
            ("userRoleName" Core..=) Core.<$> userRoleName,
            ("roleBase" Core..=) Core.<$> roleBase,
            ("roleSearchMatching" Core..=)
              Core.<$> roleSearchMatching,
            ("hosts" Core..=) Core.<$> hosts,
            ("roleSearchSubtree" Core..=)
              Core.<$> roleSearchSubtree
          ]
      )
