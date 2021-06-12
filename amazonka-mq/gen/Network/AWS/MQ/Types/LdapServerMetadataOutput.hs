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
-- Module      : Network.AWS.MQ.Types.LdapServerMetadataOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LdapServerMetadataOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- /See:/ 'newLdapServerMetadataOutput' smart constructor.
data LdapServerMetadataOutput = LdapServerMetadataOutput'
  { -- | Fully qualified name of the directory where you want to search for
    -- users.
    userBase :: Core.Maybe Core.Text,
    -- | The search criteria for users.
    userSearchMatching :: Core.Maybe Core.Text,
    -- | Specifies the LDAP attribute that identifies the group name attribute in
    -- the object returned from the group membership query.
    roleName :: Core.Maybe Core.Text,
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
-- Create a value of 'LdapServerMetadataOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userBase', 'ldapServerMetadataOutput_userBase' - Fully qualified name of the directory where you want to search for
-- users.
--
-- 'userSearchMatching', 'ldapServerMetadataOutput_userSearchMatching' - The search criteria for users.
--
-- 'roleName', 'ldapServerMetadataOutput_roleName' - Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
--
-- 'userSearchSubtree', 'ldapServerMetadataOutput_userSearchSubtree' - The directory search scope for the user. If set to true, scope is to
-- search the entire sub-tree.
--
-- 'serviceAccountUsername', 'ldapServerMetadataOutput_serviceAccountUsername' - Service account username.
--
-- 'userRoleName', 'ldapServerMetadataOutput_userRoleName' - Specifies the name of the LDAP attribute for the user group membership.
--
-- 'roleBase', 'ldapServerMetadataOutput_roleBase' - Fully qualified name of the directory to search for a user’s groups.
--
-- 'roleSearchMatching', 'ldapServerMetadataOutput_roleSearchMatching' - The search criteria for groups.
--
-- 'hosts', 'ldapServerMetadataOutput_hosts' - Fully qualified domain name of the LDAP server. Optional failover
-- server.
--
-- 'roleSearchSubtree', 'ldapServerMetadataOutput_roleSearchSubtree' - The directory search scope for the role. If set to true, scope is to
-- search the entire sub-tree.
newLdapServerMetadataOutput ::
  LdapServerMetadataOutput
newLdapServerMetadataOutput =
  LdapServerMetadataOutput'
    { userBase = Core.Nothing,
      userSearchMatching = Core.Nothing,
      roleName = Core.Nothing,
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
ldapServerMetadataOutput_userBase :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_userBase = Lens.lens (\LdapServerMetadataOutput' {userBase} -> userBase) (\s@LdapServerMetadataOutput' {} a -> s {userBase = a} :: LdapServerMetadataOutput)

-- | The search criteria for users.
ldapServerMetadataOutput_userSearchMatching :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_userSearchMatching = Lens.lens (\LdapServerMetadataOutput' {userSearchMatching} -> userSearchMatching) (\s@LdapServerMetadataOutput' {} a -> s {userSearchMatching = a} :: LdapServerMetadataOutput)

-- | Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
ldapServerMetadataOutput_roleName :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_roleName = Lens.lens (\LdapServerMetadataOutput' {roleName} -> roleName) (\s@LdapServerMetadataOutput' {} a -> s {roleName = a} :: LdapServerMetadataOutput)

-- | The directory search scope for the user. If set to true, scope is to
-- search the entire sub-tree.
ldapServerMetadataOutput_userSearchSubtree :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Bool)
ldapServerMetadataOutput_userSearchSubtree = Lens.lens (\LdapServerMetadataOutput' {userSearchSubtree} -> userSearchSubtree) (\s@LdapServerMetadataOutput' {} a -> s {userSearchSubtree = a} :: LdapServerMetadataOutput)

-- | Service account username.
ldapServerMetadataOutput_serviceAccountUsername :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_serviceAccountUsername = Lens.lens (\LdapServerMetadataOutput' {serviceAccountUsername} -> serviceAccountUsername) (\s@LdapServerMetadataOutput' {} a -> s {serviceAccountUsername = a} :: LdapServerMetadataOutput)

-- | Specifies the name of the LDAP attribute for the user group membership.
ldapServerMetadataOutput_userRoleName :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_userRoleName = Lens.lens (\LdapServerMetadataOutput' {userRoleName} -> userRoleName) (\s@LdapServerMetadataOutput' {} a -> s {userRoleName = a} :: LdapServerMetadataOutput)

-- | Fully qualified name of the directory to search for a user’s groups.
ldapServerMetadataOutput_roleBase :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_roleBase = Lens.lens (\LdapServerMetadataOutput' {roleBase} -> roleBase) (\s@LdapServerMetadataOutput' {} a -> s {roleBase = a} :: LdapServerMetadataOutput)

-- | The search criteria for groups.
ldapServerMetadataOutput_roleSearchMatching :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Text)
ldapServerMetadataOutput_roleSearchMatching = Lens.lens (\LdapServerMetadataOutput' {roleSearchMatching} -> roleSearchMatching) (\s@LdapServerMetadataOutput' {} a -> s {roleSearchMatching = a} :: LdapServerMetadataOutput)

-- | Fully qualified domain name of the LDAP server. Optional failover
-- server.
ldapServerMetadataOutput_hosts :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe [Core.Text])
ldapServerMetadataOutput_hosts = Lens.lens (\LdapServerMetadataOutput' {hosts} -> hosts) (\s@LdapServerMetadataOutput' {} a -> s {hosts = a} :: LdapServerMetadataOutput) Core.. Lens.mapping Lens._Coerce

-- | The directory search scope for the role. If set to true, scope is to
-- search the entire sub-tree.
ldapServerMetadataOutput_roleSearchSubtree :: Lens.Lens' LdapServerMetadataOutput (Core.Maybe Core.Bool)
ldapServerMetadataOutput_roleSearchSubtree = Lens.lens (\LdapServerMetadataOutput' {roleSearchSubtree} -> roleSearchSubtree) (\s@LdapServerMetadataOutput' {} a -> s {roleSearchSubtree = a} :: LdapServerMetadataOutput)

instance Core.FromJSON LdapServerMetadataOutput where
  parseJSON =
    Core.withObject
      "LdapServerMetadataOutput"
      ( \x ->
          LdapServerMetadataOutput'
            Core.<$> (x Core..:? "userBase")
            Core.<*> (x Core..:? "userSearchMatching")
            Core.<*> (x Core..:? "roleName")
            Core.<*> (x Core..:? "userSearchSubtree")
            Core.<*> (x Core..:? "serviceAccountUsername")
            Core.<*> (x Core..:? "userRoleName")
            Core.<*> (x Core..:? "roleBase")
            Core.<*> (x Core..:? "roleSearchMatching")
            Core.<*> (x Core..:? "hosts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "roleSearchSubtree")
      )

instance Core.Hashable LdapServerMetadataOutput

instance Core.NFData LdapServerMetadataOutput
