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
-- Module      : Amazonka.MQ.Types.LdapServerMetadataOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.LdapServerMetadataOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker.
--
-- /See:/ 'newLdapServerMetadataOutput' smart constructor.
data LdapServerMetadataOutput = LdapServerMetadataOutput'
  { -- | Specifies the LDAP attribute that identifies the group name attribute in
    -- the object returned from the group membership query.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the user. If set to true, scope is to
    -- search the entire subtree.
    userSearchSubtree :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the LDAP attribute for the user group membership.
    userRoleName :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the role. If set to true, scope is to
    -- search the entire subtree.
    roleSearchSubtree :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the location of the LDAP server such as AWS Directory Service
    -- for Microsoft Active Directory . Optional failover server.
    hosts :: [Prelude.Text],
    -- | The LDAP search filter used to find users within the userBase. The
    -- client\'s username is substituted into the {0} placeholder in the search
    -- filter. For example, if this option is set to (uid={0}) and the received
    -- username is janedoe, the search filter becomes (uid=janedoe) after
    -- string substitution. It will result in matching an entry like
    -- uid=janedoe, ou=Users,ou=corp, dc=corp, dc=example, dc=com.
    userSearchMatching :: Prelude.Text,
    -- | Select a particular subtree of the directory information tree (DIT) to
    -- search for user entries. The subtree is specified by a DN, which
    -- specifies the base node of the subtree. For example, by setting this
    -- option to ou=Users,ou=corp, dc=corp, dc=example, dc=com, the search for
    -- user entries is restricted to the subtree beneath ou=Users, ou=corp,
    -- dc=corp, dc=example, dc=com.
    userBase :: Prelude.Text,
    -- | The LDAP search filter used to find roles within the roleBase. The
    -- distinguished name of the user matched by userSearchMatching is
    -- substituted into the {0} placeholder in the search filter. The client\'s
    -- username is substituted into the {1} placeholder. For example, if you
    -- set this option to (member=uid={1})for the user janedoe, the search
    -- filter becomes (member=uid=janedoe) after string substitution. It
    -- matches all role entries that have a member attribute equal to
    -- uid=janedoe under the subtree selected by the roleBase.
    roleSearchMatching :: Prelude.Text,
    -- | Service account username. A service account is an account in your LDAP
    -- server that has access to initiate a connection. For example,
    -- cn=admin,dc=corp, dc=example, dc=com.
    serviceAccountUsername :: Prelude.Text,
    -- | The distinguished name of the node in the directory information tree
    -- (DIT) to search for roles or groups. For example, ou=group, ou=corp,
    -- dc=corp, dc=example, dc=com.
    roleBase :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LdapServerMetadataOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'ldapServerMetadataOutput_roleName' - Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
--
-- 'userSearchSubtree', 'ldapServerMetadataOutput_userSearchSubtree' - The directory search scope for the user. If set to true, scope is to
-- search the entire subtree.
--
-- 'userRoleName', 'ldapServerMetadataOutput_userRoleName' - Specifies the name of the LDAP attribute for the user group membership.
--
-- 'roleSearchSubtree', 'ldapServerMetadataOutput_roleSearchSubtree' - The directory search scope for the role. If set to true, scope is to
-- search the entire subtree.
--
-- 'hosts', 'ldapServerMetadataOutput_hosts' - Specifies the location of the LDAP server such as AWS Directory Service
-- for Microsoft Active Directory . Optional failover server.
--
-- 'userSearchMatching', 'ldapServerMetadataOutput_userSearchMatching' - The LDAP search filter used to find users within the userBase. The
-- client\'s username is substituted into the {0} placeholder in the search
-- filter. For example, if this option is set to (uid={0}) and the received
-- username is janedoe, the search filter becomes (uid=janedoe) after
-- string substitution. It will result in matching an entry like
-- uid=janedoe, ou=Users,ou=corp, dc=corp, dc=example, dc=com.
--
-- 'userBase', 'ldapServerMetadataOutput_userBase' - Select a particular subtree of the directory information tree (DIT) to
-- search for user entries. The subtree is specified by a DN, which
-- specifies the base node of the subtree. For example, by setting this
-- option to ou=Users,ou=corp, dc=corp, dc=example, dc=com, the search for
-- user entries is restricted to the subtree beneath ou=Users, ou=corp,
-- dc=corp, dc=example, dc=com.
--
-- 'roleSearchMatching', 'ldapServerMetadataOutput_roleSearchMatching' - The LDAP search filter used to find roles within the roleBase. The
-- distinguished name of the user matched by userSearchMatching is
-- substituted into the {0} placeholder in the search filter. The client\'s
-- username is substituted into the {1} placeholder. For example, if you
-- set this option to (member=uid={1})for the user janedoe, the search
-- filter becomes (member=uid=janedoe) after string substitution. It
-- matches all role entries that have a member attribute equal to
-- uid=janedoe under the subtree selected by the roleBase.
--
-- 'serviceAccountUsername', 'ldapServerMetadataOutput_serviceAccountUsername' - Service account username. A service account is an account in your LDAP
-- server that has access to initiate a connection. For example,
-- cn=admin,dc=corp, dc=example, dc=com.
--
-- 'roleBase', 'ldapServerMetadataOutput_roleBase' - The distinguished name of the node in the directory information tree
-- (DIT) to search for roles or groups. For example, ou=group, ou=corp,
-- dc=corp, dc=example, dc=com.
newLdapServerMetadataOutput ::
  -- | 'userSearchMatching'
  Prelude.Text ->
  -- | 'userBase'
  Prelude.Text ->
  -- | 'roleSearchMatching'
  Prelude.Text ->
  -- | 'serviceAccountUsername'
  Prelude.Text ->
  -- | 'roleBase'
  Prelude.Text ->
  LdapServerMetadataOutput
newLdapServerMetadataOutput
  pUserSearchMatching_
  pUserBase_
  pRoleSearchMatching_
  pServiceAccountUsername_
  pRoleBase_ =
    LdapServerMetadataOutput'
      { roleName =
          Prelude.Nothing,
        userSearchSubtree = Prelude.Nothing,
        userRoleName = Prelude.Nothing,
        roleSearchSubtree = Prelude.Nothing,
        hosts = Prelude.mempty,
        userSearchMatching = pUserSearchMatching_,
        userBase = pUserBase_,
        roleSearchMatching = pRoleSearchMatching_,
        serviceAccountUsername = pServiceAccountUsername_,
        roleBase = pRoleBase_
      }

-- | Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
ldapServerMetadataOutput_roleName :: Lens.Lens' LdapServerMetadataOutput (Prelude.Maybe Prelude.Text)
ldapServerMetadataOutput_roleName = Lens.lens (\LdapServerMetadataOutput' {roleName} -> roleName) (\s@LdapServerMetadataOutput' {} a -> s {roleName = a} :: LdapServerMetadataOutput)

-- | The directory search scope for the user. If set to true, scope is to
-- search the entire subtree.
ldapServerMetadataOutput_userSearchSubtree :: Lens.Lens' LdapServerMetadataOutput (Prelude.Maybe Prelude.Bool)
ldapServerMetadataOutput_userSearchSubtree = Lens.lens (\LdapServerMetadataOutput' {userSearchSubtree} -> userSearchSubtree) (\s@LdapServerMetadataOutput' {} a -> s {userSearchSubtree = a} :: LdapServerMetadataOutput)

-- | Specifies the name of the LDAP attribute for the user group membership.
ldapServerMetadataOutput_userRoleName :: Lens.Lens' LdapServerMetadataOutput (Prelude.Maybe Prelude.Text)
ldapServerMetadataOutput_userRoleName = Lens.lens (\LdapServerMetadataOutput' {userRoleName} -> userRoleName) (\s@LdapServerMetadataOutput' {} a -> s {userRoleName = a} :: LdapServerMetadataOutput)

-- | The directory search scope for the role. If set to true, scope is to
-- search the entire subtree.
ldapServerMetadataOutput_roleSearchSubtree :: Lens.Lens' LdapServerMetadataOutput (Prelude.Maybe Prelude.Bool)
ldapServerMetadataOutput_roleSearchSubtree = Lens.lens (\LdapServerMetadataOutput' {roleSearchSubtree} -> roleSearchSubtree) (\s@LdapServerMetadataOutput' {} a -> s {roleSearchSubtree = a} :: LdapServerMetadataOutput)

-- | Specifies the location of the LDAP server such as AWS Directory Service
-- for Microsoft Active Directory . Optional failover server.
ldapServerMetadataOutput_hosts :: Lens.Lens' LdapServerMetadataOutput [Prelude.Text]
ldapServerMetadataOutput_hosts = Lens.lens (\LdapServerMetadataOutput' {hosts} -> hosts) (\s@LdapServerMetadataOutput' {} a -> s {hosts = a} :: LdapServerMetadataOutput) Prelude.. Lens.coerced

-- | The LDAP search filter used to find users within the userBase. The
-- client\'s username is substituted into the {0} placeholder in the search
-- filter. For example, if this option is set to (uid={0}) and the received
-- username is janedoe, the search filter becomes (uid=janedoe) after
-- string substitution. It will result in matching an entry like
-- uid=janedoe, ou=Users,ou=corp, dc=corp, dc=example, dc=com.
ldapServerMetadataOutput_userSearchMatching :: Lens.Lens' LdapServerMetadataOutput Prelude.Text
ldapServerMetadataOutput_userSearchMatching = Lens.lens (\LdapServerMetadataOutput' {userSearchMatching} -> userSearchMatching) (\s@LdapServerMetadataOutput' {} a -> s {userSearchMatching = a} :: LdapServerMetadataOutput)

-- | Select a particular subtree of the directory information tree (DIT) to
-- search for user entries. The subtree is specified by a DN, which
-- specifies the base node of the subtree. For example, by setting this
-- option to ou=Users,ou=corp, dc=corp, dc=example, dc=com, the search for
-- user entries is restricted to the subtree beneath ou=Users, ou=corp,
-- dc=corp, dc=example, dc=com.
ldapServerMetadataOutput_userBase :: Lens.Lens' LdapServerMetadataOutput Prelude.Text
ldapServerMetadataOutput_userBase = Lens.lens (\LdapServerMetadataOutput' {userBase} -> userBase) (\s@LdapServerMetadataOutput' {} a -> s {userBase = a} :: LdapServerMetadataOutput)

-- | The LDAP search filter used to find roles within the roleBase. The
-- distinguished name of the user matched by userSearchMatching is
-- substituted into the {0} placeholder in the search filter. The client\'s
-- username is substituted into the {1} placeholder. For example, if you
-- set this option to (member=uid={1})for the user janedoe, the search
-- filter becomes (member=uid=janedoe) after string substitution. It
-- matches all role entries that have a member attribute equal to
-- uid=janedoe under the subtree selected by the roleBase.
ldapServerMetadataOutput_roleSearchMatching :: Lens.Lens' LdapServerMetadataOutput Prelude.Text
ldapServerMetadataOutput_roleSearchMatching = Lens.lens (\LdapServerMetadataOutput' {roleSearchMatching} -> roleSearchMatching) (\s@LdapServerMetadataOutput' {} a -> s {roleSearchMatching = a} :: LdapServerMetadataOutput)

-- | Service account username. A service account is an account in your LDAP
-- server that has access to initiate a connection. For example,
-- cn=admin,dc=corp, dc=example, dc=com.
ldapServerMetadataOutput_serviceAccountUsername :: Lens.Lens' LdapServerMetadataOutput Prelude.Text
ldapServerMetadataOutput_serviceAccountUsername = Lens.lens (\LdapServerMetadataOutput' {serviceAccountUsername} -> serviceAccountUsername) (\s@LdapServerMetadataOutput' {} a -> s {serviceAccountUsername = a} :: LdapServerMetadataOutput)

-- | The distinguished name of the node in the directory information tree
-- (DIT) to search for roles or groups. For example, ou=group, ou=corp,
-- dc=corp, dc=example, dc=com.
ldapServerMetadataOutput_roleBase :: Lens.Lens' LdapServerMetadataOutput Prelude.Text
ldapServerMetadataOutput_roleBase = Lens.lens (\LdapServerMetadataOutput' {roleBase} -> roleBase) (\s@LdapServerMetadataOutput' {} a -> s {roleBase = a} :: LdapServerMetadataOutput)

instance Data.FromJSON LdapServerMetadataOutput where
  parseJSON =
    Data.withObject
      "LdapServerMetadataOutput"
      ( \x ->
          LdapServerMetadataOutput'
            Prelude.<$> (x Data..:? "roleName")
            Prelude.<*> (x Data..:? "userSearchSubtree")
            Prelude.<*> (x Data..:? "userRoleName")
            Prelude.<*> (x Data..:? "roleSearchSubtree")
            Prelude.<*> (x Data..:? "hosts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "userSearchMatching")
            Prelude.<*> (x Data..: "userBase")
            Prelude.<*> (x Data..: "roleSearchMatching")
            Prelude.<*> (x Data..: "serviceAccountUsername")
            Prelude.<*> (x Data..: "roleBase")
      )

instance Prelude.Hashable LdapServerMetadataOutput where
  hashWithSalt _salt LdapServerMetadataOutput' {..} =
    _salt `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` userSearchSubtree
      `Prelude.hashWithSalt` userRoleName
      `Prelude.hashWithSalt` roleSearchSubtree
      `Prelude.hashWithSalt` hosts
      `Prelude.hashWithSalt` userSearchMatching
      `Prelude.hashWithSalt` userBase
      `Prelude.hashWithSalt` roleSearchMatching
      `Prelude.hashWithSalt` serviceAccountUsername
      `Prelude.hashWithSalt` roleBase

instance Prelude.NFData LdapServerMetadataOutput where
  rnf LdapServerMetadataOutput' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf userSearchSubtree
      `Prelude.seq` Prelude.rnf userRoleName
      `Prelude.seq` Prelude.rnf roleSearchSubtree
      `Prelude.seq` Prelude.rnf hosts
      `Prelude.seq` Prelude.rnf userSearchMatching
      `Prelude.seq` Prelude.rnf userBase
      `Prelude.seq` Prelude.rnf roleSearchMatching
      `Prelude.seq` Prelude.rnf serviceAccountUsername
      `Prelude.seq` Prelude.rnf roleBase
