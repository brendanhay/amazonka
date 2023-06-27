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
-- Module      : Amazonka.MQ.Types.LdapServerMetadataInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.LdapServerMetadataInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. The metadata of the LDAP server used to authenticate and
-- authorize connections to the broker.
--
-- Does not apply to RabbitMQ brokers.
--
-- /See:/ 'newLdapServerMetadataInput' smart constructor.
data LdapServerMetadataInput = LdapServerMetadataInput'
  { -- | Specifies the LDAP attribute that identifies the group name attribute in
    -- the object returned from the group membership query.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the role. If set to true, scope is to
    -- search the entire subtree.
    roleSearchSubtree :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the LDAP attribute for the user group membership.
    userRoleName :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the user. If set to true, scope is to
    -- search the entire subtree.
    userSearchSubtree :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the location of the LDAP server such as Directory Service for
    -- Microsoft Active Directory. Optional failover server.
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
    roleBase :: Prelude.Text,
    -- | Service account password. A service account is an account in your LDAP
    -- server that has access to initiate a connection. For example,
    -- cn=admin,dc=corp, dc=example, dc=com.
    serviceAccountPassword :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LdapServerMetadataInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'ldapServerMetadataInput_roleName' - Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
--
-- 'roleSearchSubtree', 'ldapServerMetadataInput_roleSearchSubtree' - The directory search scope for the role. If set to true, scope is to
-- search the entire subtree.
--
-- 'userRoleName', 'ldapServerMetadataInput_userRoleName' - Specifies the name of the LDAP attribute for the user group membership.
--
-- 'userSearchSubtree', 'ldapServerMetadataInput_userSearchSubtree' - The directory search scope for the user. If set to true, scope is to
-- search the entire subtree.
--
-- 'hosts', 'ldapServerMetadataInput_hosts' - Specifies the location of the LDAP server such as Directory Service for
-- Microsoft Active Directory. Optional failover server.
--
-- 'userSearchMatching', 'ldapServerMetadataInput_userSearchMatching' - The LDAP search filter used to find users within the userBase. The
-- client\'s username is substituted into the {0} placeholder in the search
-- filter. For example, if this option is set to (uid={0}) and the received
-- username is janedoe, the search filter becomes (uid=janedoe) after
-- string substitution. It will result in matching an entry like
-- uid=janedoe, ou=Users,ou=corp, dc=corp, dc=example, dc=com.
--
-- 'userBase', 'ldapServerMetadataInput_userBase' - Select a particular subtree of the directory information tree (DIT) to
-- search for user entries. The subtree is specified by a DN, which
-- specifies the base node of the subtree. For example, by setting this
-- option to ou=Users,ou=corp, dc=corp, dc=example, dc=com, the search for
-- user entries is restricted to the subtree beneath ou=Users, ou=corp,
-- dc=corp, dc=example, dc=com.
--
-- 'roleSearchMatching', 'ldapServerMetadataInput_roleSearchMatching' - The LDAP search filter used to find roles within the roleBase. The
-- distinguished name of the user matched by userSearchMatching is
-- substituted into the {0} placeholder in the search filter. The client\'s
-- username is substituted into the {1} placeholder. For example, if you
-- set this option to (member=uid={1})for the user janedoe, the search
-- filter becomes (member=uid=janedoe) after string substitution. It
-- matches all role entries that have a member attribute equal to
-- uid=janedoe under the subtree selected by the roleBase.
--
-- 'serviceAccountUsername', 'ldapServerMetadataInput_serviceAccountUsername' - Service account username. A service account is an account in your LDAP
-- server that has access to initiate a connection. For example,
-- cn=admin,dc=corp, dc=example, dc=com.
--
-- 'roleBase', 'ldapServerMetadataInput_roleBase' - The distinguished name of the node in the directory information tree
-- (DIT) to search for roles or groups. For example, ou=group, ou=corp,
-- dc=corp, dc=example, dc=com.
--
-- 'serviceAccountPassword', 'ldapServerMetadataInput_serviceAccountPassword' - Service account password. A service account is an account in your LDAP
-- server that has access to initiate a connection. For example,
-- cn=admin,dc=corp, dc=example, dc=com.
newLdapServerMetadataInput ::
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
  -- | 'serviceAccountPassword'
  Prelude.Text ->
  LdapServerMetadataInput
newLdapServerMetadataInput
  pUserSearchMatching_
  pUserBase_
  pRoleSearchMatching_
  pServiceAccountUsername_
  pRoleBase_
  pServiceAccountPassword_ =
    LdapServerMetadataInput'
      { roleName =
          Prelude.Nothing,
        roleSearchSubtree = Prelude.Nothing,
        userRoleName = Prelude.Nothing,
        userSearchSubtree = Prelude.Nothing,
        hosts = Prelude.mempty,
        userSearchMatching = pUserSearchMatching_,
        userBase = pUserBase_,
        roleSearchMatching = pRoleSearchMatching_,
        serviceAccountUsername = pServiceAccountUsername_,
        roleBase = pRoleBase_,
        serviceAccountPassword = pServiceAccountPassword_
      }

-- | Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
ldapServerMetadataInput_roleName :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_roleName = Lens.lens (\LdapServerMetadataInput' {roleName} -> roleName) (\s@LdapServerMetadataInput' {} a -> s {roleName = a} :: LdapServerMetadataInput)

-- | The directory search scope for the role. If set to true, scope is to
-- search the entire subtree.
ldapServerMetadataInput_roleSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Bool)
ldapServerMetadataInput_roleSearchSubtree = Lens.lens (\LdapServerMetadataInput' {roleSearchSubtree} -> roleSearchSubtree) (\s@LdapServerMetadataInput' {} a -> s {roleSearchSubtree = a} :: LdapServerMetadataInput)

-- | Specifies the name of the LDAP attribute for the user group membership.
ldapServerMetadataInput_userRoleName :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_userRoleName = Lens.lens (\LdapServerMetadataInput' {userRoleName} -> userRoleName) (\s@LdapServerMetadataInput' {} a -> s {userRoleName = a} :: LdapServerMetadataInput)

-- | The directory search scope for the user. If set to true, scope is to
-- search the entire subtree.
ldapServerMetadataInput_userSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Bool)
ldapServerMetadataInput_userSearchSubtree = Lens.lens (\LdapServerMetadataInput' {userSearchSubtree} -> userSearchSubtree) (\s@LdapServerMetadataInput' {} a -> s {userSearchSubtree = a} :: LdapServerMetadataInput)

-- | Specifies the location of the LDAP server such as Directory Service for
-- Microsoft Active Directory. Optional failover server.
ldapServerMetadataInput_hosts :: Lens.Lens' LdapServerMetadataInput [Prelude.Text]
ldapServerMetadataInput_hosts = Lens.lens (\LdapServerMetadataInput' {hosts} -> hosts) (\s@LdapServerMetadataInput' {} a -> s {hosts = a} :: LdapServerMetadataInput) Prelude.. Lens.coerced

-- | The LDAP search filter used to find users within the userBase. The
-- client\'s username is substituted into the {0} placeholder in the search
-- filter. For example, if this option is set to (uid={0}) and the received
-- username is janedoe, the search filter becomes (uid=janedoe) after
-- string substitution. It will result in matching an entry like
-- uid=janedoe, ou=Users,ou=corp, dc=corp, dc=example, dc=com.
ldapServerMetadataInput_userSearchMatching :: Lens.Lens' LdapServerMetadataInput Prelude.Text
ldapServerMetadataInput_userSearchMatching = Lens.lens (\LdapServerMetadataInput' {userSearchMatching} -> userSearchMatching) (\s@LdapServerMetadataInput' {} a -> s {userSearchMatching = a} :: LdapServerMetadataInput)

-- | Select a particular subtree of the directory information tree (DIT) to
-- search for user entries. The subtree is specified by a DN, which
-- specifies the base node of the subtree. For example, by setting this
-- option to ou=Users,ou=corp, dc=corp, dc=example, dc=com, the search for
-- user entries is restricted to the subtree beneath ou=Users, ou=corp,
-- dc=corp, dc=example, dc=com.
ldapServerMetadataInput_userBase :: Lens.Lens' LdapServerMetadataInput Prelude.Text
ldapServerMetadataInput_userBase = Lens.lens (\LdapServerMetadataInput' {userBase} -> userBase) (\s@LdapServerMetadataInput' {} a -> s {userBase = a} :: LdapServerMetadataInput)

-- | The LDAP search filter used to find roles within the roleBase. The
-- distinguished name of the user matched by userSearchMatching is
-- substituted into the {0} placeholder in the search filter. The client\'s
-- username is substituted into the {1} placeholder. For example, if you
-- set this option to (member=uid={1})for the user janedoe, the search
-- filter becomes (member=uid=janedoe) after string substitution. It
-- matches all role entries that have a member attribute equal to
-- uid=janedoe under the subtree selected by the roleBase.
ldapServerMetadataInput_roleSearchMatching :: Lens.Lens' LdapServerMetadataInput Prelude.Text
ldapServerMetadataInput_roleSearchMatching = Lens.lens (\LdapServerMetadataInput' {roleSearchMatching} -> roleSearchMatching) (\s@LdapServerMetadataInput' {} a -> s {roleSearchMatching = a} :: LdapServerMetadataInput)

-- | Service account username. A service account is an account in your LDAP
-- server that has access to initiate a connection. For example,
-- cn=admin,dc=corp, dc=example, dc=com.
ldapServerMetadataInput_serviceAccountUsername :: Lens.Lens' LdapServerMetadataInput Prelude.Text
ldapServerMetadataInput_serviceAccountUsername = Lens.lens (\LdapServerMetadataInput' {serviceAccountUsername} -> serviceAccountUsername) (\s@LdapServerMetadataInput' {} a -> s {serviceAccountUsername = a} :: LdapServerMetadataInput)

-- | The distinguished name of the node in the directory information tree
-- (DIT) to search for roles or groups. For example, ou=group, ou=corp,
-- dc=corp, dc=example, dc=com.
ldapServerMetadataInput_roleBase :: Lens.Lens' LdapServerMetadataInput Prelude.Text
ldapServerMetadataInput_roleBase = Lens.lens (\LdapServerMetadataInput' {roleBase} -> roleBase) (\s@LdapServerMetadataInput' {} a -> s {roleBase = a} :: LdapServerMetadataInput)

-- | Service account password. A service account is an account in your LDAP
-- server that has access to initiate a connection. For example,
-- cn=admin,dc=corp, dc=example, dc=com.
ldapServerMetadataInput_serviceAccountPassword :: Lens.Lens' LdapServerMetadataInput Prelude.Text
ldapServerMetadataInput_serviceAccountPassword = Lens.lens (\LdapServerMetadataInput' {serviceAccountPassword} -> serviceAccountPassword) (\s@LdapServerMetadataInput' {} a -> s {serviceAccountPassword = a} :: LdapServerMetadataInput)

instance Prelude.Hashable LdapServerMetadataInput where
  hashWithSalt _salt LdapServerMetadataInput' {..} =
    _salt
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` roleSearchSubtree
      `Prelude.hashWithSalt` userRoleName
      `Prelude.hashWithSalt` userSearchSubtree
      `Prelude.hashWithSalt` hosts
      `Prelude.hashWithSalt` userSearchMatching
      `Prelude.hashWithSalt` userBase
      `Prelude.hashWithSalt` roleSearchMatching
      `Prelude.hashWithSalt` serviceAccountUsername
      `Prelude.hashWithSalt` roleBase
      `Prelude.hashWithSalt` serviceAccountPassword

instance Prelude.NFData LdapServerMetadataInput where
  rnf LdapServerMetadataInput' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf roleSearchSubtree
      `Prelude.seq` Prelude.rnf userRoleName
      `Prelude.seq` Prelude.rnf userSearchSubtree
      `Prelude.seq` Prelude.rnf hosts
      `Prelude.seq` Prelude.rnf userSearchMatching
      `Prelude.seq` Prelude.rnf userBase
      `Prelude.seq` Prelude.rnf roleSearchMatching
      `Prelude.seq` Prelude.rnf serviceAccountUsername
      `Prelude.seq` Prelude.rnf roleBase
      `Prelude.seq` Prelude.rnf serviceAccountPassword

instance Data.ToJSON LdapServerMetadataInput where
  toJSON LdapServerMetadataInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleName" Data..=) Prelude.<$> roleName,
            ("roleSearchSubtree" Data..=)
              Prelude.<$> roleSearchSubtree,
            ("userRoleName" Data..=) Prelude.<$> userRoleName,
            ("userSearchSubtree" Data..=)
              Prelude.<$> userSearchSubtree,
            Prelude.Just ("hosts" Data..= hosts),
            Prelude.Just
              ("userSearchMatching" Data..= userSearchMatching),
            Prelude.Just ("userBase" Data..= userBase),
            Prelude.Just
              ("roleSearchMatching" Data..= roleSearchMatching),
            Prelude.Just
              ( "serviceAccountUsername"
                  Data..= serviceAccountUsername
              ),
            Prelude.Just ("roleBase" Data..= roleBase),
            Prelude.Just
              ( "serviceAccountPassword"
                  Data..= serviceAccountPassword
              )
          ]
      )
