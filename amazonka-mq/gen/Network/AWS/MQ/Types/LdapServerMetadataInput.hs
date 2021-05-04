{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata of the LDAP server used to authenticate and authorize
-- connections to the broker.
--
-- /See:/ 'newLdapServerMetadataInput' smart constructor.
data LdapServerMetadataInput = LdapServerMetadataInput'
  { -- | Fully qualified name of the directory where you want to search for
    -- users.
    userBase :: Prelude.Maybe Prelude.Text,
    -- | The search criteria for users.
    userSearchMatching :: Prelude.Maybe Prelude.Text,
    -- | Specifies the LDAP attribute that identifies the group name attribute in
    -- the object returned from the group membership query.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Service account password.
    serviceAccountPassword :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the user. If set to true, scope is to
    -- search the entire sub-tree.
    userSearchSubtree :: Prelude.Maybe Prelude.Bool,
    -- | Service account username.
    serviceAccountUsername :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the LDAP attribute for the user group membership.
    userRoleName :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified name of the directory to search for a user’s groups.
    roleBase :: Prelude.Maybe Prelude.Text,
    -- | The search criteria for groups.
    roleSearchMatching :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name of the LDAP server. Optional failover
    -- server.
    hosts :: Prelude.Maybe [Prelude.Text],
    -- | The directory search scope for the role. If set to true, scope is to
    -- search the entire sub-tree.
    roleSearchSubtree :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { userBase =
        Prelude.Nothing,
      userSearchMatching = Prelude.Nothing,
      roleName = Prelude.Nothing,
      serviceAccountPassword = Prelude.Nothing,
      userSearchSubtree = Prelude.Nothing,
      serviceAccountUsername = Prelude.Nothing,
      userRoleName = Prelude.Nothing,
      roleBase = Prelude.Nothing,
      roleSearchMatching = Prelude.Nothing,
      hosts = Prelude.Nothing,
      roleSearchSubtree = Prelude.Nothing
    }

-- | Fully qualified name of the directory where you want to search for
-- users.
ldapServerMetadataInput_userBase :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_userBase = Lens.lens (\LdapServerMetadataInput' {userBase} -> userBase) (\s@LdapServerMetadataInput' {} a -> s {userBase = a} :: LdapServerMetadataInput)

-- | The search criteria for users.
ldapServerMetadataInput_userSearchMatching :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_userSearchMatching = Lens.lens (\LdapServerMetadataInput' {userSearchMatching} -> userSearchMatching) (\s@LdapServerMetadataInput' {} a -> s {userSearchMatching = a} :: LdapServerMetadataInput)

-- | Specifies the LDAP attribute that identifies the group name attribute in
-- the object returned from the group membership query.
ldapServerMetadataInput_roleName :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_roleName = Lens.lens (\LdapServerMetadataInput' {roleName} -> roleName) (\s@LdapServerMetadataInput' {} a -> s {roleName = a} :: LdapServerMetadataInput)

-- | Service account password.
ldapServerMetadataInput_serviceAccountPassword :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_serviceAccountPassword = Lens.lens (\LdapServerMetadataInput' {serviceAccountPassword} -> serviceAccountPassword) (\s@LdapServerMetadataInput' {} a -> s {serviceAccountPassword = a} :: LdapServerMetadataInput)

-- | The directory search scope for the user. If set to true, scope is to
-- search the entire sub-tree.
ldapServerMetadataInput_userSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Bool)
ldapServerMetadataInput_userSearchSubtree = Lens.lens (\LdapServerMetadataInput' {userSearchSubtree} -> userSearchSubtree) (\s@LdapServerMetadataInput' {} a -> s {userSearchSubtree = a} :: LdapServerMetadataInput)

-- | Service account username.
ldapServerMetadataInput_serviceAccountUsername :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_serviceAccountUsername = Lens.lens (\LdapServerMetadataInput' {serviceAccountUsername} -> serviceAccountUsername) (\s@LdapServerMetadataInput' {} a -> s {serviceAccountUsername = a} :: LdapServerMetadataInput)

-- | Specifies the name of the LDAP attribute for the user group membership.
ldapServerMetadataInput_userRoleName :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_userRoleName = Lens.lens (\LdapServerMetadataInput' {userRoleName} -> userRoleName) (\s@LdapServerMetadataInput' {} a -> s {userRoleName = a} :: LdapServerMetadataInput)

-- | Fully qualified name of the directory to search for a user’s groups.
ldapServerMetadataInput_roleBase :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_roleBase = Lens.lens (\LdapServerMetadataInput' {roleBase} -> roleBase) (\s@LdapServerMetadataInput' {} a -> s {roleBase = a} :: LdapServerMetadataInput)

-- | The search criteria for groups.
ldapServerMetadataInput_roleSearchMatching :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Text)
ldapServerMetadataInput_roleSearchMatching = Lens.lens (\LdapServerMetadataInput' {roleSearchMatching} -> roleSearchMatching) (\s@LdapServerMetadataInput' {} a -> s {roleSearchMatching = a} :: LdapServerMetadataInput)

-- | Fully qualified domain name of the LDAP server. Optional failover
-- server.
ldapServerMetadataInput_hosts :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe [Prelude.Text])
ldapServerMetadataInput_hosts = Lens.lens (\LdapServerMetadataInput' {hosts} -> hosts) (\s@LdapServerMetadataInput' {} a -> s {hosts = a} :: LdapServerMetadataInput) Prelude.. Lens.mapping Prelude._Coerce

-- | The directory search scope for the role. If set to true, scope is to
-- search the entire sub-tree.
ldapServerMetadataInput_roleSearchSubtree :: Lens.Lens' LdapServerMetadataInput (Prelude.Maybe Prelude.Bool)
ldapServerMetadataInput_roleSearchSubtree = Lens.lens (\LdapServerMetadataInput' {roleSearchSubtree} -> roleSearchSubtree) (\s@LdapServerMetadataInput' {} a -> s {roleSearchSubtree = a} :: LdapServerMetadataInput)

instance Prelude.Hashable LdapServerMetadataInput

instance Prelude.NFData LdapServerMetadataInput

instance Prelude.ToJSON LdapServerMetadataInput where
  toJSON LdapServerMetadataInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("userBase" Prelude..=) Prelude.<$> userBase,
            ("userSearchMatching" Prelude..=)
              Prelude.<$> userSearchMatching,
            ("roleName" Prelude..=) Prelude.<$> roleName,
            ("serviceAccountPassword" Prelude..=)
              Prelude.<$> serviceAccountPassword,
            ("userSearchSubtree" Prelude..=)
              Prelude.<$> userSearchSubtree,
            ("serviceAccountUsername" Prelude..=)
              Prelude.<$> serviceAccountUsername,
            ("userRoleName" Prelude..=) Prelude.<$> userRoleName,
            ("roleBase" Prelude..=) Prelude.<$> roleBase,
            ("roleSearchMatching" Prelude..=)
              Prelude.<$> roleSearchMatching,
            ("hosts" Prelude..=) Prelude.<$> hosts,
            ("roleSearchSubtree" Prelude..=)
              Prelude.<$> roleSearchSubtree
          ]
      )
