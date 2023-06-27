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
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLdapServerMetadataDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerLdapServerMetadataDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of the Lightweight Directory Access Protocol (LDAP) server
-- used to authenticate and authorize connections to the broker. This is an
-- optional failover server.
--
-- /See:/ 'newAwsAmazonMqBrokerLdapServerMetadataDetails' smart constructor.
data AwsAmazonMqBrokerLdapServerMetadataDetails = AwsAmazonMqBrokerLdapServerMetadataDetails'
  { -- | Specifies the location of the LDAP server, such as Amazon Web Services
    -- Directory Service for Microsoft Active Directory.
    hosts :: Prelude.Maybe [Prelude.Text],
    -- | The distinguished name of the node in the directory information tree
    -- (DIT) to search for roles or groups.
    roleBase :: Prelude.Maybe Prelude.Text,
    -- | The group name attribute in a role entry whose value is the name of that
    -- role.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The LDAP search filter used to find roles within the @roleBase@.
    roleSearchMatching :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the role. If set to @true@, the scope is
    -- to search the entire subtree.
    roleSearchSubtree :: Prelude.Maybe Prelude.Bool,
    -- | A username for the service account, which is an account in your LDAP
    -- server that has access to initiate a connection.
    serviceAccountUsername :: Prelude.Maybe Prelude.Text,
    -- | Selects a particular subtree of the directory information tree (DIT) to
    -- search for user entries.
    userBase :: Prelude.Maybe Prelude.Text,
    -- | The name of the LDAP attribute in the user\'s directory entry for the
    -- user\'s group membership.
    userRoleName :: Prelude.Maybe Prelude.Text,
    -- | The LDAP search filter used to find users within the @userBase@.
    userSearchMatching :: Prelude.Maybe Prelude.Text,
    -- | The directory search scope for the user. If set to true, the scope is to
    -- search the entire subtree.
    userSearchSubtree :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerLdapServerMetadataDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hosts', 'awsAmazonMqBrokerLdapServerMetadataDetails_hosts' - Specifies the location of the LDAP server, such as Amazon Web Services
-- Directory Service for Microsoft Active Directory.
--
-- 'roleBase', 'awsAmazonMqBrokerLdapServerMetadataDetails_roleBase' - The distinguished name of the node in the directory information tree
-- (DIT) to search for roles or groups.
--
-- 'roleName', 'awsAmazonMqBrokerLdapServerMetadataDetails_roleName' - The group name attribute in a role entry whose value is the name of that
-- role.
--
-- 'roleSearchMatching', 'awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchMatching' - The LDAP search filter used to find roles within the @roleBase@.
--
-- 'roleSearchSubtree', 'awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchSubtree' - The directory search scope for the role. If set to @true@, the scope is
-- to search the entire subtree.
--
-- 'serviceAccountUsername', 'awsAmazonMqBrokerLdapServerMetadataDetails_serviceAccountUsername' - A username for the service account, which is an account in your LDAP
-- server that has access to initiate a connection.
--
-- 'userBase', 'awsAmazonMqBrokerLdapServerMetadataDetails_userBase' - Selects a particular subtree of the directory information tree (DIT) to
-- search for user entries.
--
-- 'userRoleName', 'awsAmazonMqBrokerLdapServerMetadataDetails_userRoleName' - The name of the LDAP attribute in the user\'s directory entry for the
-- user\'s group membership.
--
-- 'userSearchMatching', 'awsAmazonMqBrokerLdapServerMetadataDetails_userSearchMatching' - The LDAP search filter used to find users within the @userBase@.
--
-- 'userSearchSubtree', 'awsAmazonMqBrokerLdapServerMetadataDetails_userSearchSubtree' - The directory search scope for the user. If set to true, the scope is to
-- search the entire subtree.
newAwsAmazonMqBrokerLdapServerMetadataDetails ::
  AwsAmazonMqBrokerLdapServerMetadataDetails
newAwsAmazonMqBrokerLdapServerMetadataDetails =
  AwsAmazonMqBrokerLdapServerMetadataDetails'
    { hosts =
        Prelude.Nothing,
      roleBase = Prelude.Nothing,
      roleName = Prelude.Nothing,
      roleSearchMatching =
        Prelude.Nothing,
      roleSearchSubtree =
        Prelude.Nothing,
      serviceAccountUsername =
        Prelude.Nothing,
      userBase = Prelude.Nothing,
      userRoleName = Prelude.Nothing,
      userSearchMatching =
        Prelude.Nothing,
      userSearchSubtree =
        Prelude.Nothing
    }

-- | Specifies the location of the LDAP server, such as Amazon Web Services
-- Directory Service for Microsoft Active Directory.
awsAmazonMqBrokerLdapServerMetadataDetails_hosts :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe [Prelude.Text])
awsAmazonMqBrokerLdapServerMetadataDetails_hosts = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {hosts} -> hosts) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {hosts = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails) Prelude.. Lens.mapping Lens.coerced

-- | The distinguished name of the node in the directory information tree
-- (DIT) to search for roles or groups.
awsAmazonMqBrokerLdapServerMetadataDetails_roleBase :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_roleBase = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {roleBase} -> roleBase) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {roleBase = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | The group name attribute in a role entry whose value is the name of that
-- role.
awsAmazonMqBrokerLdapServerMetadataDetails_roleName :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_roleName = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {roleName} -> roleName) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {roleName = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | The LDAP search filter used to find roles within the @roleBase@.
awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchMatching :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchMatching = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {roleSearchMatching} -> roleSearchMatching) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {roleSearchMatching = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | The directory search scope for the role. If set to @true@, the scope is
-- to search the entire subtree.
awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchSubtree :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerLdapServerMetadataDetails_roleSearchSubtree = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {roleSearchSubtree} -> roleSearchSubtree) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {roleSearchSubtree = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | A username for the service account, which is an account in your LDAP
-- server that has access to initiate a connection.
awsAmazonMqBrokerLdapServerMetadataDetails_serviceAccountUsername :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_serviceAccountUsername = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {serviceAccountUsername} -> serviceAccountUsername) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {serviceAccountUsername = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | Selects a particular subtree of the directory information tree (DIT) to
-- search for user entries.
awsAmazonMqBrokerLdapServerMetadataDetails_userBase :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_userBase = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {userBase} -> userBase) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {userBase = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | The name of the LDAP attribute in the user\'s directory entry for the
-- user\'s group membership.
awsAmazonMqBrokerLdapServerMetadataDetails_userRoleName :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_userRoleName = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {userRoleName} -> userRoleName) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {userRoleName = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | The LDAP search filter used to find users within the @userBase@.
awsAmazonMqBrokerLdapServerMetadataDetails_userSearchMatching :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerLdapServerMetadataDetails_userSearchMatching = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {userSearchMatching} -> userSearchMatching) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {userSearchMatching = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

-- | The directory search scope for the user. If set to true, the scope is to
-- search the entire subtree.
awsAmazonMqBrokerLdapServerMetadataDetails_userSearchSubtree :: Lens.Lens' AwsAmazonMqBrokerLdapServerMetadataDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerLdapServerMetadataDetails_userSearchSubtree = Lens.lens (\AwsAmazonMqBrokerLdapServerMetadataDetails' {userSearchSubtree} -> userSearchSubtree) (\s@AwsAmazonMqBrokerLdapServerMetadataDetails' {} a -> s {userSearchSubtree = a} :: AwsAmazonMqBrokerLdapServerMetadataDetails)

instance
  Data.FromJSON
    AwsAmazonMqBrokerLdapServerMetadataDetails
  where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerLdapServerMetadataDetails"
      ( \x ->
          AwsAmazonMqBrokerLdapServerMetadataDetails'
            Prelude.<$> (x Data..:? "Hosts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RoleBase")
            Prelude.<*> (x Data..:? "RoleName")
            Prelude.<*> (x Data..:? "RoleSearchMatching")
            Prelude.<*> (x Data..:? "RoleSearchSubtree")
            Prelude.<*> (x Data..:? "ServiceAccountUsername")
            Prelude.<*> (x Data..:? "UserBase")
            Prelude.<*> (x Data..:? "UserRoleName")
            Prelude.<*> (x Data..:? "UserSearchMatching")
            Prelude.<*> (x Data..:? "UserSearchSubtree")
      )

instance
  Prelude.Hashable
    AwsAmazonMqBrokerLdapServerMetadataDetails
  where
  hashWithSalt
    _salt
    AwsAmazonMqBrokerLdapServerMetadataDetails' {..} =
      _salt
        `Prelude.hashWithSalt` hosts
        `Prelude.hashWithSalt` roleBase
        `Prelude.hashWithSalt` roleName
        `Prelude.hashWithSalt` roleSearchMatching
        `Prelude.hashWithSalt` roleSearchSubtree
        `Prelude.hashWithSalt` serviceAccountUsername
        `Prelude.hashWithSalt` userBase
        `Prelude.hashWithSalt` userRoleName
        `Prelude.hashWithSalt` userSearchMatching
        `Prelude.hashWithSalt` userSearchSubtree

instance
  Prelude.NFData
    AwsAmazonMqBrokerLdapServerMetadataDetails
  where
  rnf AwsAmazonMqBrokerLdapServerMetadataDetails' {..} =
    Prelude.rnf hosts
      `Prelude.seq` Prelude.rnf roleBase
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf roleSearchMatching
      `Prelude.seq` Prelude.rnf roleSearchSubtree
      `Prelude.seq` Prelude.rnf serviceAccountUsername
      `Prelude.seq` Prelude.rnf userBase
      `Prelude.seq` Prelude.rnf userRoleName
      `Prelude.seq` Prelude.rnf userSearchMatching
      `Prelude.seq` Prelude.rnf userSearchSubtree

instance
  Data.ToJSON
    AwsAmazonMqBrokerLdapServerMetadataDetails
  where
  toJSON
    AwsAmazonMqBrokerLdapServerMetadataDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Hosts" Data..=) Prelude.<$> hosts,
              ("RoleBase" Data..=) Prelude.<$> roleBase,
              ("RoleName" Data..=) Prelude.<$> roleName,
              ("RoleSearchMatching" Data..=)
                Prelude.<$> roleSearchMatching,
              ("RoleSearchSubtree" Data..=)
                Prelude.<$> roleSearchSubtree,
              ("ServiceAccountUsername" Data..=)
                Prelude.<$> serviceAccountUsername,
              ("UserBase" Data..=) Prelude.<$> userBase,
              ("UserRoleName" Data..=) Prelude.<$> userRoleName,
              ("UserSearchMatching" Data..=)
                Prelude.<$> userSearchMatching,
              ("UserSearchSubtree" Data..=)
                Prelude.<$> userSearchSubtree
            ]
        )
