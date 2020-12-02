{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.LdapServerMetadataOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.LdapServerMetadataOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata of the LDAP server used to authenticate and authorize connections to the broker.
--
-- /See:/ 'ldapServerMetadataOutput' smart constructor.
data LdapServerMetadataOutput = LdapServerMetadataOutput'
  { _lsmoUserBase ::
      !(Maybe Text),
    _lsmoUserSearchMatching :: !(Maybe Text),
    _lsmoUserRoleName :: !(Maybe Text),
    _lsmoServiceAccountUsername ::
      !(Maybe Text),
    _lsmoUserSearchSubtree :: !(Maybe Bool),
    _lsmoRoleSearchSubtree :: !(Maybe Bool),
    _lsmoHosts :: !(Maybe [Text]),
    _lsmoRoleName :: !(Maybe Text),
    _lsmoRoleSearchMatching :: !(Maybe Text),
    _lsmoRoleBase :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LdapServerMetadataOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsmoUserBase' - Fully qualified name of the directory where you want to search for users.
--
-- * 'lsmoUserSearchMatching' - The search criteria for users.
--
-- * 'lsmoUserRoleName' - Specifies the name of the LDAP attribute for the user group membership.
--
-- * 'lsmoServiceAccountUsername' - Service account username.
--
-- * 'lsmoUserSearchSubtree' - The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
--
-- * 'lsmoRoleSearchSubtree' - The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
--
-- * 'lsmoHosts' - Fully qualified domain name of the LDAP server. Optional failover server.
--
-- * 'lsmoRoleName' - Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
--
-- * 'lsmoRoleSearchMatching' - The search criteria for groups.
--
-- * 'lsmoRoleBase' - Fully qualified name of the directory to search for a user’s groups.
ldapServerMetadataOutput ::
  LdapServerMetadataOutput
ldapServerMetadataOutput =
  LdapServerMetadataOutput'
    { _lsmoUserBase = Nothing,
      _lsmoUserSearchMatching = Nothing,
      _lsmoUserRoleName = Nothing,
      _lsmoServiceAccountUsername = Nothing,
      _lsmoUserSearchSubtree = Nothing,
      _lsmoRoleSearchSubtree = Nothing,
      _lsmoHosts = Nothing,
      _lsmoRoleName = Nothing,
      _lsmoRoleSearchMatching = Nothing,
      _lsmoRoleBase = Nothing
    }

-- | Fully qualified name of the directory where you want to search for users.
lsmoUserBase :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoUserBase = lens _lsmoUserBase (\s a -> s {_lsmoUserBase = a})

-- | The search criteria for users.
lsmoUserSearchMatching :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoUserSearchMatching = lens _lsmoUserSearchMatching (\s a -> s {_lsmoUserSearchMatching = a})

-- | Specifies the name of the LDAP attribute for the user group membership.
lsmoUserRoleName :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoUserRoleName = lens _lsmoUserRoleName (\s a -> s {_lsmoUserRoleName = a})

-- | Service account username.
lsmoServiceAccountUsername :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoServiceAccountUsername = lens _lsmoServiceAccountUsername (\s a -> s {_lsmoServiceAccountUsername = a})

-- | The directory search scope for the user. If set to true, scope is to search the entire sub-tree.
lsmoUserSearchSubtree :: Lens' LdapServerMetadataOutput (Maybe Bool)
lsmoUserSearchSubtree = lens _lsmoUserSearchSubtree (\s a -> s {_lsmoUserSearchSubtree = a})

-- | The directory search scope for the role. If set to true, scope is to search the entire sub-tree.
lsmoRoleSearchSubtree :: Lens' LdapServerMetadataOutput (Maybe Bool)
lsmoRoleSearchSubtree = lens _lsmoRoleSearchSubtree (\s a -> s {_lsmoRoleSearchSubtree = a})

-- | Fully qualified domain name of the LDAP server. Optional failover server.
lsmoHosts :: Lens' LdapServerMetadataOutput [Text]
lsmoHosts = lens _lsmoHosts (\s a -> s {_lsmoHosts = a}) . _Default . _Coerce

-- | Specifies the LDAP attribute that identifies the group name attribute in the object returned from the group membership query.
lsmoRoleName :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoRoleName = lens _lsmoRoleName (\s a -> s {_lsmoRoleName = a})

-- | The search criteria for groups.
lsmoRoleSearchMatching :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoRoleSearchMatching = lens _lsmoRoleSearchMatching (\s a -> s {_lsmoRoleSearchMatching = a})

-- | Fully qualified name of the directory to search for a user’s groups.
lsmoRoleBase :: Lens' LdapServerMetadataOutput (Maybe Text)
lsmoRoleBase = lens _lsmoRoleBase (\s a -> s {_lsmoRoleBase = a})

instance FromJSON LdapServerMetadataOutput where
  parseJSON =
    withObject
      "LdapServerMetadataOutput"
      ( \x ->
          LdapServerMetadataOutput'
            <$> (x .:? "userBase")
            <*> (x .:? "userSearchMatching")
            <*> (x .:? "userRoleName")
            <*> (x .:? "serviceAccountUsername")
            <*> (x .:? "userSearchSubtree")
            <*> (x .:? "roleSearchSubtree")
            <*> (x .:? "hosts" .!= mempty)
            <*> (x .:? "roleName")
            <*> (x .:? "roleSearchMatching")
            <*> (x .:? "roleBase")
      )

instance Hashable LdapServerMetadataOutput

instance NFData LdapServerMetadataOutput
