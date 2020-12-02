{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RoleAliasDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RoleAliasDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Role alias description.
--
--
--
-- /See:/ 'roleAliasDescription' smart constructor.
data RoleAliasDescription = RoleAliasDescription'
  { _radRoleAliasARN ::
      !(Maybe Text),
    _radLastModifiedDate :: !(Maybe POSIX),
    _radRoleAlias :: !(Maybe Text),
    _radOwner :: !(Maybe Text),
    _radCreationDate :: !(Maybe POSIX),
    _radCredentialDurationSeconds :: !(Maybe Nat),
    _radRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoleAliasDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'radRoleAliasARN' - The ARN of the role alias.
--
-- * 'radLastModifiedDate' - The UNIX timestamp of when the role alias was last modified.
--
-- * 'radRoleAlias' - The role alias.
--
-- * 'radOwner' - The role alias owner.
--
-- * 'radCreationDate' - The UNIX timestamp of when the role alias was created.
--
-- * 'radCredentialDurationSeconds' - The number of seconds for which the credential is valid.
--
-- * 'radRoleARN' - The role ARN.
roleAliasDescription ::
  RoleAliasDescription
roleAliasDescription =
  RoleAliasDescription'
    { _radRoleAliasARN = Nothing,
      _radLastModifiedDate = Nothing,
      _radRoleAlias = Nothing,
      _radOwner = Nothing,
      _radCreationDate = Nothing,
      _radCredentialDurationSeconds = Nothing,
      _radRoleARN = Nothing
    }

-- | The ARN of the role alias.
radRoleAliasARN :: Lens' RoleAliasDescription (Maybe Text)
radRoleAliasARN = lens _radRoleAliasARN (\s a -> s {_radRoleAliasARN = a})

-- | The UNIX timestamp of when the role alias was last modified.
radLastModifiedDate :: Lens' RoleAliasDescription (Maybe UTCTime)
radLastModifiedDate = lens _radLastModifiedDate (\s a -> s {_radLastModifiedDate = a}) . mapping _Time

-- | The role alias.
radRoleAlias :: Lens' RoleAliasDescription (Maybe Text)
radRoleAlias = lens _radRoleAlias (\s a -> s {_radRoleAlias = a})

-- | The role alias owner.
radOwner :: Lens' RoleAliasDescription (Maybe Text)
radOwner = lens _radOwner (\s a -> s {_radOwner = a})

-- | The UNIX timestamp of when the role alias was created.
radCreationDate :: Lens' RoleAliasDescription (Maybe UTCTime)
radCreationDate = lens _radCreationDate (\s a -> s {_radCreationDate = a}) . mapping _Time

-- | The number of seconds for which the credential is valid.
radCredentialDurationSeconds :: Lens' RoleAliasDescription (Maybe Natural)
radCredentialDurationSeconds = lens _radCredentialDurationSeconds (\s a -> s {_radCredentialDurationSeconds = a}) . mapping _Nat

-- | The role ARN.
radRoleARN :: Lens' RoleAliasDescription (Maybe Text)
radRoleARN = lens _radRoleARN (\s a -> s {_radRoleARN = a})

instance FromJSON RoleAliasDescription where
  parseJSON =
    withObject
      "RoleAliasDescription"
      ( \x ->
          RoleAliasDescription'
            <$> (x .:? "roleAliasArn")
            <*> (x .:? "lastModifiedDate")
            <*> (x .:? "roleAlias")
            <*> (x .:? "owner")
            <*> (x .:? "creationDate")
            <*> (x .:? "credentialDurationSeconds")
            <*> (x .:? "roleArn")
      )

instance Hashable RoleAliasDescription

instance NFData RoleAliasDescription
