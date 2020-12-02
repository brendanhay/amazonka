{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentMember where

import Network.AWS.Cloud9.Types.Permissions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an environment member for an AWS Cloud9 development environment.
--
--
--
-- /See:/ 'environmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { _emLastAccess ::
      !(Maybe POSIX),
    _emUserId :: !(Maybe Text),
    _emUserARN :: !(Maybe Text),
    _emPermissions :: !(Maybe Permissions),
    _emEnvironmentId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emLastAccess' - The time, expressed in epoch time format, when the environment member last opened the environment.
--
-- * 'emUserId' - The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
--
-- * 'emUserARN' - The Amazon Resource Name (ARN) of the environment member.
--
-- * 'emPermissions' - The type of environment member permissions associated with this environment member. Available values include:     * @owner@ : Owns the environment.     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment.
--
-- * 'emEnvironmentId' - The ID of the environment for the environment member.
environmentMember ::
  EnvironmentMember
environmentMember =
  EnvironmentMember'
    { _emLastAccess = Nothing,
      _emUserId = Nothing,
      _emUserARN = Nothing,
      _emPermissions = Nothing,
      _emEnvironmentId = Nothing
    }

-- | The time, expressed in epoch time format, when the environment member last opened the environment.
emLastAccess :: Lens' EnvironmentMember (Maybe UTCTime)
emLastAccess = lens _emLastAccess (\s a -> s {_emLastAccess = a}) . mapping _Time

-- | The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
emUserId :: Lens' EnvironmentMember (Maybe Text)
emUserId = lens _emUserId (\s a -> s {_emUserId = a})

-- | The Amazon Resource Name (ARN) of the environment member.
emUserARN :: Lens' EnvironmentMember (Maybe Text)
emUserARN = lens _emUserARN (\s a -> s {_emUserARN = a})

-- | The type of environment member permissions associated with this environment member. Available values include:     * @owner@ : Owns the environment.     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment.
emPermissions :: Lens' EnvironmentMember (Maybe Permissions)
emPermissions = lens _emPermissions (\s a -> s {_emPermissions = a})

-- | The ID of the environment for the environment member.
emEnvironmentId :: Lens' EnvironmentMember (Maybe Text)
emEnvironmentId = lens _emEnvironmentId (\s a -> s {_emEnvironmentId = a})

instance FromJSON EnvironmentMember where
  parseJSON =
    withObject
      "EnvironmentMember"
      ( \x ->
          EnvironmentMember'
            <$> (x .:? "lastAccess")
            <*> (x .:? "userId")
            <*> (x .:? "userArn")
            <*> (x .:? "permissions")
            <*> (x .:? "environmentId")
      )

instance Hashable EnvironmentMember

instance NFData EnvironmentMember
