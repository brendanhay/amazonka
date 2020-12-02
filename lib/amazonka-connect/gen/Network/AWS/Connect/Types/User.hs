{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.User where

import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a user account for a Amazon Connect instance.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uRoutingProfileId :: !(Maybe Text),
    _uDirectoryUserId :: !(Maybe Text),
    _uARN :: !(Maybe Text),
    _uIdentityInfo :: !(Maybe UserIdentityInfo),
    _uSecurityProfileIds :: !(Maybe (List1 Text)),
    _uUsername :: !(Maybe Text),
    _uId :: !(Maybe Text),
    _uHierarchyGroupId :: !(Maybe Text),
    _uPhoneConfig :: !(Maybe UserPhoneConfig),
    _uTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uRoutingProfileId' - The identifier of the routing profile for the user.
--
-- * 'uDirectoryUserId' - The identifier of the user account in the directory used for identity management.
--
-- * 'uARN' - The Amazon Resource Name (ARN) of the user account.
--
-- * 'uIdentityInfo' - Information about the user identity.
--
-- * 'uSecurityProfileIds' - The identifiers of the security profiles for the user.
--
-- * 'uUsername' - The user name assigned to the user account.
--
-- * 'uId' - The identifier of the user account.
--
-- * 'uHierarchyGroupId' - The identifier of the hierarchy group for the user.
--
-- * 'uPhoneConfig' - Information about the phone configuration for the user.
--
-- * 'uTags' - The tags.
user ::
  User
user =
  User'
    { _uRoutingProfileId = Nothing,
      _uDirectoryUserId = Nothing,
      _uARN = Nothing,
      _uIdentityInfo = Nothing,
      _uSecurityProfileIds = Nothing,
      _uUsername = Nothing,
      _uId = Nothing,
      _uHierarchyGroupId = Nothing,
      _uPhoneConfig = Nothing,
      _uTags = Nothing
    }

-- | The identifier of the routing profile for the user.
uRoutingProfileId :: Lens' User (Maybe Text)
uRoutingProfileId = lens _uRoutingProfileId (\s a -> s {_uRoutingProfileId = a})

-- | The identifier of the user account in the directory used for identity management.
uDirectoryUserId :: Lens' User (Maybe Text)
uDirectoryUserId = lens _uDirectoryUserId (\s a -> s {_uDirectoryUserId = a})

-- | The Amazon Resource Name (ARN) of the user account.
uARN :: Lens' User (Maybe Text)
uARN = lens _uARN (\s a -> s {_uARN = a})

-- | Information about the user identity.
uIdentityInfo :: Lens' User (Maybe UserIdentityInfo)
uIdentityInfo = lens _uIdentityInfo (\s a -> s {_uIdentityInfo = a})

-- | The identifiers of the security profiles for the user.
uSecurityProfileIds :: Lens' User (Maybe (NonEmpty Text))
uSecurityProfileIds = lens _uSecurityProfileIds (\s a -> s {_uSecurityProfileIds = a}) . mapping _List1

-- | The user name assigned to the user account.
uUsername :: Lens' User (Maybe Text)
uUsername = lens _uUsername (\s a -> s {_uUsername = a})

-- | The identifier of the user account.
uId :: Lens' User (Maybe Text)
uId = lens _uId (\s a -> s {_uId = a})

-- | The identifier of the hierarchy group for the user.
uHierarchyGroupId :: Lens' User (Maybe Text)
uHierarchyGroupId = lens _uHierarchyGroupId (\s a -> s {_uHierarchyGroupId = a})

-- | Information about the phone configuration for the user.
uPhoneConfig :: Lens' User (Maybe UserPhoneConfig)
uPhoneConfig = lens _uPhoneConfig (\s a -> s {_uPhoneConfig = a})

-- | The tags.
uTags :: Lens' User (HashMap Text (Text))
uTags = lens _uTags (\s a -> s {_uTags = a}) . _Default . _Map

instance FromJSON User where
  parseJSON =
    withObject
      "User"
      ( \x ->
          User'
            <$> (x .:? "RoutingProfileId")
            <*> (x .:? "DirectoryUserId")
            <*> (x .:? "Arn")
            <*> (x .:? "IdentityInfo")
            <*> (x .:? "SecurityProfileIds")
            <*> (x .:? "Username")
            <*> (x .:? "Id")
            <*> (x .:? "HierarchyGroupId")
            <*> (x .:? "PhoneConfig")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable User

instance NFData User
