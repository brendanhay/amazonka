{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.User where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.LocaleType
import Network.AWS.WorkDocs.Types.UserStatusType
import Network.AWS.WorkDocs.Types.UserStorageMetadata
import Network.AWS.WorkDocs.Types.UserType

-- | Describes a user.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uGivenName :: !(Maybe Text),
    _uStatus :: !(Maybe UserStatusType),
    _uLocale :: !(Maybe LocaleType),
    _uUsername :: !(Maybe Text),
    _uStorage :: !(Maybe UserStorageMetadata),
    _uModifiedTimestamp :: !(Maybe POSIX),
    _uEmailAddress :: !(Maybe Text),
    _uId :: !(Maybe Text),
    _uRootFolderId :: !(Maybe Text),
    _uType :: !(Maybe UserType),
    _uSurname :: !(Maybe Text),
    _uTimeZoneId :: !(Maybe Text),
    _uCreatedTimestamp :: !(Maybe POSIX),
    _uOrganizationId :: !(Maybe Text),
    _uRecycleBinFolderId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uGivenName' - The given name of the user.
--
-- * 'uStatus' - The status of the user.
--
-- * 'uLocale' - The locale of the user.
--
-- * 'uUsername' - The login name of the user.
--
-- * 'uStorage' - The storage for the user.
--
-- * 'uModifiedTimestamp' - The time when the user was modified.
--
-- * 'uEmailAddress' - The email address of the user.
--
-- * 'uId' - The ID of the user.
--
-- * 'uRootFolderId' - The ID of the root folder.
--
-- * 'uType' - The type of user.
--
-- * 'uSurname' - The surname of the user.
--
-- * 'uTimeZoneId' - The time zone ID of the user.
--
-- * 'uCreatedTimestamp' - The time when the user was created.
--
-- * 'uOrganizationId' - The ID of the organization.
--
-- * 'uRecycleBinFolderId' - The ID of the recycle bin folder.
user ::
  User
user =
  User'
    { _uGivenName = Nothing,
      _uStatus = Nothing,
      _uLocale = Nothing,
      _uUsername = Nothing,
      _uStorage = Nothing,
      _uModifiedTimestamp = Nothing,
      _uEmailAddress = Nothing,
      _uId = Nothing,
      _uRootFolderId = Nothing,
      _uType = Nothing,
      _uSurname = Nothing,
      _uTimeZoneId = Nothing,
      _uCreatedTimestamp = Nothing,
      _uOrganizationId = Nothing,
      _uRecycleBinFolderId = Nothing
    }

-- | The given name of the user.
uGivenName :: Lens' User (Maybe Text)
uGivenName = lens _uGivenName (\s a -> s {_uGivenName = a})

-- | The status of the user.
uStatus :: Lens' User (Maybe UserStatusType)
uStatus = lens _uStatus (\s a -> s {_uStatus = a})

-- | The locale of the user.
uLocale :: Lens' User (Maybe LocaleType)
uLocale = lens _uLocale (\s a -> s {_uLocale = a})

-- | The login name of the user.
uUsername :: Lens' User (Maybe Text)
uUsername = lens _uUsername (\s a -> s {_uUsername = a})

-- | The storage for the user.
uStorage :: Lens' User (Maybe UserStorageMetadata)
uStorage = lens _uStorage (\s a -> s {_uStorage = a})

-- | The time when the user was modified.
uModifiedTimestamp :: Lens' User (Maybe UTCTime)
uModifiedTimestamp = lens _uModifiedTimestamp (\s a -> s {_uModifiedTimestamp = a}) . mapping _Time

-- | The email address of the user.
uEmailAddress :: Lens' User (Maybe Text)
uEmailAddress = lens _uEmailAddress (\s a -> s {_uEmailAddress = a})

-- | The ID of the user.
uId :: Lens' User (Maybe Text)
uId = lens _uId (\s a -> s {_uId = a})

-- | The ID of the root folder.
uRootFolderId :: Lens' User (Maybe Text)
uRootFolderId = lens _uRootFolderId (\s a -> s {_uRootFolderId = a})

-- | The type of user.
uType :: Lens' User (Maybe UserType)
uType = lens _uType (\s a -> s {_uType = a})

-- | The surname of the user.
uSurname :: Lens' User (Maybe Text)
uSurname = lens _uSurname (\s a -> s {_uSurname = a})

-- | The time zone ID of the user.
uTimeZoneId :: Lens' User (Maybe Text)
uTimeZoneId = lens _uTimeZoneId (\s a -> s {_uTimeZoneId = a})

-- | The time when the user was created.
uCreatedTimestamp :: Lens' User (Maybe UTCTime)
uCreatedTimestamp = lens _uCreatedTimestamp (\s a -> s {_uCreatedTimestamp = a}) . mapping _Time

-- | The ID of the organization.
uOrganizationId :: Lens' User (Maybe Text)
uOrganizationId = lens _uOrganizationId (\s a -> s {_uOrganizationId = a})

-- | The ID of the recycle bin folder.
uRecycleBinFolderId :: Lens' User (Maybe Text)
uRecycleBinFolderId = lens _uRecycleBinFolderId (\s a -> s {_uRecycleBinFolderId = a})

instance FromJSON User where
  parseJSON =
    withObject
      "User"
      ( \x ->
          User'
            <$> (x .:? "GivenName")
            <*> (x .:? "Status")
            <*> (x .:? "Locale")
            <*> (x .:? "Username")
            <*> (x .:? "Storage")
            <*> (x .:? "ModifiedTimestamp")
            <*> (x .:? "EmailAddress")
            <*> (x .:? "Id")
            <*> (x .:? "RootFolderId")
            <*> (x .:? "Type")
            <*> (x .:? "Surname")
            <*> (x .:? "TimeZoneId")
            <*> (x .:? "CreatedTimestamp")
            <*> (x .:? "OrganizationId")
            <*> (x .:? "RecycleBinFolderId")
      )

instance Hashable User

instance NFData User
