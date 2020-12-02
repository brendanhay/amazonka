{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.User where

import Network.AWS.AppStream.Types.AuthenticationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a user in the user pool.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uStatus :: !(Maybe Text),
    _uEnabled :: !(Maybe Bool),
    _uLastName :: !(Maybe (Sensitive Text)),
    _uARN :: !(Maybe Text),
    _uCreatedTime :: !(Maybe POSIX),
    _uUserName :: !(Maybe (Sensitive Text)),
    _uFirstName :: !(Maybe (Sensitive Text)),
    _uAuthenticationType :: !AuthenticationType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uStatus' - The status of the user in the user pool. The status can be one of the following:     * UNCONFIRMED – The user is created but not confirmed.     * CONFIRMED – The user is confirmed.     * ARCHIVED – The user is no longer active.     * COMPROMISED – The user is disabled because of a potential security threat.     * UNKNOWN – The user status is not known.
--
-- * 'uEnabled' - Specifies whether the user in the user pool is enabled.
--
-- * 'uLastName' - The last name, or surname, of the user.
--
-- * 'uARN' - The ARN of the user.
--
-- * 'uCreatedTime' - The date and time the user was created in the user pool.
--
-- * 'uUserName' - The email address of the user.
--
-- * 'uFirstName' - The first name, or given name, of the user.
--
-- * 'uAuthenticationType' - The authentication type for the user.
user ::
  -- | 'uAuthenticationType'
  AuthenticationType ->
  User
user pAuthenticationType_ =
  User'
    { _uStatus = Nothing,
      _uEnabled = Nothing,
      _uLastName = Nothing,
      _uARN = Nothing,
      _uCreatedTime = Nothing,
      _uUserName = Nothing,
      _uFirstName = Nothing,
      _uAuthenticationType = pAuthenticationType_
    }

-- | The status of the user in the user pool. The status can be one of the following:     * UNCONFIRMED – The user is created but not confirmed.     * CONFIRMED – The user is confirmed.     * ARCHIVED – The user is no longer active.     * COMPROMISED – The user is disabled because of a potential security threat.     * UNKNOWN – The user status is not known.
uStatus :: Lens' User (Maybe Text)
uStatus = lens _uStatus (\s a -> s {_uStatus = a})

-- | Specifies whether the user in the user pool is enabled.
uEnabled :: Lens' User (Maybe Bool)
uEnabled = lens _uEnabled (\s a -> s {_uEnabled = a})

-- | The last name, or surname, of the user.
uLastName :: Lens' User (Maybe Text)
uLastName = lens _uLastName (\s a -> s {_uLastName = a}) . mapping _Sensitive

-- | The ARN of the user.
uARN :: Lens' User (Maybe Text)
uARN = lens _uARN (\s a -> s {_uARN = a})

-- | The date and time the user was created in the user pool.
uCreatedTime :: Lens' User (Maybe UTCTime)
uCreatedTime = lens _uCreatedTime (\s a -> s {_uCreatedTime = a}) . mapping _Time

-- | The email address of the user.
uUserName :: Lens' User (Maybe Text)
uUserName = lens _uUserName (\s a -> s {_uUserName = a}) . mapping _Sensitive

-- | The first name, or given name, of the user.
uFirstName :: Lens' User (Maybe Text)
uFirstName = lens _uFirstName (\s a -> s {_uFirstName = a}) . mapping _Sensitive

-- | The authentication type for the user.
uAuthenticationType :: Lens' User AuthenticationType
uAuthenticationType = lens _uAuthenticationType (\s a -> s {_uAuthenticationType = a})

instance FromJSON User where
  parseJSON =
    withObject
      "User"
      ( \x ->
          User'
            <$> (x .:? "Status")
            <*> (x .:? "Enabled")
            <*> (x .:? "LastName")
            <*> (x .:? "Arn")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "UserName")
            <*> (x .:? "FirstName")
            <*> (x .: "AuthenticationType")
      )

instance Hashable User

instance NFData User
