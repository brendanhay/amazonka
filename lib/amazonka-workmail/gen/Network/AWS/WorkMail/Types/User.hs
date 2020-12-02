{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.User where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.UserRole

-- | The representation of an Amazon WorkMail user.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uEmail :: !(Maybe Text),
    _uState :: !(Maybe EntityState),
    _uDisabledDate :: !(Maybe POSIX),
    _uName :: !(Maybe Text),
    _uId :: !(Maybe Text),
    _uDisplayName :: !(Maybe Text),
    _uUserRole :: !(Maybe UserRole),
    _uEnabledDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uEmail' - The email of the user.
--
-- * 'uState' - The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'uDisabledDate' - The date indicating when the user was disabled from Amazon WorkMail use.
--
-- * 'uName' - The name of the user.
--
-- * 'uId' - The identifier of the user.
--
-- * 'uDisplayName' - The display name of the user.
--
-- * 'uUserRole' - The role of the user.
--
-- * 'uEnabledDate' - The date indicating when the user was enabled for Amazon WorkMail use.
user ::
  User
user =
  User'
    { _uEmail = Nothing,
      _uState = Nothing,
      _uDisabledDate = Nothing,
      _uName = Nothing,
      _uId = Nothing,
      _uDisplayName = Nothing,
      _uUserRole = Nothing,
      _uEnabledDate = Nothing
    }

-- | The email of the user.
uEmail :: Lens' User (Maybe Text)
uEmail = lens _uEmail (\s a -> s {_uEmail = a})

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
uState :: Lens' User (Maybe EntityState)
uState = lens _uState (\s a -> s {_uState = a})

-- | The date indicating when the user was disabled from Amazon WorkMail use.
uDisabledDate :: Lens' User (Maybe UTCTime)
uDisabledDate = lens _uDisabledDate (\s a -> s {_uDisabledDate = a}) . mapping _Time

-- | The name of the user.
uName :: Lens' User (Maybe Text)
uName = lens _uName (\s a -> s {_uName = a})

-- | The identifier of the user.
uId :: Lens' User (Maybe Text)
uId = lens _uId (\s a -> s {_uId = a})

-- | The display name of the user.
uDisplayName :: Lens' User (Maybe Text)
uDisplayName = lens _uDisplayName (\s a -> s {_uDisplayName = a})

-- | The role of the user.
uUserRole :: Lens' User (Maybe UserRole)
uUserRole = lens _uUserRole (\s a -> s {_uUserRole = a})

-- | The date indicating when the user was enabled for Amazon WorkMail use.
uEnabledDate :: Lens' User (Maybe UTCTime)
uEnabledDate = lens _uEnabledDate (\s a -> s {_uEnabledDate = a}) . mapping _Time

instance FromJSON User where
  parseJSON =
    withObject
      "User"
      ( \x ->
          User'
            <$> (x .:? "Email")
            <*> (x .:? "State")
            <*> (x .:? "DisabledDate")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "DisplayName")
            <*> (x .:? "UserRole")
            <*> (x .:? "EnabledDate")
      )

instance Hashable User

instance NFData User
