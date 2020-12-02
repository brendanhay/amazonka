{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserType where

import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user type.
--
--
--
-- /See:/ 'userType' smart constructor.
data UserType = UserType'
  { _utEnabled :: !(Maybe Bool),
    _utUserStatus :: !(Maybe UserStatusType),
    _utUsername :: !(Maybe (Sensitive Text)),
    _utUserCreateDate :: !(Maybe POSIX),
    _utAttributes :: !(Maybe [AttributeType]),
    _utMFAOptions :: !(Maybe [MFAOptionType]),
    _utUserLastModifiedDate :: !(Maybe POSIX)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utEnabled' - Specifies whether the user is enabled.
--
-- * 'utUserStatus' - The user status. Can be one of the following:     * UNCONFIRMED - User has been created but not confirmed.     * CONFIRMED - User has been confirmed.     * ARCHIVED - User is no longer active.     * COMPROMISED - User is disabled due to a potential security threat.     * UNKNOWN - User status is not known.     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else.
--
-- * 'utUsername' - The user name of the user you wish to describe.
--
-- * 'utUserCreateDate' - The creation date of the user.
--
-- * 'utAttributes' - A container with information about the user type attributes.
--
-- * 'utMFAOptions' - The MFA options for the user.
--
-- * 'utUserLastModifiedDate' - The last modified date of the user.
userType ::
  UserType
userType =
  UserType'
    { _utEnabled = Nothing,
      _utUserStatus = Nothing,
      _utUsername = Nothing,
      _utUserCreateDate = Nothing,
      _utAttributes = Nothing,
      _utMFAOptions = Nothing,
      _utUserLastModifiedDate = Nothing
    }

-- | Specifies whether the user is enabled.
utEnabled :: Lens' UserType (Maybe Bool)
utEnabled = lens _utEnabled (\s a -> s {_utEnabled = a})

-- | The user status. Can be one of the following:     * UNCONFIRMED - User has been created but not confirmed.     * CONFIRMED - User has been confirmed.     * ARCHIVED - User is no longer active.     * COMPROMISED - User is disabled due to a potential security threat.     * UNKNOWN - User status is not known.     * RESET_REQUIRED - User is confirmed, but the user must request a code and reset his or her password before he or she can sign in.     * FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign in using a temporary password, but on first sign-in, the user must change his or her password to a new value before doing anything else.
utUserStatus :: Lens' UserType (Maybe UserStatusType)
utUserStatus = lens _utUserStatus (\s a -> s {_utUserStatus = a})

-- | The user name of the user you wish to describe.
utUsername :: Lens' UserType (Maybe Text)
utUsername = lens _utUsername (\s a -> s {_utUsername = a}) . mapping _Sensitive

-- | The creation date of the user.
utUserCreateDate :: Lens' UserType (Maybe UTCTime)
utUserCreateDate = lens _utUserCreateDate (\s a -> s {_utUserCreateDate = a}) . mapping _Time

-- | A container with information about the user type attributes.
utAttributes :: Lens' UserType [AttributeType]
utAttributes = lens _utAttributes (\s a -> s {_utAttributes = a}) . _Default . _Coerce

-- | The MFA options for the user.
utMFAOptions :: Lens' UserType [MFAOptionType]
utMFAOptions = lens _utMFAOptions (\s a -> s {_utMFAOptions = a}) . _Default . _Coerce

-- | The last modified date of the user.
utUserLastModifiedDate :: Lens' UserType (Maybe UTCTime)
utUserLastModifiedDate = lens _utUserLastModifiedDate (\s a -> s {_utUserLastModifiedDate = a}) . mapping _Time

instance FromJSON UserType where
  parseJSON =
    withObject
      "UserType"
      ( \x ->
          UserType'
            <$> (x .:? "Enabled")
            <*> (x .:? "UserStatus")
            <*> (x .:? "Username")
            <*> (x .:? "UserCreateDate")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "MFAOptions" .!= mempty)
            <*> (x .:? "UserLastModifiedDate")
      )

instance Hashable UserType

instance NFData UserType
