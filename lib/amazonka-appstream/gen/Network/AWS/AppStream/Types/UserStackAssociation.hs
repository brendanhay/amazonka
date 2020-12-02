{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociation where

import Network.AWS.AppStream.Types.AuthenticationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a user in the user pool and the associated stack.
--
--
--
-- /See:/ 'userStackAssociation' smart constructor.
data UserStackAssociation = UserStackAssociation'
  { _usaSendEmailNotification ::
      !(Maybe Bool),
    _usaStackName :: !Text,
    _usaUserName :: !(Sensitive Text),
    _usaAuthenticationType :: !AuthenticationType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserStackAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usaSendEmailNotification' - Specifies whether a welcome email is sent to a user after the user is created in the user pool.
--
-- * 'usaStackName' - The name of the stack that is associated with the user.
--
-- * 'usaUserName' - The email address of the user who is associated with the stack.
--
-- * 'usaAuthenticationType' - The authentication type for the user.
userStackAssociation ::
  -- | 'usaStackName'
  Text ->
  -- | 'usaUserName'
  Text ->
  -- | 'usaAuthenticationType'
  AuthenticationType ->
  UserStackAssociation
userStackAssociation pStackName_ pUserName_ pAuthenticationType_ =
  UserStackAssociation'
    { _usaSendEmailNotification = Nothing,
      _usaStackName = pStackName_,
      _usaUserName = _Sensitive # pUserName_,
      _usaAuthenticationType = pAuthenticationType_
    }

-- | Specifies whether a welcome email is sent to a user after the user is created in the user pool.
usaSendEmailNotification :: Lens' UserStackAssociation (Maybe Bool)
usaSendEmailNotification = lens _usaSendEmailNotification (\s a -> s {_usaSendEmailNotification = a})

-- | The name of the stack that is associated with the user.
usaStackName :: Lens' UserStackAssociation Text
usaStackName = lens _usaStackName (\s a -> s {_usaStackName = a})

-- | The email address of the user who is associated with the stack.
usaUserName :: Lens' UserStackAssociation Text
usaUserName = lens _usaUserName (\s a -> s {_usaUserName = a}) . _Sensitive

-- | The authentication type for the user.
usaAuthenticationType :: Lens' UserStackAssociation AuthenticationType
usaAuthenticationType = lens _usaAuthenticationType (\s a -> s {_usaAuthenticationType = a})

instance FromJSON UserStackAssociation where
  parseJSON =
    withObject
      "UserStackAssociation"
      ( \x ->
          UserStackAssociation'
            <$> (x .:? "SendEmailNotification")
            <*> (x .: "StackName")
            <*> (x .: "UserName")
            <*> (x .: "AuthenticationType")
      )

instance Hashable UserStackAssociation

instance NFData UserStackAssociation

instance ToJSON UserStackAssociation where
  toJSON UserStackAssociation' {..} =
    object
      ( catMaybes
          [ ("SendEmailNotification" .=) <$> _usaSendEmailNotification,
            Just ("StackName" .= _usaStackName),
            Just ("UserName" .= _usaUserName),
            Just ("AuthenticationType" .= _usaAuthenticationType)
          ]
      )
