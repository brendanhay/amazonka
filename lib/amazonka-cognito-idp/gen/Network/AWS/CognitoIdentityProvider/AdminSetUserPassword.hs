{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified user's password in a user pool as an administrator. Works on any user.
--
--
-- The password can be temporary or permanent. If it is temporary, the user status will be placed into the @FORCE_CHANGE_PASSWORD@ state. When the user next tries to sign in, the InitiateAuth/AdminInitiateAuth response will contain the @NEW_PASSWORD_REQUIRED@ challenge. If the user does not sign in before it expires, the user will not be able to sign in and their password will need to be reset by an administrator.
--
-- Once the user has set a new password, or the password is permanent, the user status will be set to @Confirmed@ .
module Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
  ( -- * Creating a Request
    adminSetUserPassword,
    AdminSetUserPassword,

    -- * Request Lenses
    asupPermanent,
    asupUserPoolId,
    asupUsername,
    asupPassword,

    -- * Destructuring the Response
    adminSetUserPasswordResponse,
    AdminSetUserPasswordResponse,

    -- * Response Lenses
    asuprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminSetUserPassword' smart constructor.
data AdminSetUserPassword = AdminSetUserPassword'
  { _asupPermanent ::
      !(Maybe Bool),
    _asupUserPoolId :: !Text,
    _asupUsername :: !(Sensitive Text),
    _asupPassword :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminSetUserPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asupPermanent' - @True@ if the password is permanent, @False@ if it is temporary.
--
-- * 'asupUserPoolId' - The user pool ID for the user pool where you want to set the user's password.
--
-- * 'asupUsername' - The user name of the user whose password you wish to set.
--
-- * 'asupPassword' - The password for the user.
adminSetUserPassword ::
  -- | 'asupUserPoolId'
  Text ->
  -- | 'asupUsername'
  Text ->
  -- | 'asupPassword'
  Text ->
  AdminSetUserPassword
adminSetUserPassword pUserPoolId_ pUsername_ pPassword_ =
  AdminSetUserPassword'
    { _asupPermanent = Nothing,
      _asupUserPoolId = pUserPoolId_,
      _asupUsername = _Sensitive # pUsername_,
      _asupPassword = _Sensitive # pPassword_
    }

-- | @True@ if the password is permanent, @False@ if it is temporary.
asupPermanent :: Lens' AdminSetUserPassword (Maybe Bool)
asupPermanent = lens _asupPermanent (\s a -> s {_asupPermanent = a})

-- | The user pool ID for the user pool where you want to set the user's password.
asupUserPoolId :: Lens' AdminSetUserPassword Text
asupUserPoolId = lens _asupUserPoolId (\s a -> s {_asupUserPoolId = a})

-- | The user name of the user whose password you wish to set.
asupUsername :: Lens' AdminSetUserPassword Text
asupUsername = lens _asupUsername (\s a -> s {_asupUsername = a}) . _Sensitive

-- | The password for the user.
asupPassword :: Lens' AdminSetUserPassword Text
asupPassword = lens _asupPassword (\s a -> s {_asupPassword = a}) . _Sensitive

instance AWSRequest AdminSetUserPassword where
  type Rs AdminSetUserPassword = AdminSetUserPasswordResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveEmpty
      (\s h x -> AdminSetUserPasswordResponse' <$> (pure (fromEnum s)))

instance Hashable AdminSetUserPassword

instance NFData AdminSetUserPassword

instance ToHeaders AdminSetUserPassword where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSCognitoIdentityProviderService.AdminSetUserPassword" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AdminSetUserPassword where
  toJSON AdminSetUserPassword' {..} =
    object
      ( catMaybes
          [ ("Permanent" .=) <$> _asupPermanent,
            Just ("UserPoolId" .= _asupUserPoolId),
            Just ("Username" .= _asupUsername),
            Just ("Password" .= _asupPassword)
          ]
      )

instance ToPath AdminSetUserPassword where
  toPath = const "/"

instance ToQuery AdminSetUserPassword where
  toQuery = const mempty

-- | /See:/ 'adminSetUserPasswordResponse' smart constructor.
newtype AdminSetUserPasswordResponse = AdminSetUserPasswordResponse'
  { _asuprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminSetUserPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asuprsResponseStatus' - -- | The response status code.
adminSetUserPasswordResponse ::
  -- | 'asuprsResponseStatus'
  Int ->
  AdminSetUserPasswordResponse
adminSetUserPasswordResponse pResponseStatus_ =
  AdminSetUserPasswordResponse'
    { _asuprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
asuprsResponseStatus :: Lens' AdminSetUserPasswordResponse Int
asuprsResponseStatus = lens _asuprsResponseStatus (\s a -> s {_asuprsResponseStatus = a})

instance NFData AdminSetUserPasswordResponse
