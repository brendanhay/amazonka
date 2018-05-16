{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified user's password in a user pool as an administrator. Works on any user.
--
--
-- When a developer calls this API, the current password is invalidated, so it must be changed. If a user tries to sign in after the API is called, the app will get a PasswordResetRequiredException exception back and should direct the user down the flow to reset the password, which is the same as the forgot password flow. In addition, if the user pool has phone verification selected and a verified phone number exists for the user, or if email verification is selected and a verified email exists for the user, calling this API will also result in sending a message to the end user with the code to change their password.
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
    (
    -- * Creating a Request
      adminResetUserPassword
    , AdminResetUserPassword
    -- * Request Lenses
    , arupUserPoolId
    , arupUsername

    -- * Destructuring the Response
    , adminResetUserPasswordResponse
    , AdminResetUserPasswordResponse
    -- * Response Lenses
    , aruprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to reset a user's password as an administrator.
--
--
--
-- /See:/ 'adminResetUserPassword' smart constructor.
data AdminResetUserPassword = AdminResetUserPassword'
  { _arupUserPoolId :: !Text
  , _arupUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminResetUserPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arupUserPoolId' - The user pool ID for the user pool where you want to reset the user's password.
--
-- * 'arupUsername' - The user name of the user whose password you wish to reset.
adminResetUserPassword
    :: Text -- ^ 'arupUserPoolId'
    -> Text -- ^ 'arupUsername'
    -> AdminResetUserPassword
adminResetUserPassword pUserPoolId_ pUsername_ =
  AdminResetUserPassword'
    {_arupUserPoolId = pUserPoolId_, _arupUsername = _Sensitive # pUsername_}


-- | The user pool ID for the user pool where you want to reset the user's password.
arupUserPoolId :: Lens' AdminResetUserPassword Text
arupUserPoolId = lens _arupUserPoolId (\ s a -> s{_arupUserPoolId = a})

-- | The user name of the user whose password you wish to reset.
arupUsername :: Lens' AdminResetUserPassword Text
arupUsername = lens _arupUsername (\ s a -> s{_arupUsername = a}) . _Sensitive

instance AWSRequest AdminResetUserPassword where
        type Rs AdminResetUserPassword =
             AdminResetUserPasswordResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminResetUserPasswordResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminResetUserPassword where

instance NFData AdminResetUserPassword where

instance ToHeaders AdminResetUserPassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminResetUserPassword"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminResetUserPassword where
        toJSON AdminResetUserPassword'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _arupUserPoolId),
                  Just ("Username" .= _arupUsername)])

instance ToPath AdminResetUserPassword where
        toPath = const "/"

instance ToQuery AdminResetUserPassword where
        toQuery = const mempty

-- | Represents the response from the server to reset a user password as an administrator.
--
--
--
-- /See:/ 'adminResetUserPasswordResponse' smart constructor.
newtype AdminResetUserPasswordResponse = AdminResetUserPasswordResponse'
  { _aruprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminResetUserPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aruprsResponseStatus' - -- | The response status code.
adminResetUserPasswordResponse
    :: Int -- ^ 'aruprsResponseStatus'
    -> AdminResetUserPasswordResponse
adminResetUserPasswordResponse pResponseStatus_ =
  AdminResetUserPasswordResponse' {_aruprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aruprsResponseStatus :: Lens' AdminResetUserPasswordResponse Int
aruprsResponseStatus = lens _aruprsResponseStatus (\ s a -> s{_aruprsResponseStatus = a})

instance NFData AdminResetUserPasswordResponse where
