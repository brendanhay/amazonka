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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminDisableUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user as an administrator. Works on any user.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminDisableUser
    (
    -- * Creating a Request
      adminDisableUser
    , AdminDisableUser
    -- * Request Lenses
    , aduUserPoolId
    , aduUsername

    -- * Destructuring the Response
    , adminDisableUserResponse
    , AdminDisableUserResponse
    -- * Response Lenses
    , adursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to disable any user as an administrator.
--
--
--
-- /See:/ 'adminDisableUser' smart constructor.
data AdminDisableUser = AdminDisableUser'
  { _aduUserPoolId :: !Text
  , _aduUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminDisableUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aduUserPoolId' - The user pool ID for the user pool where you want to disable the user.
--
-- * 'aduUsername' - The user name of the user you wish to disable.
adminDisableUser
    :: Text -- ^ 'aduUserPoolId'
    -> Text -- ^ 'aduUsername'
    -> AdminDisableUser
adminDisableUser pUserPoolId_ pUsername_ =
  AdminDisableUser'
    {_aduUserPoolId = pUserPoolId_, _aduUsername = _Sensitive # pUsername_}


-- | The user pool ID for the user pool where you want to disable the user.
aduUserPoolId :: Lens' AdminDisableUser Text
aduUserPoolId = lens _aduUserPoolId (\ s a -> s{_aduUserPoolId = a})

-- | The user name of the user you wish to disable.
aduUsername :: Lens' AdminDisableUser Text
aduUsername = lens _aduUsername (\ s a -> s{_aduUsername = a}) . _Sensitive

instance AWSRequest AdminDisableUser where
        type Rs AdminDisableUser = AdminDisableUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminDisableUserResponse' <$> (pure (fromEnum s)))

instance Hashable AdminDisableUser where

instance NFData AdminDisableUser where

instance ToHeaders AdminDisableUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminDisableUser"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminDisableUser where
        toJSON AdminDisableUser'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _aduUserPoolId),
                  Just ("Username" .= _aduUsername)])

instance ToPath AdminDisableUser where
        toPath = const "/"

instance ToQuery AdminDisableUser where
        toQuery = const mempty

-- | Represents the response received from the server to disable the user as an administrator.
--
--
--
-- /See:/ 'adminDisableUserResponse' smart constructor.
newtype AdminDisableUserResponse = AdminDisableUserResponse'
  { _adursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminDisableUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adursResponseStatus' - -- | The response status code.
adminDisableUserResponse
    :: Int -- ^ 'adursResponseStatus'
    -> AdminDisableUserResponse
adminDisableUserResponse pResponseStatus_ =
  AdminDisableUserResponse' {_adursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
adursResponseStatus :: Lens' AdminDisableUserResponse Int
adursResponseStatus = lens _adursResponseStatus (\ s a -> s{_adursResponseStatus = a})

instance NFData AdminDisableUserResponse where
