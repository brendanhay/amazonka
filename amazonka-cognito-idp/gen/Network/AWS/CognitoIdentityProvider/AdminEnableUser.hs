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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminEnableUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified user as an administrator. Works on any user.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminEnableUser
    (
    -- * Creating a Request
      adminEnableUser
    , AdminEnableUser
    -- * Request Lenses
    , aeuUserPoolId
    , aeuUsername

    -- * Destructuring the Response
    , adminEnableUserResponse
    , AdminEnableUserResponse
    -- * Response Lenses
    , aeursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request that enables the user as an administrator.
--
--
--
-- /See:/ 'adminEnableUser' smart constructor.
data AdminEnableUser = AdminEnableUser'
  { _aeuUserPoolId :: !Text
  , _aeuUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminEnableUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeuUserPoolId' - The user pool ID for the user pool where you want to enable the user.
--
-- * 'aeuUsername' - The user name of the user you wish to enable.
adminEnableUser
    :: Text -- ^ 'aeuUserPoolId'
    -> Text -- ^ 'aeuUsername'
    -> AdminEnableUser
adminEnableUser pUserPoolId_ pUsername_ =
  AdminEnableUser'
    {_aeuUserPoolId = pUserPoolId_, _aeuUsername = _Sensitive # pUsername_}


-- | The user pool ID for the user pool where you want to enable the user.
aeuUserPoolId :: Lens' AdminEnableUser Text
aeuUserPoolId = lens _aeuUserPoolId (\ s a -> s{_aeuUserPoolId = a})

-- | The user name of the user you wish to enable.
aeuUsername :: Lens' AdminEnableUser Text
aeuUsername = lens _aeuUsername (\ s a -> s{_aeuUsername = a}) . _Sensitive

instance AWSRequest AdminEnableUser where
        type Rs AdminEnableUser = AdminEnableUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminEnableUserResponse' <$> (pure (fromEnum s)))

instance Hashable AdminEnableUser where

instance NFData AdminEnableUser where

instance ToHeaders AdminEnableUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminEnableUser"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminEnableUser where
        toJSON AdminEnableUser'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _aeuUserPoolId),
                  Just ("Username" .= _aeuUsername)])

instance ToPath AdminEnableUser where
        toPath = const "/"

instance ToQuery AdminEnableUser where
        toQuery = const mempty

-- | Represents the response from the server for the request to enable a user as an administrator.
--
--
--
-- /See:/ 'adminEnableUserResponse' smart constructor.
newtype AdminEnableUserResponse = AdminEnableUserResponse'
  { _aeursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminEnableUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeursResponseStatus' - -- | The response status code.
adminEnableUserResponse
    :: Int -- ^ 'aeursResponseStatus'
    -> AdminEnableUserResponse
adminEnableUserResponse pResponseStatus_ =
  AdminEnableUserResponse' {_aeursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aeursResponseStatus :: Lens' AdminEnableUserResponse Int
aeursResponseStatus = lens _aeursResponseStatus (\ s a -> s{_aeursResponseStatus = a})

instance NFData AdminEnableUserResponse where
