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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices, as an administrator.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
    (
    -- * Creating a Request
      adminUserGlobalSignOut
    , AdminUserGlobalSignOut
    -- * Request Lenses
    , augsoUserPoolId
    , augsoUsername

    -- * Destructuring the Response
    , adminUserGlobalSignOutResponse
    , AdminUserGlobalSignOutResponse
    -- * Response Lenses
    , augsorsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to sign out of all devices, as an administrator.
--
--
--
-- /See:/ 'adminUserGlobalSignOut' smart constructor.
data AdminUserGlobalSignOut = AdminUserGlobalSignOut'
  { _augsoUserPoolId :: !Text
  , _augsoUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUserGlobalSignOut' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'augsoUserPoolId' - The user pool ID.
--
-- * 'augsoUsername' - The user name.
adminUserGlobalSignOut
    :: Text -- ^ 'augsoUserPoolId'
    -> Text -- ^ 'augsoUsername'
    -> AdminUserGlobalSignOut
adminUserGlobalSignOut pUserPoolId_ pUsername_ =
  AdminUserGlobalSignOut'
    {_augsoUserPoolId = pUserPoolId_, _augsoUsername = _Sensitive # pUsername_}


-- | The user pool ID.
augsoUserPoolId :: Lens' AdminUserGlobalSignOut Text
augsoUserPoolId = lens _augsoUserPoolId (\ s a -> s{_augsoUserPoolId = a})

-- | The user name.
augsoUsername :: Lens' AdminUserGlobalSignOut Text
augsoUsername = lens _augsoUsername (\ s a -> s{_augsoUsername = a}) . _Sensitive

instance AWSRequest AdminUserGlobalSignOut where
        type Rs AdminUserGlobalSignOut =
             AdminUserGlobalSignOutResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminUserGlobalSignOutResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminUserGlobalSignOut where

instance NFData AdminUserGlobalSignOut where

instance ToHeaders AdminUserGlobalSignOut where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminUserGlobalSignOut"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminUserGlobalSignOut where
        toJSON AdminUserGlobalSignOut'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _augsoUserPoolId),
                  Just ("Username" .= _augsoUsername)])

instance ToPath AdminUserGlobalSignOut where
        toPath = const "/"

instance ToQuery AdminUserGlobalSignOut where
        toQuery = const mempty

-- | The global sign-out response, as an administrator.
--
--
--
-- /See:/ 'adminUserGlobalSignOutResponse' smart constructor.
newtype AdminUserGlobalSignOutResponse = AdminUserGlobalSignOutResponse'
  { _augsorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUserGlobalSignOutResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'augsorsResponseStatus' - -- | The response status code.
adminUserGlobalSignOutResponse
    :: Int -- ^ 'augsorsResponseStatus'
    -> AdminUserGlobalSignOutResponse
adminUserGlobalSignOutResponse pResponseStatus_ =
  AdminUserGlobalSignOutResponse' {_augsorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
augsorsResponseStatus :: Lens' AdminUserGlobalSignOutResponse Int
augsorsResponseStatus = lens _augsorsResponseStatus (\ s a -> s{_augsorsResponseStatus = a})

instance NFData AdminUserGlobalSignOutResponse where
