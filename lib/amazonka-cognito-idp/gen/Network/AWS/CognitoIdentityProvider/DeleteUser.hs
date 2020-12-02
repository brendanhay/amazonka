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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to delete himself or herself.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteUser
    (
    -- * Creating a Request
      deleteUser
    , DeleteUser
    -- * Request Lenses
    , duAccessToken

    -- * Destructuring the Response
    , deleteUserResponse
    , DeleteUserResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to delete a user.
--
--
--
-- /See:/ 'deleteUser' smart constructor.
newtype DeleteUser = DeleteUser'
  { _duAccessToken :: Sensitive Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duAccessToken' - The access token from a request to delete a user.
deleteUser
    :: Text -- ^ 'duAccessToken'
    -> DeleteUser
deleteUser pAccessToken_ =
  DeleteUser' {_duAccessToken = _Sensitive # pAccessToken_}


-- | The access token from a request to delete a user.
duAccessToken :: Lens' DeleteUser Text
duAccessToken = lens _duAccessToken (\ s a -> s{_duAccessToken = a}) . _Sensitive

instance AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull DeleteUserResponse'

instance Hashable DeleteUser where

instance NFData DeleteUser where

instance ToHeaders DeleteUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteUser" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUser where
        toJSON DeleteUser'{..}
          = object
              (catMaybes [Just ("AccessToken" .= _duAccessToken)])

instance ToPath DeleteUser where
        toPath = const "/"

instance ToQuery DeleteUser where
        toQuery = const mempty

-- | /See:/ 'deleteUserResponse' smart constructor.
data DeleteUserResponse =
  DeleteUserResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserResponse' with the minimum fields required to make a request.
--
deleteUserResponse
    :: DeleteUserResponse
deleteUserResponse = DeleteUserResponse'


instance NFData DeleteUserResponse where
