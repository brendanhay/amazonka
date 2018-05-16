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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to delete the user pool client.
--
--
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
    (
    -- * Creating a Request
      deleteUserPoolClient
    , DeleteUserPoolClient
    -- * Request Lenses
    , dupcUserPoolId
    , dupcClientId

    -- * Destructuring the Response
    , deleteUserPoolClientResponse
    , DeleteUserPoolClientResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to delete a user pool client.
--
--
--
-- /See:/ 'deleteUserPoolClient' smart constructor.
data DeleteUserPoolClient = DeleteUserPoolClient'
  { _dupcUserPoolId :: !Text
  , _dupcClientId   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupcUserPoolId' - The user pool ID for the user pool where you want to delete the client.
--
-- * 'dupcClientId' - The app client ID of the app associated with the user pool.
deleteUserPoolClient
    :: Text -- ^ 'dupcUserPoolId'
    -> Text -- ^ 'dupcClientId'
    -> DeleteUserPoolClient
deleteUserPoolClient pUserPoolId_ pClientId_ =
  DeleteUserPoolClient'
    {_dupcUserPoolId = pUserPoolId_, _dupcClientId = _Sensitive # pClientId_}


-- | The user pool ID for the user pool where you want to delete the client.
dupcUserPoolId :: Lens' DeleteUserPoolClient Text
dupcUserPoolId = lens _dupcUserPoolId (\ s a -> s{_dupcUserPoolId = a})

-- | The app client ID of the app associated with the user pool.
dupcClientId :: Lens' DeleteUserPoolClient Text
dupcClientId = lens _dupcClientId (\ s a -> s{_dupcClientId = a}) . _Sensitive

instance AWSRequest DeleteUserPoolClient where
        type Rs DeleteUserPoolClient =
             DeleteUserPoolClientResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull DeleteUserPoolClientResponse'

instance Hashable DeleteUserPoolClient where

instance NFData DeleteUserPoolClient where

instance ToHeaders DeleteUserPoolClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DeleteUserPoolClient"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserPoolClient where
        toJSON DeleteUserPoolClient'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _dupcUserPoolId),
                  Just ("ClientId" .= _dupcClientId)])

instance ToPath DeleteUserPoolClient where
        toPath = const "/"

instance ToQuery DeleteUserPoolClient where
        toQuery = const mempty

-- | /See:/ 'deleteUserPoolClientResponse' smart constructor.
data DeleteUserPoolClientResponse =
  DeleteUserPoolClientResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPoolClientResponse' with the minimum fields required to make a request.
--
deleteUserPoolClientResponse
    :: DeleteUserPoolClientResponse
deleteUserPoolClientResponse = DeleteUserPoolClientResponse'


instance NFData DeleteUserPoolClientResponse where
