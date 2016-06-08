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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user pool client.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
    (
    -- * Creating a Request
      createUserPoolClient
    , CreateUserPoolClient
    -- * Request Lenses
    , cupcGenerateSecret
    , cupcUserPoolId
    , cupcClientName

    -- * Destructuring the Response
    , createUserPoolClientResponse
    , CreateUserPoolClientResponse
    -- * Response Lenses
    , cupcrsUserPoolClient
    , cupcrsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to create a user pool client.
--
-- /See:/ 'createUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
    { _cupcGenerateSecret :: !(Maybe Bool)
    , _cupcUserPoolId     :: !Text
    , _cupcClientName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcGenerateSecret'
--
-- * 'cupcUserPoolId'
--
-- * 'cupcClientName'
createUserPoolClient
    :: Text -- ^ 'cupcUserPoolId'
    -> Text -- ^ 'cupcClientName'
    -> CreateUserPoolClient
createUserPoolClient pUserPoolId_ pClientName_ =
    CreateUserPoolClient'
    { _cupcGenerateSecret = Nothing
    , _cupcUserPoolId = pUserPoolId_
    , _cupcClientName = pClientName_
    }

-- | Boolean to specify whether you want to generate a secret for the user pool client being created.
cupcGenerateSecret :: Lens' CreateUserPoolClient (Maybe Bool)
cupcGenerateSecret = lens _cupcGenerateSecret (\ s a -> s{_cupcGenerateSecret = a});

-- | The user pool ID for the user pool where you want to create a user pool client.
cupcUserPoolId :: Lens' CreateUserPoolClient Text
cupcUserPoolId = lens _cupcUserPoolId (\ s a -> s{_cupcUserPoolId = a});

-- | The client name for the user pool client you would like to create.
cupcClientName :: Lens' CreateUserPoolClient Text
cupcClientName = lens _cupcClientName (\ s a -> s{_cupcClientName = a});

instance AWSRequest CreateUserPoolClient where
        type Rs CreateUserPoolClient =
             CreateUserPoolClientResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserPoolClientResponse' <$>
                   (x .?> "UserPoolClient") <*> (pure (fromEnum s)))

instance Hashable CreateUserPoolClient

instance NFData CreateUserPoolClient

instance ToHeaders CreateUserPoolClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.CreateUserPoolClient"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserPoolClient where
        toJSON CreateUserPoolClient'{..}
          = object
              (catMaybes
                 [("GenerateSecret" .=) <$> _cupcGenerateSecret,
                  Just ("UserPoolId" .= _cupcUserPoolId),
                  Just ("ClientName" .= _cupcClientName)])

instance ToPath CreateUserPoolClient where
        toPath = const "/"

instance ToQuery CreateUserPoolClient where
        toQuery = const mempty

-- | Represents the response from the server to create a user pool client.
--
-- /See:/ 'createUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
    { _cupcrsUserPoolClient :: !(Maybe UserPoolClientType)
    , _cupcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcrsUserPoolClient'
--
-- * 'cupcrsResponseStatus'
createUserPoolClientResponse
    :: Int -- ^ 'cupcrsResponseStatus'
    -> CreateUserPoolClientResponse
createUserPoolClientResponse pResponseStatus_ =
    CreateUserPoolClientResponse'
    { _cupcrsUserPoolClient = Nothing
    , _cupcrsResponseStatus = pResponseStatus_
    }

-- | The user pool client that was just created.
cupcrsUserPoolClient :: Lens' CreateUserPoolClientResponse (Maybe UserPoolClientType)
cupcrsUserPoolClient = lens _cupcrsUserPoolClient (\ s a -> s{_cupcrsUserPoolClient = a});

-- | The response status code.
cupcrsResponseStatus :: Lens' CreateUserPoolClientResponse Int
cupcrsResponseStatus = lens _cupcrsResponseStatus (\ s a -> s{_cupcrsResponseStatus = a});

instance NFData CreateUserPoolClientResponse
