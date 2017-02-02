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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to update the specified user pool client and password policy.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
    (
    -- * Creating a Request
      updateUserPoolClient
    , UpdateUserPoolClient
    -- * Request Lenses
    , uupcRefreshTokenValidity
    , uupcExplicitAuthFlows
    , uupcWriteAttributes
    , uupcReadAttributes
    , uupcClientName
    , uupcUserPoolId
    , uupcClientId

    -- * Destructuring the Response
    , updateUserPoolClientResponse
    , UpdateUserPoolClientResponse
    -- * Response Lenses
    , uupcrsUserPoolClient
    , uupcrsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to update the user pool client.
--
--
--
-- /See:/ 'updateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
    { _uupcRefreshTokenValidity :: !(Maybe Nat)
    , _uupcExplicitAuthFlows    :: !(Maybe [ExplicitAuthFlowsType])
    , _uupcWriteAttributes      :: !(Maybe [Text])
    , _uupcReadAttributes       :: !(Maybe [Text])
    , _uupcClientName           :: !(Maybe Text)
    , _uupcUserPoolId           :: !Text
    , _uupcClientId             :: !(Sensitive Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupcRefreshTokenValidity' - The validity of the refresh token, in days.
--
-- * 'uupcExplicitAuthFlows' - Explicit authentication flows.
--
-- * 'uupcWriteAttributes' - The writeable attributes of the user pool.
--
-- * 'uupcReadAttributes' - The read-only attributes of the user pool.
--
-- * 'uupcClientName' - The client name from the update user pool client request.
--
-- * 'uupcUserPoolId' - The user pool ID for the user pool where you want to update the user pool client.
--
-- * 'uupcClientId' - The ID of the client associated with the user pool.
updateUserPoolClient
    :: Text -- ^ 'uupcUserPoolId'
    -> Text -- ^ 'uupcClientId'
    -> UpdateUserPoolClient
updateUserPoolClient pUserPoolId_ pClientId_ =
    UpdateUserPoolClient'
    { _uupcRefreshTokenValidity = Nothing
    , _uupcExplicitAuthFlows = Nothing
    , _uupcWriteAttributes = Nothing
    , _uupcReadAttributes = Nothing
    , _uupcClientName = Nothing
    , _uupcUserPoolId = pUserPoolId_
    , _uupcClientId = _Sensitive # pClientId_
    }

-- | The validity of the refresh token, in days.
uupcRefreshTokenValidity :: Lens' UpdateUserPoolClient (Maybe Natural)
uupcRefreshTokenValidity = lens _uupcRefreshTokenValidity (\ s a -> s{_uupcRefreshTokenValidity = a}) . mapping _Nat;

-- | Explicit authentication flows.
uupcExplicitAuthFlows :: Lens' UpdateUserPoolClient [ExplicitAuthFlowsType]
uupcExplicitAuthFlows = lens _uupcExplicitAuthFlows (\ s a -> s{_uupcExplicitAuthFlows = a}) . _Default . _Coerce;

-- | The writeable attributes of the user pool.
uupcWriteAttributes :: Lens' UpdateUserPoolClient [Text]
uupcWriteAttributes = lens _uupcWriteAttributes (\ s a -> s{_uupcWriteAttributes = a}) . _Default . _Coerce;

-- | The read-only attributes of the user pool.
uupcReadAttributes :: Lens' UpdateUserPoolClient [Text]
uupcReadAttributes = lens _uupcReadAttributes (\ s a -> s{_uupcReadAttributes = a}) . _Default . _Coerce;

-- | The client name from the update user pool client request.
uupcClientName :: Lens' UpdateUserPoolClient (Maybe Text)
uupcClientName = lens _uupcClientName (\ s a -> s{_uupcClientName = a});

-- | The user pool ID for the user pool where you want to update the user pool client.
uupcUserPoolId :: Lens' UpdateUserPoolClient Text
uupcUserPoolId = lens _uupcUserPoolId (\ s a -> s{_uupcUserPoolId = a});

-- | The ID of the client associated with the user pool.
uupcClientId :: Lens' UpdateUserPoolClient Text
uupcClientId = lens _uupcClientId (\ s a -> s{_uupcClientId = a}) . _Sensitive;

instance AWSRequest UpdateUserPoolClient where
        type Rs UpdateUserPoolClient =
             UpdateUserPoolClientResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserPoolClientResponse' <$>
                   (x .?> "UserPoolClient") <*> (pure (fromEnum s)))

instance Hashable UpdateUserPoolClient

instance NFData UpdateUserPoolClient

instance ToHeaders UpdateUserPoolClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateUserPoolClient"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserPoolClient where
        toJSON UpdateUserPoolClient'{..}
          = object
              (catMaybes
                 [("RefreshTokenValidity" .=) <$>
                    _uupcRefreshTokenValidity,
                  ("ExplicitAuthFlows" .=) <$> _uupcExplicitAuthFlows,
                  ("WriteAttributes" .=) <$> _uupcWriteAttributes,
                  ("ReadAttributes" .=) <$> _uupcReadAttributes,
                  ("ClientName" .=) <$> _uupcClientName,
                  Just ("UserPoolId" .= _uupcUserPoolId),
                  Just ("ClientId" .= _uupcClientId)])

instance ToPath UpdateUserPoolClient where
        toPath = const "/"

instance ToQuery UpdateUserPoolClient where
        toQuery = const mempty

-- | Represents the response from the server to the request to update the user pool client.
--
--
--
-- /See:/ 'updateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
    { _uupcrsUserPoolClient :: !(Maybe UserPoolClientType)
    , _uupcrsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupcrsUserPoolClient' - The user pool client value from the response from the server when an update user pool client request is made.
--
-- * 'uupcrsResponseStatus' - -- | The response status code.
updateUserPoolClientResponse
    :: Int -- ^ 'uupcrsResponseStatus'
    -> UpdateUserPoolClientResponse
updateUserPoolClientResponse pResponseStatus_ =
    UpdateUserPoolClientResponse'
    { _uupcrsUserPoolClient = Nothing
    , _uupcrsResponseStatus = pResponseStatus_
    }

-- | The user pool client value from the response from the server when an update user pool client request is made.
uupcrsUserPoolClient :: Lens' UpdateUserPoolClientResponse (Maybe UserPoolClientType)
uupcrsUserPoolClient = lens _uupcrsUserPoolClient (\ s a -> s{_uupcrsUserPoolClient = a});

-- | -- | The response status code.
uupcrsResponseStatus :: Lens' UpdateUserPoolClientResponse Int
uupcrsResponseStatus = lens _uupcrsResponseStatus (\ s a -> s{_uupcrsResponseStatus = a});

instance NFData UpdateUserPoolClientResponse
