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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateResourceServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OAuth2.0 resource server and defines custom scopes in it.
--
--
module Network.AWS.CognitoIdentityProvider.CreateResourceServer
    (
    -- * Creating a Request
      createResourceServer
    , CreateResourceServer
    -- * Request Lenses
    , crsScopes
    , crsUserPoolId
    , crsIdentifier
    , crsName

    -- * Destructuring the Response
    , createResourceServerResponse
    , CreateResourceServerResponse
    -- * Response Lenses
    , crsrsResponseStatus
    , crsrsResourceServer
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createResourceServer' smart constructor.
data CreateResourceServer = CreateResourceServer'
  { _crsScopes     :: !(Maybe [ResourceServerScopeType])
  , _crsUserPoolId :: !Text
  , _crsIdentifier :: !Text
  , _crsName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsScopes' - A list of scopes. Each scope is map, where the keys are @name@ and @description@ .
--
-- * 'crsUserPoolId' - The user pool ID for the user pool.
--
-- * 'crsIdentifier' - A unique resource server identifier for the resource server. This could be an HTTPS endpoint where the resource server is located. For example, @https://my-weather-api.example.com@ .
--
-- * 'crsName' - A friendly name for the resource server.
createResourceServer
    :: Text -- ^ 'crsUserPoolId'
    -> Text -- ^ 'crsIdentifier'
    -> Text -- ^ 'crsName'
    -> CreateResourceServer
createResourceServer pUserPoolId_ pIdentifier_ pName_ =
  CreateResourceServer'
    { _crsScopes = Nothing
    , _crsUserPoolId = pUserPoolId_
    , _crsIdentifier = pIdentifier_
    , _crsName = pName_
    }


-- | A list of scopes. Each scope is map, where the keys are @name@ and @description@ .
crsScopes :: Lens' CreateResourceServer [ResourceServerScopeType]
crsScopes = lens _crsScopes (\ s a -> s{_crsScopes = a}) . _Default . _Coerce

-- | The user pool ID for the user pool.
crsUserPoolId :: Lens' CreateResourceServer Text
crsUserPoolId = lens _crsUserPoolId (\ s a -> s{_crsUserPoolId = a})

-- | A unique resource server identifier for the resource server. This could be an HTTPS endpoint where the resource server is located. For example, @https://my-weather-api.example.com@ .
crsIdentifier :: Lens' CreateResourceServer Text
crsIdentifier = lens _crsIdentifier (\ s a -> s{_crsIdentifier = a})

-- | A friendly name for the resource server.
crsName :: Lens' CreateResourceServer Text
crsName = lens _crsName (\ s a -> s{_crsName = a})

instance AWSRequest CreateResourceServer where
        type Rs CreateResourceServer =
             CreateResourceServerResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 CreateResourceServerResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ResourceServer"))

instance Hashable CreateResourceServer where

instance NFData CreateResourceServer where

instance ToHeaders CreateResourceServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.CreateResourceServer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateResourceServer where
        toJSON CreateResourceServer'{..}
          = object
              (catMaybes
                 [("Scopes" .=) <$> _crsScopes,
                  Just ("UserPoolId" .= _crsUserPoolId),
                  Just ("Identifier" .= _crsIdentifier),
                  Just ("Name" .= _crsName)])

instance ToPath CreateResourceServer where
        toPath = const "/"

instance ToQuery CreateResourceServer where
        toQuery = const mempty

-- | /See:/ 'createResourceServerResponse' smart constructor.
data CreateResourceServerResponse = CreateResourceServerResponse'
  { _crsrsResponseStatus :: !Int
  , _crsrsResourceServer :: !ResourceServerType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsrsResponseStatus' - -- | The response status code.
--
-- * 'crsrsResourceServer' - The newly created resource server.
createResourceServerResponse
    :: Int -- ^ 'crsrsResponseStatus'
    -> ResourceServerType -- ^ 'crsrsResourceServer'
    -> CreateResourceServerResponse
createResourceServerResponse pResponseStatus_ pResourceServer_ =
  CreateResourceServerResponse'
    { _crsrsResponseStatus = pResponseStatus_
    , _crsrsResourceServer = pResourceServer_
    }


-- | -- | The response status code.
crsrsResponseStatus :: Lens' CreateResourceServerResponse Int
crsrsResponseStatus = lens _crsrsResponseStatus (\ s a -> s{_crsrsResponseStatus = a})

-- | The newly created resource server.
crsrsResourceServer :: Lens' CreateResourceServerResponse ResourceServerType
crsrsResourceServer = lens _crsrsResourceServer (\ s a -> s{_crsrsResourceServer = a})

instance NFData CreateResourceServerResponse where
