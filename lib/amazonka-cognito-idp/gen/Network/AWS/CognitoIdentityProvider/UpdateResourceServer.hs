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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateResourceServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and scopes of resource server. All other fields are read-only.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateResourceServer
    (
    -- * Creating a Request
      updateResourceServer
    , UpdateResourceServer
    -- * Request Lenses
    , ursScopes
    , ursUserPoolId
    , ursIdentifier
    , ursName

    -- * Destructuring the Response
    , updateResourceServerResponse
    , UpdateResourceServerResponse
    -- * Response Lenses
    , ursrsResponseStatus
    , ursrsResourceServer
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateResourceServer' smart constructor.
data UpdateResourceServer = UpdateResourceServer'
  { _ursScopes     :: !(Maybe [ResourceServerScopeType])
  , _ursUserPoolId :: !Text
  , _ursIdentifier :: !Text
  , _ursName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResourceServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursScopes' - The scope values to be set for the resource server.
--
-- * 'ursUserPoolId' - The user pool ID for the user pool.
--
-- * 'ursIdentifier' - The identifier for the resource server.
--
-- * 'ursName' - The name of the resource server.
updateResourceServer
    :: Text -- ^ 'ursUserPoolId'
    -> Text -- ^ 'ursIdentifier'
    -> Text -- ^ 'ursName'
    -> UpdateResourceServer
updateResourceServer pUserPoolId_ pIdentifier_ pName_ =
  UpdateResourceServer'
    { _ursScopes = Nothing
    , _ursUserPoolId = pUserPoolId_
    , _ursIdentifier = pIdentifier_
    , _ursName = pName_
    }


-- | The scope values to be set for the resource server.
ursScopes :: Lens' UpdateResourceServer [ResourceServerScopeType]
ursScopes = lens _ursScopes (\ s a -> s{_ursScopes = a}) . _Default . _Coerce

-- | The user pool ID for the user pool.
ursUserPoolId :: Lens' UpdateResourceServer Text
ursUserPoolId = lens _ursUserPoolId (\ s a -> s{_ursUserPoolId = a})

-- | The identifier for the resource server.
ursIdentifier :: Lens' UpdateResourceServer Text
ursIdentifier = lens _ursIdentifier (\ s a -> s{_ursIdentifier = a})

-- | The name of the resource server.
ursName :: Lens' UpdateResourceServer Text
ursName = lens _ursName (\ s a -> s{_ursName = a})

instance AWSRequest UpdateResourceServer where
        type Rs UpdateResourceServer =
             UpdateResourceServerResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateResourceServerResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ResourceServer"))

instance Hashable UpdateResourceServer where

instance NFData UpdateResourceServer where

instance ToHeaders UpdateResourceServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateResourceServer"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateResourceServer where
        toJSON UpdateResourceServer'{..}
          = object
              (catMaybes
                 [("Scopes" .=) <$> _ursScopes,
                  Just ("UserPoolId" .= _ursUserPoolId),
                  Just ("Identifier" .= _ursIdentifier),
                  Just ("Name" .= _ursName)])

instance ToPath UpdateResourceServer where
        toPath = const "/"

instance ToQuery UpdateResourceServer where
        toQuery = const mempty

-- | /See:/ 'updateResourceServerResponse' smart constructor.
data UpdateResourceServerResponse = UpdateResourceServerResponse'
  { _ursrsResponseStatus :: !Int
  , _ursrsResourceServer :: !ResourceServerType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResourceServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursrsResponseStatus' - -- | The response status code.
--
-- * 'ursrsResourceServer' - The resource server.
updateResourceServerResponse
    :: Int -- ^ 'ursrsResponseStatus'
    -> ResourceServerType -- ^ 'ursrsResourceServer'
    -> UpdateResourceServerResponse
updateResourceServerResponse pResponseStatus_ pResourceServer_ =
  UpdateResourceServerResponse'
    { _ursrsResponseStatus = pResponseStatus_
    , _ursrsResourceServer = pResourceServer_
    }


-- | -- | The response status code.
ursrsResponseStatus :: Lens' UpdateResourceServerResponse Int
ursrsResponseStatus = lens _ursrsResponseStatus (\ s a -> s{_ursrsResponseStatus = a})

-- | The resource server.
ursrsResourceServer :: Lens' UpdateResourceServerResponse ResourceServerType
ursrsResourceServer = lens _ursrsResourceServer (\ s a -> s{_ursrsResourceServer = a})

instance NFData UpdateResourceServerResponse where
