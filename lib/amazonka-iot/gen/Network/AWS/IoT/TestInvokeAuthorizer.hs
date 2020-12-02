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
-- Module      : Network.AWS.IoT.TestInvokeAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invoke the specified custom authorizer for testing purposes.
--
--
module Network.AWS.IoT.TestInvokeAuthorizer
    (
    -- * Creating a Request
      testInvokeAuthorizer
    , TestInvokeAuthorizer
    -- * Request Lenses
    , tiaAuthorizerName
    , tiaToken
    , tiaTokenSignature

    -- * Destructuring the Response
    , testInvokeAuthorizerResponse
    , TestInvokeAuthorizerResponse
    -- * Response Lenses
    , tiarsPolicyDocuments
    , tiarsPrincipalId
    , tiarsDisconnectAfterInSeconds
    , tiarsIsAuthenticated
    , tiarsRefreshAfterInSeconds
    , tiarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testInvokeAuthorizer' smart constructor.
data TestInvokeAuthorizer = TestInvokeAuthorizer'
  { _tiaAuthorizerName :: !Text
  , _tiaToken          :: !Text
  , _tiaTokenSignature :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestInvokeAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiaAuthorizerName' - The custom authorizer name.
--
-- * 'tiaToken' - The token returned by your custom authentication service.
--
-- * 'tiaTokenSignature' - The signature made with the token and your custom authentication service's private key.
testInvokeAuthorizer
    :: Text -- ^ 'tiaAuthorizerName'
    -> Text -- ^ 'tiaToken'
    -> Text -- ^ 'tiaTokenSignature'
    -> TestInvokeAuthorizer
testInvokeAuthorizer pAuthorizerName_ pToken_ pTokenSignature_ =
  TestInvokeAuthorizer'
    { _tiaAuthorizerName = pAuthorizerName_
    , _tiaToken = pToken_
    , _tiaTokenSignature = pTokenSignature_
    }


-- | The custom authorizer name.
tiaAuthorizerName :: Lens' TestInvokeAuthorizer Text
tiaAuthorizerName = lens _tiaAuthorizerName (\ s a -> s{_tiaAuthorizerName = a})

-- | The token returned by your custom authentication service.
tiaToken :: Lens' TestInvokeAuthorizer Text
tiaToken = lens _tiaToken (\ s a -> s{_tiaToken = a})

-- | The signature made with the token and your custom authentication service's private key.
tiaTokenSignature :: Lens' TestInvokeAuthorizer Text
tiaTokenSignature = lens _tiaTokenSignature (\ s a -> s{_tiaTokenSignature = a})

instance AWSRequest TestInvokeAuthorizer where
        type Rs TestInvokeAuthorizer =
             TestInvokeAuthorizerResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 TestInvokeAuthorizerResponse' <$>
                   (x .?> "policyDocuments" .!@ mempty) <*>
                     (x .?> "principalId")
                     <*> (x .?> "disconnectAfterInSeconds")
                     <*> (x .?> "isAuthenticated")
                     <*> (x .?> "refreshAfterInSeconds")
                     <*> (pure (fromEnum s)))

instance Hashable TestInvokeAuthorizer where

instance NFData TestInvokeAuthorizer where

instance ToHeaders TestInvokeAuthorizer where
        toHeaders = const mempty

instance ToJSON TestInvokeAuthorizer where
        toJSON TestInvokeAuthorizer'{..}
          = object
              (catMaybes
                 [Just ("token" .= _tiaToken),
                  Just ("tokenSignature" .= _tiaTokenSignature)])

instance ToPath TestInvokeAuthorizer where
        toPath TestInvokeAuthorizer'{..}
          = mconcat
              ["/authorizer/", toBS _tiaAuthorizerName, "/test"]

instance ToQuery TestInvokeAuthorizer where
        toQuery = const mempty

-- | /See:/ 'testInvokeAuthorizerResponse' smart constructor.
data TestInvokeAuthorizerResponse = TestInvokeAuthorizerResponse'
  { _tiarsPolicyDocuments          :: !(Maybe [Text])
  , _tiarsPrincipalId              :: !(Maybe Text)
  , _tiarsDisconnectAfterInSeconds :: !(Maybe Int)
  , _tiarsIsAuthenticated          :: !(Maybe Bool)
  , _tiarsRefreshAfterInSeconds    :: !(Maybe Int)
  , _tiarsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestInvokeAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiarsPolicyDocuments' - IAM policy documents.
--
-- * 'tiarsPrincipalId' - The principal ID.
--
-- * 'tiarsDisconnectAfterInSeconds' - The number of seconds after which the connection is terminated.
--
-- * 'tiarsIsAuthenticated' - True if the token is authenticated, otherwise false.
--
-- * 'tiarsRefreshAfterInSeconds' - The number of seconds after which the temporary credentials are refreshed.
--
-- * 'tiarsResponseStatus' - -- | The response status code.
testInvokeAuthorizerResponse
    :: Int -- ^ 'tiarsResponseStatus'
    -> TestInvokeAuthorizerResponse
testInvokeAuthorizerResponse pResponseStatus_ =
  TestInvokeAuthorizerResponse'
    { _tiarsPolicyDocuments = Nothing
    , _tiarsPrincipalId = Nothing
    , _tiarsDisconnectAfterInSeconds = Nothing
    , _tiarsIsAuthenticated = Nothing
    , _tiarsRefreshAfterInSeconds = Nothing
    , _tiarsResponseStatus = pResponseStatus_
    }


-- | IAM policy documents.
tiarsPolicyDocuments :: Lens' TestInvokeAuthorizerResponse [Text]
tiarsPolicyDocuments = lens _tiarsPolicyDocuments (\ s a -> s{_tiarsPolicyDocuments = a}) . _Default . _Coerce

-- | The principal ID.
tiarsPrincipalId :: Lens' TestInvokeAuthorizerResponse (Maybe Text)
tiarsPrincipalId = lens _tiarsPrincipalId (\ s a -> s{_tiarsPrincipalId = a})

-- | The number of seconds after which the connection is terminated.
tiarsDisconnectAfterInSeconds :: Lens' TestInvokeAuthorizerResponse (Maybe Int)
tiarsDisconnectAfterInSeconds = lens _tiarsDisconnectAfterInSeconds (\ s a -> s{_tiarsDisconnectAfterInSeconds = a})

-- | True if the token is authenticated, otherwise false.
tiarsIsAuthenticated :: Lens' TestInvokeAuthorizerResponse (Maybe Bool)
tiarsIsAuthenticated = lens _tiarsIsAuthenticated (\ s a -> s{_tiarsIsAuthenticated = a})

-- | The number of seconds after which the temporary credentials are refreshed.
tiarsRefreshAfterInSeconds :: Lens' TestInvokeAuthorizerResponse (Maybe Int)
tiarsRefreshAfterInSeconds = lens _tiarsRefreshAfterInSeconds (\ s a -> s{_tiarsRefreshAfterInSeconds = a})

-- | -- | The response status code.
tiarsResponseStatus :: Lens' TestInvokeAuthorizerResponse Int
tiarsResponseStatus = lens _tiarsResponseStatus (\ s a -> s{_tiarsResponseStatus = a})

instance NFData TestInvokeAuthorizerResponse where
